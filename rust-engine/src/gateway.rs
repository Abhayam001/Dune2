// rust-engine/src/gateway.rs

use std::sync::Arc;
use std::collections::HashMap;
use dashmap::DashMap;
use tokio::sync::Mutex;
use anyhow::{bail, Result};

use crate::feed_manager::MarketTick;

/// Represents an order request coming from the OCaml swarm brain.
#[derive(Debug, Clone)]
pub struct Order {
    pub id: u64,
    pub symbol: String,
    pub quantity: f64,          // Positive for buy, negative for sell
    pub price: f64,             // Target limit / market price
    pub timestamp: i64,
    pub notional: f64,          // |quantity| * price
    pub leverage: f64,
    pub asset_class: u8,
}

impl Order {
    pub fn new(id: u64, symbol: &str, quantity: f64, price: f64, timestamp: i64, leverage: f64, asset_class: u8) -> Self {
        let notional = quantity.abs() * price;
        Self {
            id,
            symbol: symbol.to_string(),
            quantity,
            price,
            timestamp,
            notional,
            leverage,
            asset_class,
        }
    }
}

/// Configuration for the pre-trade risk firewall.
#[derive(Debug, Clone)]
pub struct RiskConfig {
    pub max_notional_per_asset: f64,
    pub max_total_notional: f64,
    pub max_leverage: f64,
    pub max_drawdown_percent: f64,
    pub max_latency_ms: u64,
}

impl Default for RiskConfig {
    fn default() -> Self {
        Self {
            max_notional_per_asset: 50_000.0,
            max_total_notional: 100_000.0,
            max_leverage: 3.0,
            max_drawdown_percent: 5.0,
            max_latency_ms: 500,
        }
    }
}

/// Holds runtime risk state.
#[derive(Debug, Default)]
struct RiskState {
    per_asset: DashMap<u8, f64>,
    total_notional: Mutex<f64>,
    peak_equity: Mutex<f64>,
    latency_ms: DashMap<u8, u64>,
}

/// Real-time Risk Firewall checking limits before routing.
pub struct RiskFirewall {
    config: RiskConfig,
    state: Arc<RiskState>,
}

impl RiskFirewall {
    pub fn new(config: RiskConfig) -> Self {
        Self {
            config,
            state: Arc::new(RiskState::default()),
        }
    }

    pub async fn evaluate(&self, order: &Order) -> Result<()> {
        let asset_notional = order.notional.abs();
        let asset_key = order.asset_class;

        // 1. Per-asset limit
        let cur_asset_notional = self.state.per_asset.get(&asset_key).map(|v| *v).unwrap_or(0.0);
        if cur_asset_notional + asset_notional > self.config.max_notional_per_asset {
            bail!("Asset-class notional limit exceeded");
        }

        // 2. Global notional limit
        let mut total = self.state.total_notional.lock().await;
        if *total + asset_notional > self.config.max_total_notional {
            bail!("Global notional limit exceeded");
        }

        // 3. Leverage bound
        if order.leverage > self.config.max_leverage {
            bail!("Leverage exceeds allowed maximum");
        }

        // 4. Latency breach
        if let Some(lat) = self.state.latency_ms.get(&asset_key) {
            if *lat > self.config.max_latency_ms {
                bail!("Latency breach for asset class");
            }
        }

        // 5. Drawdown evaluation
        let mut peak = self.state.peak_equity.lock().await;
        let current_equity = *peak - asset_notional;
        if *peak > 0.0 {
            let drawdown = (*peak - current_equity) / *peak * 100.0;
            if drawdown > self.config.max_drawdown_percent {
                bail!("Drawdown limit breached");
            }
        }

        self.state.per_asset
            .entry(asset_key)
            .and_modify(|v| *v += asset_notional)
            .or_insert(asset_notional);
        *total += asset_notional;
        if current_equity > *peak {
            *peak = current_equity;
        }

        Ok(())
    }

    pub fn update_latency(&self, asset_class: u8, latency_ms: u64) {
        self.state.latency_ms.insert(asset_class, latency_ms);
    }
}

/// Simulated Paper Broker with virtual balance, realistic fee model, and live P&L tracking.
#[derive(Debug)]
pub struct PaperBroker {
    pub virtual_cash: f64,
    pub initial_capital: f64,
    pub positions: HashMap<String, f64>,
    pub realized_pnl: f64,
    pub trade_count: u64,
    pub fee_rate: f64, // e.g. 0.0004 = 0.04% taker fee
}

impl PaperBroker {
    pub fn new(initial_capital: f64) -> Self {
        Self {
            virtual_cash: initial_capital,
            initial_capital,
            positions: HashMap::new(),
            realized_pnl: 0.0,
            trade_count: 0,
            fee_rate: 0.0004,
        }
    }

    /// Simulates order fill at current live market tick with slippage and exchange fees.
    pub fn execute_paper_order(&mut self, order: &Order, tick: &MarketTick) -> Result<()> {
        let is_buy = order.quantity > 0.0;
        let fill_price = if is_buy { tick.ask_price } else { tick.bid_price };
        let execution_cost = order.quantity.abs() * fill_price;
        let fee = execution_cost * self.fee_rate;

        if is_buy && (self.virtual_cash < execution_cost + fee) {
            bail!("Insufficient paper capital for buy order");
        }

        if is_buy {
            self.virtual_cash -= execution_cost + fee;
            let pos = self.positions.entry(order.symbol.clone()).or_insert(0.0);
            *pos += order.quantity;
        } else {
            let pos = self.positions.entry(order.symbol.clone()).or_insert(0.0);
            if *pos < order.quantity.abs() {
                bail!("Attempted to sell more than currently held in paper portfolio");
            }
            *pos -= order.quantity.abs();
            self.virtual_cash += execution_cost - fee;
        }

        self.trade_count += 1;
        println!(
            "[PAPER EXECUTION #{}] Symbol: {} | Side: {} | Qty: {:.4} | Price: ${:.2} | Fee: ${:.2} | Cash: ${:.2}",
            self.trade_count,
            order.symbol,
            if is_buy { "BUY" } else { "SELL" },
            order.quantity.abs(),
            fill_price,
            fee,
            self.virtual_cash
        );

        Ok(())
    }

    /// Returns the net portfolio liquidation value and total P&L.
    pub fn calculate_equity(&self, current_prices: &HashMap<String, f64>) -> (f64, f64) {
        let mut portfolio_value = self.virtual_cash;
        for (symbol, qty) in &self.positions {
            if let Some(price) = current_prices.get(symbol) {
                portfolio_value += qty * price;
            }
        }
        let total_pnl = portfolio_value - self.initial_capital;
        (portfolio_value, total_pnl)
    }
}

pub mod prelude {
    pub use super::{RiskFirewall, RiskConfig, Order, PaperBroker};
}
