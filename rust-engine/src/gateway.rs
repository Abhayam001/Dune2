// rust-engine/src/gateway.rs

use std::sync::Arc;
use dashmap::DashMap;
use tokio::sync::Mutex;
use anyhow::Result;

/// Represents a simple order request coming from the OCaml brain.
#[derive(Debug, Clone)]
pub struct Order {
    pub id: u64,
    pub symbol: String,
    pub quantity: f64,          // Positive for buy, negative for sell
    pub price: f64,             // Limit price
    pub timestamp: i64,
    pub notional: f64,          // quantity * price
    pub leverage: f64,
    pub asset_class: u8,
}

/// Configuration for risk limits.
#[derive(Debug, Clone)]
pub struct RiskConfig {
    pub max_notional_per_asset: f64,
    pub max_total_notional: f64,
    pub max_leverage: f64,
    pub max_drawdown_percent: f64,
    pub max_latency_ms: u64,
}

/// Holds runtime risk state, updated atomically.
#[derive(Debug, Default)]
struct RiskState {
    // Accumulated notional per asset class (key = asset class enum value)
    per_asset: DashMap<u8, f64>,
    // Total notional across all assets
    total_notional: Mutex<f64>,
    // Peak equity to compute drawdown
    peak_equity: Mutex<f64>,
    // Latest observed latency (ms) per asset
    latency_ms: DashMap<u8, u64>,
}

/// The real‑time firewall that validates orders against configured limits.
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

    /// Checks an order; returns Ok(()) if it passes, otherwise Err with reason.
    pub async fn evaluate(&self, order: &Order) -> Result<()> {
        // 1️⃣ Notional limits per asset.
        let asset_notional = order.notional.abs();
        let asset_key = order.asset_class;
        let cur_asset_notional = self.state.per_asset.get(&asset_key).map(|v| *v).unwrap_or(0.0);
        if cur_asset_notional + asset_notional > self.config.max_notional_per_asset {
            anyhow::bail!("Asset‑class notional limit exceeded");
        }

        // 2️⃣ Global notional.
        let mut total = self.state.total_notional.lock().await;
        if *total + asset_notional > self.config.max_total_notional {
            anyhow::bail!("Global notional limit exceeded");
        }

        // 3️⃣ Leverage bound.
        if order.leverage > self.config.max_leverage {
            anyhow::bail!("Leverage exceeds allowed maximum");
        }

        // 4️⃣ Latency check – assumed to be supplied by external monitor.
        if let Some(lat) = self.state.latency_ms.get(&asset_key) {
            if *lat > self.config.max_latency_ms {
                anyhow::bail!("Latency breach for asset class");
            }
        }

        // 5️⃣ Drawdown – simplistic: assume current equity = peak - notional.
        // In a real system this would reference P&L stream.
        let mut peak = self.state.peak_equity.lock().await;
        let current_equity = *peak - asset_notional; // dummy figure
        if *peak > 0.0 {
            let drawdown = ( *peak - current_equity) / *peak * 100.0;
            if drawdown > self.config.max_drawdown_percent {
                anyhow::bail!("Drawdown limit breached");
            }
        }
        // If we reach here, order passes – update state.
        self.state.per_asset
            .entry(asset_key)
            .and_modify(|v| *v += asset_notional)
            .or_insert(asset_notional);
        *total += asset_notional;
        // Update peak equity if equity exceeds it.
        if current_equity > *peak {
            *peak = current_equity;
        }
        Ok(())
    }

    /// External hook to update observed latency per asset class.
    pub fn update_latency(&self, asset_class: u8, latency_ms: u64) {
        self.state.latency_ms.insert(asset_class, latency_ms);
    }
}

// Helper to compute notional from order fields – useful for callers.
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

pub mod prelude {
    pub use super::{RiskFirewall, RiskConfig, Order};
}
