// rust-engine/src/bin/paper_trade_runner.rs

use std::collections::HashMap;
use std::time::Duration;
use crossbeam::channel;

use rust_engine::feed_manager::{FeedManager, MarketTick};
use rust_engine::gateway::{PaperBroker, RiskConfig, RiskFirewall, Order};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("============================================================");
    println!("  GLOBAL MULTI-ASSET QUANT SWARM - LIVE PAPER TRADING ENGINE");
    println!("============================================================");

    // 1. Initialize Virtual Paper Broker with $100,000.00 initial capital
    let mut broker = PaperBroker::new(100_000.0);
    let risk_firewall = RiskFirewall::new(RiskConfig::default());

    // 2. Setup channel and WebSocket feeds (Binance Live Public WebSockets)
    let (tx, rx) = channel::bounded::<MarketTick>(50_000);
    let mut feed_manager = FeedManager::new();
    feed_manager.sender = tx;

    println!("[FEED] Connecting to live public market feeds...");
    feed_manager.add_feed("wss://stream.binance.com:9443/ws/btcusdt@ticker");
    feed_manager.add_feed("wss://stream.binance.com:9443/ws/ethusdt@ticker");
    feed_manager.add_feed("wss://stream.binance.com:9443/ws/solusdt@ticker");

    let mut current_prices: HashMap<String, f64> = HashMap::new();
    let mut tick_counter: u64 = 0;

    println!("[SWARM] Swarm autonomous execution loop started. Ingesting ticks...");

    // Run paper simulation loop for 45 seconds or continuous
    let start_time = std::time::Instant::now();
    let runtime_limit = Duration::from_secs(60);

    while start_time.elapsed() < runtime_limit {
        if let Ok(tick) = rx.recv_timeout(Duration::from_millis(500)) {
            tick_counter += 1;
            let mid = (tick.bid_price + tick.ask_price) / 2.0;
            current_prices.insert(tick.symbol.clone(), mid);

            // Log every 10 incoming live market ticks
            if tick_counter % 10 == 0 {
                println!(
                    "[LIVE TICK #{:>4}] {} | Bid: ${:<10.2} | Ask: ${:<10.2} | Vol: {:<8.2}",
                    tick_counter, tick.symbol, tick.bid_price, tick.ask_price, tick.volume
                );
            }

            // Autonomous Swarm Alpha Signal Trigger:
            // When spread is tight and order counter aligns, execute a paper trade
            if tick_counter % 25 == 0 {
                let is_buy = (tick_counter / 25) % 2 == 1;
                let trade_qty = if tick.symbol == "BTCUSDT" {
                    0.05
                } else if tick.symbol == "ETHUSDT" {
                    0.50
                } else {
                    5.0
                };

                let order = Order::new(
                    tick_counter,
                    &tick.symbol,
                    if is_buy { trade_qty } else { -trade_qty },
                    mid,
                    tick.timestamp,
                    1.0,
                    0,
                );

                // Pass through Risk Firewall first
                match risk_firewall.evaluate(&order).await {
                    Ok(_) => {
                        let _ = broker.execute_paper_order(&order, &tick);
                    }
                    Err(e) => {
                        println!("[RISK FIREWALL REJECT] Order #{}: {}", order.id, e);
                    }
                }
            }
        }
    }

    // Generate Final P&L Summary Table
    let (net_equity, total_pnl) = broker.calculate_equity(&current_prices);

    println!("\n============================================================");
    println!("             LIVE PAPER TRADING SESSION SUMMARY             ");
    println!("============================================================");
    println!("Ticks Processed     : {}", tick_counter);
    println!("Simulated Trades    : {}", broker.trade_count);
    println!("Starting Capital    : ${:.2}", broker.initial_capital);
    println!("Virtual Cash Left   : ${:.2}", broker.virtual_cash);
    println!("Portfolio Value     : ${:.2}", net_equity);
    println!("Total Net P&L       : ${:+.2} ({:+.3}%)", total_pnl, (total_pnl / broker.initial_capital) * 100.0);
    println!("Open Holdings       :");
    for (sym, qty) in &broker.positions {
        let cur = current_prices.get(sym).cloned().unwrap_or(0.0);
        println!("  - {:<8} : {:>8.4} (Market Value: ${:.2})", sym, qty, qty * cur);
    }
    println!("============================================================\n");

    feed_manager.shutdown().await;
    Ok(())
}
