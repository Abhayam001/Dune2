// rust-engine/src/feed_manager.rs

use std::sync::Arc;
use std::time::Duration;
use tokio::task::JoinHandle;
use tokio::time;
use tokio_tungstenite::{connect_async, tungstenite::protocol::Message};
use futures_util::{StreamExt, SinkExt};
use crossbeam::channel::Sender;

use crate::market_packet_capnp::market_packet;
use capnp::message::Builder;
use capnp::serialize::write_message;

/// Represents a normalized market tick to be sent across the system.
#[derive(Debug, Clone)]
pub struct MarketTick {
    pub id: u64,
    pub symbol: String,
    pub timestamp: i64,
    pub bid_price: f64,
    pub ask_price: f64,
    pub volume: f64,
    pub asset_class: market_packet::AssetClass,
}

impl From<MarketTick> for Builder<capnp::message::HeapAllocator> {
    fn from(tick: MarketTick) -> Self {
        let mut message = Builder::new_default();
        {
            let mut packet = message.init_root::<market_packet::Builder>();
            packet.set_id(tick.id);
            packet.set_symbol(&tick.symbol);
            packet.set_timestamp(tick.timestamp);
            packet.set_bid_price(tick.bid_price);
            packet.set_ask_price(tick.ask_price);
            packet.set_volume(tick.volume);
            packet.set_asset_class(tick.asset_class);
        }
        message
    }
}

/// Asynchronous feed runner for a single WebSocket endpoint.
async fn run_websocket_feed(url: String, tx: Sender<MarketTick>) -> anyhow::Result<()> {
    let (ws_stream, _) = connect_async(&url).await?;
    let (mut write, mut read) = ws_stream.split();
    // Ping keep‑alive loop
    let ping_handle: JoinHandle<()> = tokio::spawn(async move {
        let mut interval = time::interval(Duration::from_secs(30));
        loop {
            interval.tick().await;
            if write.send(Message::Ping(vec![])).await.is_err() {
                break;
            }
        }
    });

    while let Some(msg) = read.next().await {
        match msg? {
            Message::Text(txt) => {
                // Parse incoming JSON tick. Real implementation would be feed‑specific.
                if let Ok(tick) = serde_json::from_str::<serde_json::Value>(&txt) {
                    // Extract fields with graceful fallback.
                    let market_tick = MarketTick {
                        id: tick["id"].as_u64().unwrap_or_default(),
                        symbol: tick["symbol"].as_str().unwrap_or_default().to_string(),
                        timestamp: tick["ts"].as_i64().unwrap_or_default(),
                        bid_price: tick["bid"].as_f64().unwrap_or_default(),
                        ask_price: tick["ask"].as_f64().unwrap_or_default(),
                        volume: tick["vol"].as_f64().unwrap_or_default(),
                        asset_class: match tick["class"].as_str().unwrap_or("CRYPTO") {
                            "CRYPTO" => market_packet::AssetClass::Crypto,
                            "FOREX" => market_packet::AssetClass::Forex,
                            "COMMODITY" => market_packet::AssetClass::Commodity,
                            "EQUITY" => market_packet::AssetClass::Equity,
                            _ => market_packet::AssetClass::Crypto,
                        },
                    };
                    // Zero‑copy send via crossbeam channel (supports lock‑free multi‑producer).
                    let _ = tx.send(market_tick);
                }
            }
            Message::Binary(_) => { /* ignore binary frames */ }
            Message::Ping(p) => {
                let _ = write.send(Message::Pong(p)).await;
            }
            Message::Close(_) => break,
            _ => {}
        }
    }
    // Ensure ping task stops.
    ping_handle.abort();
    Ok(())
}

/// Manages multiple feed runners.
pub struct FeedManager {
    /// Vector of join handles for graceful shutdown.
    handles: Vec<JoinHandle<anyhow::Result<()>>>,
    /// Shared sender for market ticks.
    pub sender: Sender<MarketTick>,
}

impl FeedManager {
    /// Creates a new manager with a bounded channel (capacity = 100_000).
    pub fn new() -> Self {
        // Crossbeam bounded channel provides lock‑free MPMC semantics.
        let (tx, _rx) = crossbeam::channel::bounded::<MarketTick>(100_000);
        FeedManager { handles: Vec::new(), sender: tx }
    }

    /// Spawns a feed task for each WebSocket URL.
    pub fn add_feed(&mut self, url: &str) {
        let tx = self.sender.clone();
        let url_string = url.to_string();
        let handle = tokio::spawn(async move { run_websocket_feed(url_string, tx).await });
        self.handles.push(handle);
    }

    /// Await termination of all feeds (e.g., during shutdown).
    pub async fn shutdown(self) {
        for handle in self.handles {
            let _ = handle.await;
        }
    }
}

// Public re‑export for downstream crates.
pub mod prelude {
    pub use super::{FeedManager, MarketTick};
}
