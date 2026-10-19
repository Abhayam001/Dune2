// rust-engine/src/feed_manager.rs

use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;
use tokio::task::JoinHandle;
use tokio::time;
use tokio_tungstenite::{connect_async, tungstenite::protocol::Message};
use futures_util::{StreamExt, SinkExt};
use crossbeam::channel::Sender;

use crate::market_packet_capnp::{market_packet, AssetClass};
use capnp::message::Builder;

/// Represents a normalized market tick to be sent across the system.
#[derive(Debug, Clone)]
pub struct MarketTick {
    pub id: u64,
    pub symbol: String,
    pub timestamp: i64,
    pub bid_price: f64,
    pub ask_price: f64,
    pub volume: f64,
    pub asset_class: AssetClass,
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
    let (write, mut read) = ws_stream.split();
    let write = Arc::new(Mutex::new(write));

    // Ping keep‑alive loop with shared thread-safe writer
    let write_ping = Arc::clone(&write);
    let ping_handle: JoinHandle<()> = tokio::spawn(async move {
        let mut interval = time::interval(Duration::from_secs(30));
        loop {
            interval.tick().await;
            let mut lock = write_ping.lock().await;
            if lock.send(Message::Ping(vec![].into())).await.is_err() {
                break;
            }
        }
    });

    while let Some(msg) = read.next().await {
        match msg? {
            Message::Text(txt) => {
                if let Ok(tick) = serde_json::from_str::<serde_json::Value>(&txt) {
                    let market_tick = MarketTick {
                        id: tick["id"].as_u64().unwrap_or_default(),
                        symbol: tick["symbol"].as_str().unwrap_or_default().to_string(),
                        timestamp: tick["ts"].as_i64().unwrap_or_default(),
                        bid_price: tick["bid"].as_f64().unwrap_or_default(),
                        ask_price: tick["ask"].as_f64().unwrap_or_default(),
                        volume: tick["vol"].as_f64().unwrap_or_default(),
                        asset_class: match tick["class"].as_str().unwrap_or("CRYPTO") {
                            "CRYPTO" => AssetClass::Crypto,
                            "FOREX" => AssetClass::Forex,
                            "COMMODITY" => AssetClass::Commodity,
                            "EQUITY" => AssetClass::Equity,
                            _ => AssetClass::Crypto,
                        },
                    };
                    let _ = tx.send(market_tick);
                }
            }
            Message::Binary(_) => { /* ignore binary frames */ }
            Message::Ping(p) => {
                let mut lock = write.lock().await;
                let _ = lock.send(Message::Pong(p)).await;
            }
            Message::Close(_) => break,
            _ => {}
        }
    }
    ping_handle.abort();
    Ok(())
}

/// Manages multiple feed runners.
pub struct FeedManager {
    handles: Vec<JoinHandle<anyhow::Result<()>>>,
    pub sender: Sender<MarketTick>,
}

impl FeedManager {
    pub fn new() -> Self {
        let (tx, _rx) = crossbeam::channel::bounded::<MarketTick>(100_000);
        FeedManager { handles: Vec::new(), sender: tx }
    }

    pub fn add_feed(&mut self, url: &str) {
        let tx = self.sender.clone();
        let url_string = url.to_string();
        let handle = tokio::spawn(async move { run_websocket_feed(url_string, tx).await });
        self.handles.push(handle);
    }

    pub async fn shutdown(self) {
        for handle in self.handles {
            let _ = handle.await;
        }
    }
}

impl Default for FeedManager {
    fn default() -> Self {
        Self::new()
    }
}

pub mod prelude {
    pub use super::{FeedManager, MarketTick};
}
