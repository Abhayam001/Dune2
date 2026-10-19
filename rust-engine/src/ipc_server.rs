// rust-engine/src/ipc_server.rs

use std::path::Path;
use std::os::unix::net::UnixListener;
use std::io::Write;
use crossbeam::channel::Receiver;
use capnp::message::Builder;
use capnp::serialize::write_message;
use anyhow::Result;

use super::feed_manager::MarketTick;
use crate::market_packet_capnp::market_packet;

/// IPC server that streams serialized `MarketPacket`s over a Unix domain socket.
/// The OCaml brain connects as a client and reads a length‑prefixed stream of
/// Cap’n‑Proto messages.
pub struct IpcServer {
    socket_path: String,
    rx: Receiver<MarketTick>,
}

impl IpcServer {
    pub fn new(socket_path: &str, rx: Receiver<MarketTick>) -> Self {
        Self {
            socket_path: socket_path.to_string(),
            rx,
        }
    }

    /// Runs the server – blocks the current thread. In production this would be
    /// spawned onto a dedicated runtime thread.
    pub fn run(&self) -> Result<()> {
        // Ensure any stale socket file is removed.
        let path = Path::new(&self.socket_path);
        if path.exists() {
            std::fs::remove_file(path)?;
        }
        let listener = UnixListener::bind(&self.socket_path)?;
        println!("IPC server listening on {}", self.socket_path);
        // Accept a single client for simplicity – the OCaml side reconnects if needed.
        let (mut stream, _addr) = listener.accept()?;
        loop {
            // Receive a tick (blocking). In a real system you would use select/poll.
            let tick = match self.rx.recv() {
                Ok(t) => t,
                Err(_) => break, // Channel closed – shutdown.
            };
            // Convert to Cap’n‑Proto message.
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
            // Serialize with length prefix (4‑byte little‑endian size).
            let mut buf = Vec::new();
            write_message(&mut buf, &message)?;
            let len = (buf.len() as u32).to_le_bytes();
            stream.write_all(&len)?;
            stream.write_all(&buf)?;
            stream.flush()?;
        }
        Ok(())
    }
}

pub mod prelude {
    pub use super::IpcServer;
}
