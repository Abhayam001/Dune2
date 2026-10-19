pub mod feed_manager;
pub mod gateway;
pub mod ipc_server;

pub mod market_packet_capnp {
    include!(concat!(env!("OUT_DIR"), "/market_packet_capnp.rs"));
}
