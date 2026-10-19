// rust-engine/build.rs

fn main() {
    // Compile the Cap'n Proto schema into Rust code.
    capnpc::CompilerCommand::new()
        .src_prefix("../shared")
        .file("../shared/market_packet.capnp")
        .run()
        .expect("Cap'n Proto compilation failed");
}
