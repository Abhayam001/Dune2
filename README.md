# Global Multi-Asset Autonomous Quant Swarm

A production‑grade, polyglot quant platform comprising:

- **Rust Engine** – high‑throughput market data ingestion, risk firewall, and IPC bridge.
- **OCaml Brain** – multicore swarm of 1 000 autonomous bots with a safety‑council of 5 meta‑watchers.

The repository follows a monorepo layout:

```
global_quant_swarm/
├─ .gitignore
├─ README.md
├─ Cargo.toml                # workspace manifest
├─ rust-engine/
│   ├─ Cargo.toml            # rust crate
│   └─ src/…
└─ ocaml-brain/
    ├─ dune-project
    ├─ dune
    └─ src/…
```

All code is ready to be built and pushed to GitHub.
