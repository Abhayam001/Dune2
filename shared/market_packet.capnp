// market_packet.capnp
@0x9a2c5b5f9a6f8c71;

enum AssetClass {
  CRYPTO = 0;
  FOREX = 1;
  COMMODITY = 2;
  EQUITY = 3;
}

struct MarketPacket {
  id @0 :UInt64;                     // Unique identifier for the tick
  symbol @1 :Text;                    // Ticker symbol, e.g., "BTCUSD"
  timestamp @2 :Int64;                // Unix epoch milliseconds
  bidPrice @3 :Float64;               // Best bid price
  askPrice @4 :Float64;               // Best ask price
  volume @5 :Float64;                 // Trade volume in base units
  assetClass @6 :AssetClass;          // Asset class classification
}
