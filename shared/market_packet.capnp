@0x9a2c5b5f9a6f8c71;

enum AssetClass {
  crypto @0;
  forex @1;
  commodity @2;
  equity @3;
}

struct MarketPacket {
  id @0 :UInt64;
  symbol @1 :Text;
  timestamp @2 :Int64;
  bidPrice @3 :Float64;
  askPrice @4 :Float64;
  volume @5 :Float64;
  assetClass @6 :AssetClass;
}
