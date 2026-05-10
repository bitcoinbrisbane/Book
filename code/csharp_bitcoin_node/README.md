# Toy Bitcoin node — C#

A small, single-process console app that does a real handshake against a real mainnet peer, asks for the first batch of headers from the genesis block, validates each header's proof-of-work, downloads one block, verifies the merkle root, persists everything to SQLite, and stuffs the block's non-coinbase transactions into an in-memory mempool.

This accompanies *Chapter 3 — Architecture* and is not a production node. It exists to illustrate the receive → validate → persist → mempool pipeline in ~600 lines of C#.

## Run

Requires .NET 10 SDK and an outbound TCP connection (mainnet peers listen on port 8333).

```bash
cd code/csharp_bitcoin_node
dotnet run                  # default: download first 1000 blocks
dotnet run -- 10000         # or pass an explicit cap
dotnet run -- view 1        # pretty-print a stored block by height
dotnet run -- view <hash>   # …or by big-endian display hash
```

The cap is a safety net so you don't accidentally start a full-history sync. There is nothing stopping you from passing `2147483647` and walking the chain to tip — but at one block per round-trip it'll take a very long time, and recent blocks are ~1–2 MiB each so the SQLite file grows quickly.

Expected output:

```
[1] Discovering peers via DNS seeds…
    seed.bitcoin.sipa.be                25 addresses
    …

[2] Connecting to a peer…
    connected to 1.2.3.4:8333

[3] Handshaking…
    handshake complete

[4] Header sync (target: 1000 blocks)…
    headers: 1000
    1000 headers validated and stored

[5] Block download…
    500/1000   18 blocks/s   500 txs   137 KiB
    1000/1000  19 blocks/s   1000 txs  274 KiB

    1000/1000 blocks valid in 53.1s
    1000 transactions, 274 KiB persisted to node.db

Done.
```

A `node.db` SQLite file is written next to the binary with three tables: `headers`, `blocks`, `transactions`.

## What's wired up

| Layer (Chapter 3)        | File                       | Notes                                                            |
|--------------------------|----------------------------|------------------------------------------------------------------|
| Discovery                | `Discovery.cs`             | Resolves Bitcoin Core's DNS seed hostnames                       |
| Communications           | `Network.cs`               | TCP, magic + 24-byte header framing, version/verack handshake    |
| Persistence              | `Storage.cs`               | SQLite via `Microsoft.Data.Sqlite`                               |
| Validation               | `Chain.cs` (`Validator`)   | nBits → target, double-SHA-256 PoW, merkle root reconstruction   |
| Mempool                  | `Mempool.cs`               | Plain `List<Transaction>` — illustrative only                    |

## Simplifications (vs. a real node)

- One peer at a time. No connection pool, no eviction, no addrv2.
- Single-threaded, sequential block download — one `getdata`, await one `block`, repeat. A real node pipelines `getdata` across many peers in parallel.
- Locator is "just the latest hash". A real locator includes exponentially spaced ancestors so the peer can pick the right fork point on a reorg.
- Validation is PoW, chain continuity (prev hash), and merkle root. No script execution, no UTXO set, no consensus rules beyond the header.
- Witness/SegWit txs are parsed enough to compute the legacy txid for merkle. Witness data is skipped, not validated.
- The `Mempool` class exists to illustrate the layer but isn't used during IBD — confirmed transactions inside blocks are not "mempool". A real mempool fills from peer-gossiped `inv tx` / `tx` messages and applies policy filters (min relay fee, RBF, ancestor limits, etc.).

If you want a production-grade .NET implementation to compare against, look at [NBitcoin](https://github.com/MetacoSA/NBitcoin) (library) or [BTCPayServer](https://github.com/btcpayserver/btcpayserver) (which uses it).
