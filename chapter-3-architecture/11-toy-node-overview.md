## Building a toy node — overview

Theory only takes you so far. We've spent the last several pages talking about layers, transaction flow, validation and the CAP-style trade-offs distributed systems force on us. The next few pages turn that theory into running code: a small C# console app, [`code/csharp_bitcoin_node`](../code/csharp_bitcoin_node/), that connects to real Bitcoin mainnet peers, performs an Initial Block Download from genesis, validates each block, and persists everything to SQLite.

It's a *toy* under a thousand lines of C# total, but it does exercise every layer we just covered.

## Project layout

| File              | Layer                | Role                                                                 |
| ----------------- | -------------------- | -------------------------------------------------------------------- |
| `Discovery.cs`    | Discovery            | Resolves Bitcoin Core's DNS seed hostnames into peer endpoints       |
| `Network.cs`      | Communications       | TCP, message framing, varint, version/verack handshake               |
| `PeerSession.cs`  | Communications       | Owns the active peer; falls over to the next candidate on disconnect |
| `Storage.cs`      | Persistence          | `IBlockStore` interface + `SqliteBlockStore` implementation          |
| `Chain.cs`        | Validation           | `BlockHeader`, `Transaction`, `Block`, `Validator` (PoW + merkle)    |
| `Mempool.cs`      | (illustrative)       | A `List<Transaction>` wrapper to the mempool layer's simplest form   |
| `Crypto.cs`       | (foundational)       | Double SHA-256                                                       |
| `Program.cs`      | Orchestration        | `Main`, header sync loop, block download loop, console UI            |

Each of the next four pages walks through one of these layers in detail.

## How to run

```bash
cd code/csharp_bitcoin_node
dotnet run                # default: download first 1000 blocks
dotnet run -- 5000        # or pass an explicit cap
```

You should see DNS resolution, a peer handshake, header sync in batches of 2000, then a live status line ticking through block downloads with hash, tx count, size, rate, and ETA. A SQLite file `node.db` is written next to the binary.

## What we deliberately don't do

A real node is enormously larger than this. Bitcoin Core is ~150,000 lines of C++. The simplifications that matter for a reader are:

- **One peer at a time, sequential `getdata`.** Real nodes pipeline block requests across many peers in parallel.
- **No script execution, no UTXO set.** Validation is PoW + chain continuity + merkle root. Real consensus rules require a UTXO database, signature verification on every input, and a long list of soft-fork rules.
- **No reorg handling.** We trust the chain we're walking. Real nodes maintain multiple candidate chains and switch on cumulative work.
- **Mempool is illustrative only.** We don't accept gossiped `inv tx` / `tx` messages, don't apply policy filters, and don't implement RBF.

These omissions are spelled out again in the code's `README.md` and at the end of each layer page. They don't compromise the pedagogical goal. Every layer we discussed has a working, observable counterpart in this app.

## Reading order

The next pages mirror the layer pages from earlier in this chapter:

1. [Toy node — Discovery](./12-toy-node-discovery.md)
2. [Toy node — Communications](./13-toy-node-communications.md)
3. [Toy node — Persistence](./14-toy-node-persistence.md)
4. [Toy node — Validation & IBD](./15-toy-node-validation-ibd.md)

Each is self-contained and shows the actual code as it lives in the repo. If you want to follow along, keep `code/csharp_bitcoin_node/` open in your editor.
