## Mempool layer

When a transaction is broadcast, it spends some time *in flight*, signed by the user but not yet confirmed in a block. That window is seconds, minutes, or in a fee-market spike, hours. The mempool is where each node keeps the unconfirmed transactions it has seen. It's not consensus state — peers can and do hold different mempool views — but it is critical infrastructure: it's how miners pick the next block, how wallets show "pending" balances, and how fee estimation actually works.

We mentioned the mempool in earlier layer pages and again in the transaction-flow walkthroughs for Bitcoin and Ethereum. This page makes it its own layer, because the interesting design question isn't *that* a node stores unconfirmed transactions it's *which* ones it agrees to store.

## Policy vs consensus

The single most important idea about a mempool:

- **Consensus rules** are what a block must satisfy to be valid. Every node in the network agrees on these. Violate one and your block is rejected forever.
- **Policy rules** are what a transaction must satisfy to be *relayed* by *this particular node*. They are local; different nodes can run different policies; they're configurable per implementation.

Relay is cheap (every node forwards every accepted tx). Blocks are expensive (every node validates every block forever). Mempools filter aggressively so that *most* of what gets broadcast eventually gets mined — without flooding the network with traffic that no miner would touch anyway.

A concrete example: Bitcoin's consensus accepts any output above zero satoshis. Bitcoin Core's *policy*, in [`src/policy/policy.cpp`](https://github.com/bitcoin/bitcoin/blob/master/src/policy/policy.cpp), rejects outputs below the *dust threshold* (~546 sat for P2PKH). A 100-sat output is *valid* — but no Bitcoin Core node will relay it. The transaction can still be mined directly. That gap, between "valid" and "relayed", is policy.

## What every mempool enforces

Across Bitcoin and Ethereum implementations, the common rules look like this:

- **Minimum relay fee** — txs below a configurable rate (often 1 sat/vB on Bitcoin, 1 gwei priority fee on Ethereum) are rejected outright.
- **Transaction size limits** — non-standard giant txs are filtered even if technically valid.
- **Standardness** — Bitcoin requires `scriptSig` and `scriptPubKey` to match a short list of "standard" forms. Non-standard scripts can be mined directly but aren't relayed.
- **Replace-by-fee** ([BIP125](https://github.com/bitcoin/bips/blob/master/bip-0125.mediawiki)) — a replacement must pay *more total* fees than the tx it's evicting, not just a higher rate, and meet a few other rules.
- **Ancestor / descendant limits** — Bitcoin Core caps a single tx at 25 unconfirmed ancestors, bounding the chain reaction of fee-bumping a parent (which is why CPFP has a 25-deep cap).
- **Per-sender pending caps** — Ethereum mempools cap the number of pending txs from one sender, since each is keyed by nonce.

Every one of these is *policy*. Every implementation tunes them differently. A miner running custom software accepts whatever they want — that's how out-of-band transactions get mined despite never appearing in any public node's mempool.

## Eviction and the fee market

A mempool is bounded memory; it has to evict. Bitcoin Core's `CTxMemPool` defaults to 300 MB and evicts the lowest-feerate packages first when full. Geth and Nethermind each cap their tx pool in transactions, not bytes, and evict by tip.

This is why fees actually matter. A transaction that *would* fit in the next block at 10 sat/vB today might be *evicted from the mempool* tomorrow if a traffic spike pushed minimum feerates to 50 sat/vB. Once evicted, it's gone — the wallet has to rebroadcast (typically with a higher fee) to get back in front of miners.

The eviction order also feeds **fee estimation**. Calls like `estimatesmartfee` or `eth_gasPrice` are implemented by reading the current mempool's feerate distribution and recent block feerates. The mempool is the network's continuous market for next-block inclusion — and fee estimation is the public-facing read of that market.

## RBF, CPFP, and package relay

Once a fee market exists, you need ways to update fees on stuck transactions:

- **RBF (BIP125)** — the sender bumps fee on their own tx; the new tx replaces the old in mempool.
- **CPFP** — anyone (often the receiver) creates a child tx that spends the stuck parent and pays a higher fee. The mempool can mine parent + child together based on *combined* feerate.
- **Package relay** (newer — ~2024) — lets several related txs be evaluated as a unit during *relay*, not just at mining time. This closes a long-standing gap where a low-fee parent prevented a fee-bumping child from propagating at all.

Each is a small protocol modification that lives entirely in the mempool / policy layer. None of them touch consensus. That separation is what lets the fee market evolve faster than the chain rules.

## In code

Real implementations, by line count, dwarf every other layer in their respective projects:

- **Bitcoin Core**: `src/txmempool.cpp` (`CTxMemPool`), `src/validation.cpp` (`MemPoolAccept`), `src/policy/policy.cpp` for every standardness rule.
- **Geth**: `core/txpool/legacypool/legacypool.go` for ordinary txs, `core/txpool/blobpool/` for EIP-4844 blobs.
- **Nethermind**: `src/Nethermind/Nethermind.TxPool/TxPool.cs` and `src/Nethermind/Nethermind.TxPool/Filters/` (one filter class per rule — a remarkably readable structure).

In our toy node, [`Mempool.cs`](../code/csharp_bitcoin_node/Mempool.cs) is deliberately the opposite extreme — a `List<Transaction>` with no rules at all. We don't accept gossiped `inv tx` / `tx` messages, don't filter by fee, don't evict. It exists in the project to make the layer *visible* (the file is there; the file-to-layer table in the toy-node overview maps it explicitly) but its real job is to point at the gap between "toy" and "production":

> Every other layer in our toy node has a working counterpart in real implementations. The mempool's counterpart is, by line count, the largest of them.

That's the page in one sentence. Storage, framing, validation, discovery — each was one screen of code in our toy. A mempool, done properly, is thousands. That's the cost of taking *policy* seriously.
