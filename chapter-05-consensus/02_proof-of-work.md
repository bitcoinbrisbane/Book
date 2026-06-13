## Proof of Work

The 2009 [Bitcoin whitepaper](https://bitcoin.org/bitcoin.pdf), written by Satoshi Nakamoto, introduces **Proof of Work** (PoW) as the core consensus mechanism of the network. Under PoW, network participants — known as **miners** — race to find a value whose hash, when fed into a function, is *less than* an arbitrary target value. The whitepaper calls this the **target difficulty**.

The name comes from the analogy of gold miners prospecting for gold. As we saw in [the chapter on hash functions](../chapter-03-crypto-cryptography/03_hash-functions.md), a cryptographic hash output is effectively random for the user — there is no shortcut, no pattern, no way to predict in advance which input will produce a hash below the target.

So PoW is, fundamentally, a *probabilistic* search. If we think of hash attempts like rolling a die: from statistics class we know that rolling a six takes, on average, six rolls. If we equate "a roll" with "a unit of work", a miner who has found a valid hash has used, on average, `1 / (target / 2²⁵⁶)` units of work. The chain *measures* effort statistically — no individual miner can shortcut the average.

```
P(X = x) — the probability mass function
p_X : ℝ → [0, 1]
```

## Hard to compute, easy to verify

The second defining property of a PoW algorithm is *asymmetry*: it must be **hard to compute** but **easy to verify**.

Sudoku is a familiar analogy. A nine-by-nine grid takes most of us minutes — sometimes a lot more — to solve, but if someone hands you a completed puzzle, you can run your eyes down the rows and columns and verify it in seconds. Bitcoin PoW has the same asymmetry: finding a valid hash takes a network's worth of GPUs and ASICs ten minutes, but anyone with a laptop can verify the result in microseconds by running the hash once and comparing to the target.

## How it works on the network

Every miner — forgetting mining pools for the moment — works independently, trying to solve the *same* puzzle in parallel. The first miner to find a hash below the target broadcasts it to the network as the next block. The network accepts that solution and starts building the next block on top of it, with the just-found one as its `prevBlock`.

No one can *force* the other miners to build on that block. But it's a race, and it is in every miner's interest to do so — otherwise they may be mining on a chain that will be **orphaned**. The economic incentive aligns the network on a single canonical chain without any central coordinator.

## Why it works

Since each block takes a significant amount of energy and resources, once a miner solves the puzzle they get the right to add a new block of transactions to the blockchain. The block is broadcast, other nodes verify the solution (cheaply), and the chain extends.

This mechanism does three things at once:

- **Secures the network from rewriting attacks.** To change a confirmed block, an attacker has to redo *all* the proof-of-work from that block forward, and outpace the honest network while doing it.
- **Resists Sybil attacks.** Creating a thousand identities doesn't help if you have only one computer's worth of hash power. Influence is proportional to work, not to identities.
- **Incentivises participation.** The miner who found the block receives a **block reward** (newly minted Bitcoin) plus all the transaction fees from the block. That payout is what convinces them to keep running expensive hardware.

By making it computationally expensive to alter past blocks, PoW provides immutability and decentralisation, while also defending against double-spend attacks: an attacker who tried to spend the same coin twice would have to outpace the honest network.

## An aside — pruning the search tree?

Since Bitcoin mining is lucrative but also extremely competitive, any efficiency gain produces real revenue. I've sometimes wondered about algorithms that prune the nonce search space — discarding nonces that are *unlikely* to produce a low-enough hash before bothering to compute them. The trouble is that for SHA-256, "unlikely" can only be determined by *running the hash*, which is the work you were trying to avoid. Every pruning heuristic I've considered would forfeit more compute on bookkeeping than it would save on skipped nonces.

## Other algorithms

Since the Nakamoto paper there have been other consensus algorithms — Proof of Stake, Proof of Authority, Delegated PoS, BFT variants. We'll cover the most important of them in the next pages.

Bitcoin's PoW updates the network roughly every 10 minutes on average. But what *exactly* do miners hash, and what does a block look like on the wire? That's the next page.

The block consists of the following:

<!-- continued on the next page — block structure -->
