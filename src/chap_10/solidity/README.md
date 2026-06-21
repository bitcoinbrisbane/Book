# Chapter 10 — On-chain poker (for fun)

The complete, compiling source for the "for fun" Texas Hold'em contract built up
across the chapter. **This is a teaching toy, not production code** — it does not
deal cards, has no randomness or hidden information, and carries several
deliberately-flagged shortcuts (see the book pages).

## Layout

- `src/PokerTable.sol` — the contract, assembled from the book pages.
- `test/` — Foundry tests.

## Prerequisites

[Foundry](https://book.getfoundry.sh/getting-started/installation):

```bash
curl -L https://foundry.paradigm.xyz | bash
foundryup
```

## Setup

Dependencies (OpenZeppelin) are **not** vendored into this repo — install them
with Forge after cloning:

```bash
cd src/chap_10/solidity
forge install OpenZeppelin/openzeppelin-contracts
```

This populates `lib/` (gitignored). The `@openzeppelin/` remapping in
`foundry.toml` points at it.

## Build & test

```bash
forge build
forge test
```
