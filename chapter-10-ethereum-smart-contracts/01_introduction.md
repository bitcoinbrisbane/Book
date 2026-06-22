## Introduction

Up until now our focus has been on building distributed ledger technology. The chain itself. Discovery, gossip, mempool, validation, persistence. But one of the most compelling aspects of blockchains, and *the* defining feature of Ethereum, is the concept of **smart contracts**: self-executing pieces of code that run on the blockchain, allowing for automated and trustless execution of predefined rules and logic.

In our poker project, smart contracts will play a small role.  We could try and code the entire game logic in solidity, but we would run in to significant challanges.  Our approach is be a defi style bridging contract where players can deposit an underlying stable coin, like USDT that will be bridged into the game.

## Solidity

[Solidity](https://soliditylang.org/) is the most popular language for Ethereum and EVM-compatible projects. It's a simple, C-like language with curly braces and a JavaScript feel. More than 90% of Ethereum smart contracts in production are written in Solidity, which means it's by far the largest body of code, documentation, examples, and battle-tested patterns to learn from.

We'll cover the major language features in the following pages:

- **Contracts** — the unit of deployment; like a `class` in C# or Java.
- **Interfaces** — declared shape only, no implementation; for typed cross-contract calls.
- **Accessor modifiers** — `external`, `public`, `internal`, `private`, similar to TypeScript or C#.
- **Structs** — composite types, like a small record.
- **Comparison operators** — exactly what they look like, with one or two EVM gotchas around integer width and signedness.
- **Custom exceptions** — defined errors that emit a typed revert reason.
- **Inheritance** — single and multiple, with `is` and `super`.

## The starting frame of a token contract

Here's a simple example of a token contract, the kind of contract you'd write for a stablecoin or any other fungible asset.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract MyToken {
    // ...
}
```

Three things appear before the contract body even starts:

1. **The license identifier.** The `// SPDX-License-Identifier:` line is required by the Solidity compiler. SPDX is a standardised way of declaring a source file's license, and the Ethereum ecosystem adopted it via EIP. You can read more about valid identifiers at [spdx.org/licenses](https://spdx.org/licenses/). We'll choose one for our project later in the book.
2. **The pragma.** `pragma solidity ^0.8.20;` declares which compiler versions this source is valid for. The `^` means "0.8.20 or any 0.8.x above it". Pin too loosely and a future compiler change can break your contract; pin too tightly and you can't pick up security fixes. Most production code uses a caret in the 0.8.x range.
3. **The `contract` keyword** — opens a new contract definition, the unit Ethereum deploys to an address.

The example is going somewhere specific: we're going to implement an **ERC20** token. ERC20 is the widely-adopted standard for fungible tokens on Ethereum, used for everything from stablecoins to governance tokens to in-game currency. For our poker project it'll be the natural shape for chips, rewards, and any in-game asset that can be transferred between players.

But first, some background on **EIPs** and the **ERC20 standard** — covered on the next page.
