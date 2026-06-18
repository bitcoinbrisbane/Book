## Account abstraction (EIP-4337 and EIP-7702)

Ethereum was built with two account types:

- **Externally Owned Accounts (EOAs)** — backed by a private key. The only kind of account a user controls directly. EOAs can hold ETH and tokens, sign transactions, and… that's it. They can't batch, can't enforce policy, can't pay gas in anything but ETH.
- **Contract accounts** — code on-chain, with their own storage and balance. Can do anything an EOA can't — but can't *initiate* a transaction on their own. Every contract call has to start from an EOA paying the gas.

That split is the single biggest UX problem on Ethereum. A wallet user has to:

1. Hold ETH for gas, even if their actual asset is USDC.
2. Sign every interaction one-by-one (no batching).
3. Manage their own seed phrase with no recovery.
4. Lose everything if their key leaks.

**Account abstraction** (AA) erases the distinction — every account becomes a smart contract that can implement arbitrary logic for *how* a transaction is authorised and *who* pays for it.

## EIP-4337 — account abstraction without a hard fork

The genius of [EIP-4337](https://eips.ethereum.org/EIPS/eip-4337) (deployed March 2023, ratified **Final** in 2024) is that it implements AA *without changing the Ethereum protocol*. No hard fork, no new opcodes. It's purely a contract + off-chain infrastructure pattern that wallets opt into. The canonical reference for the moving parts lives at [docs.erc4337.io](https://docs.erc4337.io/).

The pieces:

- **Smart account contract** — a contract that *is* the user's wallet. It owns assets, enforces arbitrary signing logic (multisig, social recovery, session keys, biometric attestation), and exposes a `validateUserOp` method.
- **`UserOperation`** — a pseudo-transaction. Looks like a regular tx but lives in a *separate* mempool. Contains the smart account's address, the call data, gas parameters, and a signature in whatever format the smart account expects.
- **Bundlers** — off-chain actors who pick up UserOperations from the AA mempool, bundle them together, and submit them as a *single* real transaction to a singleton contract.
- **EntryPoint** — that singleton contract. Receives the bundle, validates each UserOp by calling the smart account's `validateUserOp`, executes the calls, and refunds the bundler for gas. It's deployed at the same canonical address on Ethereum mainnet and every major L2. The current release is **v0.8** (March 2025); v0.7 (`0x0000000071727De22E5E9d8BAf0edAc6f37da032`) is still widely in use, and v0.6 before it.
- **Paymasters (optional)** — contracts that *pay gas on behalf of* a UserOp. The user pays the paymaster in some other token (USDC, the dApp's own token, nothing if it's sponsored). The paymaster reimburses the bundler in ETH at the EntryPoint.

Two companion standards round out the system. [**ERC-7562**](https://eips.ethereum.org/EIPS/eip-7562) is the *validation rules* spec — the restrictions on what a smart account or paymaster may touch during validation, so a bundler can simulate a UserOp and trust that on-chain execution won't behave differently. It's what makes the alt-mempool DoS-resistant. If you ever run a bundler, ERC-7562 is the part you actually have to implement correctly.

The UX wins are substantial:

- **Gas in any token.** Or no token — a dApp can sponsor first-time user gas via a paymaster.
- **Social recovery.** Lose your key, ask three friends to sign a recovery, the smart account swaps in a new key.
- **Session keys.** Authorise a game to make small transfers without a popup per move.
- **Multi-sig and policy.** Daily spending limits, allowlisted recipients, time-locks.
- **Batching.** Approve and swap in one transaction. Or twenty.

Production adoption is meaningful but uneven: every major L2 wallet supports 4337, but adoption on mainnet is mostly confined to power users because the gas overhead of going through EntryPoint is real (~30k extra gas per UserOp).

## EIP-7702 — letting EOAs borrow contract code

[EIP-7702](https://eips.ethereum.org/EIPS/eip-7702) (shipped in the **Pectra** hard fork, 7 May 2025) is the followup that 4337 was always going to need: it lets an **EOA delegate to a contract**, so the EOA's *address* runs that contract's code while keeping its same private key. Set the delegation once with a signed authorisation and it persists until you change it (unlike a one-shot per-transaction trick).

This means an existing EOA wallet can opt into AA features (batching, session keys, sponsored gas) without migrating to a new address. No new account type, no asset transfer, no contract deployment. The user signs an *authorisation* (a small piece of signed data) and the wallet's address behaves like a smart contract.

Crucially, 7702 didn't make 4337 obsolete — **EntryPoint v0.8 absorbed it**. As of v0.8 the EntryPoint has *native* 7702 support: the UserOperation hash includes the delegation address, the contract checks the delegation is set correctly, and ERC-7562 gained a new `AUTH` validation category for it. v0.8 also ships [**Simple7702Account**](https://github.com/eth-infinitism/account-abstraction), a fully audited minimalist wallet that any EOA can safely authorise — it implements ERC-165, ERC-721/1155 receivers, ERC-1271 signatures, and 4337 v0.8 out of the box.

So the two are now one stack, not rivals: 7702 turns your existing EOA *into* a smart account, and 4337's bundler/paymaster machinery drives it. The split in practice is "7702 for regular users with existing EOAs who want a few smart-wallet features" versus "fully smart-wallet-native flows" — but both run through the same EntryPoint. Most modern wallets (Argent, Safe, Rabby, MetaMask Smart Accounts) support both.

## Why this matters

Pre-AA, *every* Ethereum dApp had the same first-time-user problem: "to use this app you need to install a wallet, write down a seed phrase, buy ETH, then come back". That funnel kills most onboarding.

Post-AA, a dApp can offer:

- One-click signup with social login (gas sponsored).
- A session that doesn't pop up for confirmations for small actions.
- Recovery via email/phone if the key is lost.
- Payment in stablecoins, never touching ETH.

For our poker project, that's the difference between *"download a wallet, fund it, then play"* and *"sign in with your email"*. We'll come back to AA when we build the desktop client in Chapter 9 — it's the single feature that most changes what a "wallet" looks like to a non-crypto-native user.

The standards are still moving. The next frontier is **native account abstraction** ([RIP-7560](https://github.com/eth-infinitism/account-abstraction)) — folding the EntryPoint's job into the protocol itself so UserOps become first-class transactions and the ~30k EntryPoint overhead disappears. Track [docs.erc4337.io](https://docs.erc4337.io/) and [eips.ethereum.org](https://eips.ethereum.org/) for the next round — paymaster compositions, multi-delegation, and the inevitable rough edges that don't show up until you've shipped to ten million users.
