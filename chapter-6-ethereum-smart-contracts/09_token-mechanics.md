## Token mechanics — `approve` and `transferFrom`

ERC20 transfers come in two shapes:

- **`transfer(to, amount)`** — direct push. The token holder calls it. Funds move immediately. Simple, but only works when the *caller* is the *owner*.
- **`transferFrom(from, to, amount)`** — pull. A *third party* moves tokens out of someone else's wallet, but only up to a pre-authorised amount. This is what every DeFi contract does to take your tokens.

The pull-based shape is the one that matters. It's also where most of ERC20's footguns live.

## The allowance dance

To let a contract spend your tokens, you call **`approve(spender, amount)`** on the token first. That sets a row in the token's `mapping(address => mapping(address => uint256)) allowance` storage: *"this spender can move up to amount of my tokens"*. Then the spender calls `transferFrom(your_address, recipient, amount)` — the token contract checks the allowance, debits it, and moves the tokens.

The pattern shows up everywhere:

```solidity
// On the underlying ERC20 (USDC, etc.)
IERC20(usdc).approve(address(pool), 1000e6);

// Then on the protocol contract (Uniswap, Aave, our staking contract…)
pool.deposit(1000e6);   // pool calls IERC20(usdc).transferFrom(msg.sender, address(this), 1000e6)
```

Two transactions. The user pays gas twice. The UX is the original sin of ERC20, and the fixes — `permit`, account abstraction — are largely workarounds for *not having to call approve as its own transaction*.

## The race condition

The original ERC20 standard has a well-known bug. If you currently have a 100-token allowance for some contract and want to change it to 50, you'd naively call `approve(spender, 50)`. But between the time your transaction is mined and the time it executes, the spender could front-run with a `transferFrom(you, them, 100)` *and then* let your `approve(50)` land — for a total of 150 tokens moved.

Three mitigations, in order of how the ecosystem actually uses them:

1. **Zero-then-set.** `approve(spender, 0)` first, wait for it to mine, then `approve(spender, 50)`. Annoying. Doubles the gas. Most production code doesn't bother.
2. **`increaseAllowance` / `decreaseAllowance`.** Newer ERC20 implementations (OpenZeppelin's, for example) add these as safer alternatives. They were also briefly *removed* from OpenZeppelin in 2023 because they didn't actually fix the underlying race — they just narrowed it.
3. **Just use `permit` (EIP-2612).** Signed off-chain, applied on-chain, valid for one specific allowance change. Eliminates the race because there's no window between "old allowance" and "new allowance" for an attacker to act in.

## Infinite approval — the lazy choice

A pattern you'll see in every wallet UI: when you first interact with a DEX or lender, the wallet asks you to approve `2²⁵⁶ - 1` (the maximum `uint256`). The contract can now move arbitrarily much of that token from your wallet, forever, until you revoke it.

The pro: you save gas — you only ever approve once. The con: you've granted the contract permission to drain your wallet of that token. If the contract is later exploited, your funds are exposed even if you weren't actively using the protocol.

Most wallets default to infinite approval because the alternative — approving exactly the right amount per transaction — burns ~$5 of gas per interaction and bewilders users. Most experienced users *revoke* their old approvals periodically via tools like [revoke.cash](https://revoke.cash). Most regular users don't.

## EIP-2612 — `permit`

[EIP-2612](https://eips.ethereum.org/EIPS/eip-2612) adds a `permit(owner, spender, value, deadline, v, r, s)` method that takes a *signature* instead of requiring an on-chain transaction. The user signs the approval off-chain (free), and the protocol contract includes the signature in its own transaction — turning a two-transaction flow into one.

Not every ERC20 supports it. USDC does. DAI does. Aave's aTokens do. Most newly-deployed tokens add it because it's roughly a 30-line extension. It is, structurally, the right fix for the `approve` race and the two-transaction UX problem — and it's why "gasless" DeFi UX got dramatically better around 2021.

The next page covers what `transferFrom`'s mirror image looks like for native ETH — the `call` / `send` / `transfer` story.
