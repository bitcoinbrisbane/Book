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

## Three structural fixes — EIP-1363, EIP-2612, EIP-3009

The two-transaction UX problem (approve, then call) has three distinct standardised solutions. They attack it from different angles — the *call* side, the *approval* side, and the *transfer* side.

### EIP-1363 — `approveAndCall`

[EIP-1363](https://eips.ethereum.org/EIPS/eip-1363) — "Payable Token" — extends ERC20 with three function pairs that *combine* the approval with the next call into a single transaction:

- `transferAndCall(to, value, data)` — transfer + invoke `onTransferReceived` on the recipient.
- `transferFromAndCall(from, to, value, data)` — same but for `transferFrom`.
- `approveAndCall(spender, value, data)` — approve + invoke `onApprovalReceived` on the spender.

Instead of `token.approve(pool, 1000); pool.deposit(1000);`, the user calls `token.approveAndCall(pool, 1000, depositCalldata)`. The token does the `approve` and then synchronously triggers the deposit on the spender — one signature, one gas payment, one transaction.

The catch: both the *token* and the *spender* contract have to implement the EIP-1363 hooks. Newly deployed tokens often ship it, but most existing DeFi protocols pre-date the standard and don't have the matching `onApprovalReceived` handler. Adoption has been steady but unspectacular.

### EIP-2612 — `permit`

[EIP-2612](https://eips.ethereum.org/EIPS/eip-2612) attacks the same problem from the *signature* side rather than the *call* side. It adds a `permit(owner, spender, value, deadline, v, r, s)` method that takes a signed approval instead of requiring an on-chain transaction. The user signs the approval off-chain (free), and the protocol contract includes the signature in its own transaction — collapsing the two-transaction flow into one and *also* eliminating the race-condition window (because there's no on-chain "old allowance" state visible to a front-runner).

Not every ERC20 supports it. DAI does. Aave's aTokens do. Most newly-deployed tokens add it because it's roughly a 30-line extension. Structurally it's the right fix for both the `approve` race and the two-transaction UX problem — and it's why "gasless" DeFi UX got dramatically better around 2021.

### EIP-3009 — `transferWithAuthorization`

[EIP-3009](https://eips.ethereum.org/EIPS/eip-3009) goes further than `permit`. Instead of letting a third party push through a *pre-approved* transfer, the user signs an authorisation for the *transfer itself* — and anyone with the signature can submit it on-chain. Skip the allowance entirely: one signed message, one transaction, one move of funds.

```solidity
function transferWithAuthorization(
    address from,
    address to,
    uint256 value,
    uint256 validAfter,
    uint256 validBefore,
    bytes32 nonce,
    uint8 v,
    bytes32 r,
    bytes32 s
) external;
```

Two design choices set it apart from `permit`:

- **Non-sequential nonces.** EIP-2612 uses a `uint256` nonce that increments per call, which means concurrent authorisations conflict — sign two payments at the same time and only one can land. EIP-3009 uses `bytes32` random nonces, so you can have any number of signed authorisations in flight at once. This is essential for machine-to-machine payments and high-frequency systems where the user isn't sitting at a wallet UI between every sign.
- **Time windows.** `validAfter` / `validBefore` let the signature be valid only within a specific time range. The signer can write a postdated cheque, or one that expires.

**Adoption is fragmented**, which is the honest weakness of the standard. Circle adopted EIP-3009 for **USDC** (v2 and later). MakerDAO went the other way and uses `permit` (EIP-2612) for **DAI**. Tether (the largest stablecoin by market cap) supports *neither*. So a protocol that wants to handle "any stablecoin" still ends up with three code paths: 3009 for USDC, 2612 for DAI, and the boring two-transaction `approve` + `transferFrom` for USDT. The dream of "one signed message, any stablecoin" is exactly that — a dream — until the standards converge.

### Which to reach for

- **EIP-3009 (transferWithAuthorization)** when you specifically want a *transfer* signed off-chain and don't want any allowance lingering after the move. Best for one-shot payments. USDC v2 is the canonical example.
- **EIP-2612 (permit)** when you want a *recurring* relationship — a protocol that will move tokens repeatedly on the user's behalf (deposits, swaps, repayments) after one off-chain signature.
- **EIP-1363 (approveAndCall)** when you don't want any off-chain signature at all — both the token *and* the spender support it, and you want one *on-chain* call to do approve + invoke.
- **None of them** are universally supported. Production code defers to the lowest common denominator — separate `approve` + call — and uses 1363 / 2612 / 3009 as fast-paths when the token in question implements them.

The next page covers what `transferFrom`'s mirror image looks like for native ETH — the `call` / `send` / `transfer` story.
