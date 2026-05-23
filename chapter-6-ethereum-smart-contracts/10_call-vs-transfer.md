## `call`, `send`, and `transfer`

Solidity exposes three ways for one contract to send native ETH to another address. They look almost identical, are subtly different, and one of them is now actively discouraged. Knowing which is which is one of those facts that separates a junior Solidity developer from a senior one in about three seconds of code review.

```solidity
// 1) call — the modern default
(bool ok, ) = recipient.call{value: amount}("");
require(ok, "transfer failed");

// 2) send — boolean-returning, capped at 2300 gas
bool ok = recipient.send(amount);
require(ok, "send failed");

// 3) transfer — reverts on failure, capped at 2300 gas
recipient.transfer(amount);
```

## How we got here

For years the orthodox advice was *"always use `transfer`"*. Its 2300-gas stipend was supposed to be enough for the recipient to log an event but not enough to do anything dangerous (re-enter the sender). And `transfer` reverts on failure, so you couldn't accidentally ignore an error.

That orthodoxy broke in October 2019 with the **Istanbul** hard fork. EIP-1884 re-priced a few EVM opcodes — `SLOAD` went from 200 to 800 gas, `BALANCE` from 400 to 700, and `EXTCODEHASH` similarly. Suddenly, a perfectly innocent receiving contract that did one extra storage read in its fallback function — common in proxies and upgradeable wallets — was hitting the 2300 stipend and reverting. Contracts that had been working for years started rejecting incoming ETH.

The orthodoxy flipped. Today the advice is **always use `call`** with an explicit gas limit or no limit at all, and rely on **re-entrancy protection** at the *application* layer rather than at the *value-transfer* layer.

## `call` — the modern pattern

```solidity
(bool ok, bytes memory data) = recipient.call{value: amount}("");
require(ok, "transfer failed");
```

What's happening:

- `recipient.call{value: amount}("")` — sends `amount` of ETH to `recipient`, with the empty bytes `""` as call data (meaning "trigger the fallback, no specific function call").
- The recipient receives *all available gas* unless you specify `call{value: amount, gas: 10_000}("")`.
- Returns `(bool success, bytes memory returnData)`. The `bool` tells you whether the call reverted.
- **You must always check `success`.** If you don't, a failed transfer silently does nothing.

The trade-off: the recipient now has enough gas to do real work — including calling *back* into your contract. That's the re-entrancy attack vector that drained The DAO in 2016. The fix isn't to use less gas (we tried; Istanbul broke that); the fix is to write your function so re-entrancy can't hurt you.

## Re-entrancy protection — checks, effects, interactions

The discipline is one ordering rule: **before** you send ETH out, **first** check preconditions and **then** update your contract's state. That way if the recipient calls back into you, your state already reflects the withdrawal, so the recipient can't double-withdraw.

```solidity
function withdraw(uint256 amount) external {
    // 1. CHECKS
    require(balances[msg.sender] >= amount, "insufficient");

    // 2. EFFECTS — update state BEFORE the external call
    balances[msg.sender] -= amount;

    // 3. INTERACTIONS — external call last
    (bool ok, ) = msg.sender.call{value: amount}("");
    require(ok, "transfer failed");
}
```

The DAO hack worked because the original code did the external call *before* updating the balance — so the attacker re-entered, the balance still read the old value, and they withdrew again.

For belt-and-braces, OpenZeppelin ships a [`ReentrancyGuard`](https://docs.openzeppelin.com/contracts/utils#ReentrancyGuard) mixin that adds a `nonReentrant` modifier — a simple boolean lock that prevents any function on the contract from re-entering itself. Use it on every function that ends in an external call. It costs a `SSTORE` per call (a few thousand gas) — cheap insurance.

## What about `send`?

`send` is `transfer`'s boolean-returning sibling — same 2300-gas stipend, but returns `false` instead of reverting. It has the same Istanbul problem as `transfer`, plus the additional footgun that *if you forget to check the return value, failed transfers silently disappear*. There is essentially no situation where `send` is the right answer in 2026. Reach for `call` instead, and `require` the result.

## Summary

| Method     | Gas      | On failure          | Use today?                                         |
| ---------- | -------- | ------------------- | -------------------------------------------------- |
| `transfer` | 2300     | reverts             | No — broken by Istanbul re-pricings                |
| `send`     | 2300     | returns `false`     | No — same Istanbul problem, plus silent failure    |
| `call`     | all      | returns `(bool, _)` | Yes — pair with checks-effects-interactions + nonReentrant |

If a contract you're reviewing still uses `transfer`, it was probably written before October 2019 and may not handle modern proxy wallets correctly. That alone is a good reason to refactor.
