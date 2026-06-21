## Our bridge contract

Now that we understand ERC20, we can put it to work. It's tempting to imagine encoding the *entire* poker game in Solidity — the deck, the shuffle, the betting rounds, the showdown. People have tried. It's a bad idea, and understanding *why* tells us exactly what our contract should and shouldn't do.

### Why not on-chain poker?

Three problems make a fully on-chain card game impractical:

- **Cost.** Every state change costs gas. A single hand of poker involves dozens of state transitions — deal, bet, call, raise, fold, reveal. Multiply that by every player at every table and the gas bill dwarfs the pot.
- **Latency.** Block times are measured in seconds. A real-time game where players expect sub-second responses cannot wait for confirmation on every action.
- **Privacy.** The blockchain is public. Hole cards must stay hidden until showdown, but anything stored or computed on-chain is visible to everyone. Hiding cards on a transparent ledger requires heavy cryptography (commit-reveal, threshold encryption, zero-knowledge proofs) that is slow and expensive.

So the game itself runs **off-chain**, in our own virtual machine — the part of the project we've spent the rest of the book building. The blockchain's job is narrower and far better suited to its strengths: **custody of real money**.

### The bridge model

Our design is a DeFi-style **bridge**. A player deposits an underlying stablecoin such as USDT or USDC into a contract on Ethereum. That deposit is detected, and an equivalent balance of in-game "chips" is credited to the player inside our off-chain game. When they cash out, the flow runs in reverse: the underlying stablecoin is released back on-chain.

The contract that does this is `CosmosBridge`. The name is a hint at the architecture: the off-chain game runs as a Cosmos-style application chain, and this contract is the doorway between Ethereum (where the real money lives) and that chain (where the game lives). A deposit on the Ethereum side becomes a credit on the Cosmos side; a validator-signed message on the Cosmos side authorises a withdrawal back on the Ethereum side.

That single responsibility — custody and the crossing between the two worlds — is everything the contract does. It holds no game logic, knows nothing about cards or pots, and never decides who won. The separation is deliberate: a contract that custodies real money should be as small and auditable as possible. Every line is a potential vulnerability, and the less it does, the less there is to get wrong.

## Walking through CosmosBridge

It's a bigger contract than anything we've looked at so far, so we'll take it in three passes:

1. **This page** — the shape of the contract: its state, its constructor, and the curious fact that it *looks* like an ERC20 without being one.
2. **The deposit path** — how money comes *in*, including an automatic Uniswap swap when the deposited token isn't the underlying.
3. **The withdrawal path** — how money goes *out*, gated by validator signatures, plus the emergency hatch.

Here's the declaration, the imports, and the state.

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { IERC20Metadata } from "@openzeppelin/contracts/token/ERC20/extensions/IERC20Metadata.sol";
import { IValidator } from "./Vault.sol";
import { IUniswapV3, ISwapRouter } from "./Uniswap/Interfaces.sol";
import { Ownable } from "@openzeppelin/contracts/access/Ownable.sol";

contract CosmosBridge is Ownable {
    struct Deposit {
        string account;
        uint256 amount;
    }

    address public immutable underlying;
    address public vault;
    address public immutable router;
    uint256 public totalDeposits;
    address private immutable _self;

    mapping(bytes32 => bool) private withdrawNonces;
    mapping(uint256 => Deposit) public deposits;
    uint256 public depositIndex;
    // ...
}
```

### Imports and inheritance

Five imports, and they map almost one-to-one onto the things this contract talks to:

- **`IERC20`** — the token interface from the previous section. The bridge holds and moves an ERC20 (the underlying stablecoin), so it needs `transfer`, `transferFrom`, `approve`, and `balanceOf`.
- **`IERC20Metadata`** — the *extension* interface that adds `name()`, `symbol()`, and `decimals()`. The bridge reads these from the underlying token to describe itself, as we'll see in a moment.
- **`IValidator`** — pulled in from our own `Vault.sol`. The vault knows who the game's **validators** are; the bridge asks it "is this address a validator?" when checking a withdrawal signature.
- **`IUniswapV3, ISwapRouter`** — Uniswap's interfaces. If someone deposits a token that *isn't* the underlying, the bridge swaps it on Uniswap before crediting them. This is the first contract in the book that composes with an external DeFi protocol.
- **`Ownable`** — the OpenZeppelin access-control base, giving us a single `owner` and the `onlyOwner` modifier. `contract CosmosBridge is Ownable` **inherits** it (the `is` keyword is Solidity's `extends`).

### State variables

```solidity
struct Deposit {
    string account;
    uint256 amount;
}

address public immutable underlying;
address public vault;
address public immutable router;
uint256 public totalDeposits;
address private immutable _self;

mapping(bytes32 => bool) private withdrawNonces;
mapping(uint256 => Deposit) public deposits;
uint256 public depositIndex;
```

Walking through them:

- **`struct Deposit { string account; uint256 amount; }`** — the record we store for each deposit. Note the `account` is a **`string`**, not an `address`. This is the Cosmos connection: the receiver isn't an Ethereum account, it's the *name of an account on the game chain* — something like `"cosmos1abc..."`. The bridge records "credit this much to that named account" and lets the off-chain chain act on it.
- **`underlying`** — the canonical stablecoin the whole system settles in (e.g. USDT). `immutable`: fixed at deployment, unchangeable forever. Depositors can trust the underlying can never be swapped out from under them.
- **`vault`** — the address of the validator registry (our `Vault`). *Not* immutable, because the validator set is managed there and the bridge may need to be re-pointed; `setVault` (owner-only) changes it.
- **`router`** — the Uniswap swap router. `immutable`.
- **`totalDeposits`** — a running total of the underlying credited through deposits. It's the bridge's idea of "how much is genuinely owed to players," which matters for the emergency-withdraw logic later.
- **`_self`** — the contract's own address, cached once as `immutable`. Reading a stored immutable is cheaper than calling `address(this)` repeatedly, and the contract references its own address a *lot* (every balance check and transfer).
- **`withdrawNonces`** — a set of used withdrawal nonces, for replay protection. Each withdrawal carries a unique nonce; once spent, the mapping flips to `true` and the same signed withdrawal can never be used twice.
- **`deposits` / `depositIndex`** — an append-only log of every deposit, keyed by an incrementing index. `deposits` is `public`, so the auto-generated getter lets anyone read the history.

### It looks like a token, but it isn't one

Here's the part that's worth pausing on, especially right after the ERC20 chapter:

```solidity
function decimals() external view returns (uint8) {
    return IERC20Metadata(underlying).decimals();
}

function symbol() external view returns (string memory) {
    return string.concat(IERC20Metadata(underlying).symbol(), "b");
}

function name() external view returns (string memory) {
    return string.concat(IERC20Metadata(underlying).name(), " Bridge");
}

function totalSupply() external view returns (uint256) {
    return IERC20(underlying).balanceOf(_self);
}
```

These four functions are *exactly* the ERC20 metadata fields we met in the last section — `name`, `symbol`, `decimals`, `totalSupply`. But `CosmosBridge` is **not** an ERC20. It has no `balanceOf(address)`, no `transfer`, no `approve` for its own units — none of the functions that would let it move *as* a token. So why implement the metadata?

Because it makes the bridge **describe itself in terms of the asset it holds**, and it does so by *reading through* to the underlying:

- **`decimals`** simply mirrors the underlying's decimals — if the underlying is 6-decimal USDT, the bridge reports 6.
- **`symbol`** takes the underlying's symbol and appends `"b"` — so USDT becomes `USDTb`, the bridged form.
- **`name`** appends `" Bridge"` — "Tether USD Bridge".
- **`totalSupply`** returns the bridge's *actual underlying balance* — `balanceOf(_self)`. The "supply" of bridged units is, by definition, however much real stablecoin the bridge is sitting on.

This is a deliberate piece of interface design. Wallets, explorers, and tooling already know how to read ERC20 metadata; by exposing the same four functions, the bridge presents itself in a familiar shape even though its real behaviour — deposits keyed by Cosmos account, signature-gated withdrawals — is nothing like a plain token. It's borrowing the *vocabulary* of ERC20 without the *mechanics*. We'll come back to what it would mean to make this a real token (and the ERC-4626 standard for exactly that) at the end of the section.

### The constructor

```solidity
function setVault(address vault_) external onlyOwner() {
    vault = vault_;
}

constructor(address underlying_, address vault_, address router_) Ownable(msg.sender) {
    underlying = underlying_;
    vault = vault_;
    router = router_;
    _self = address(this);

    IERC20(underlying).approve(_self, type(uint256).max);
    IERC20(underlying).approve(router, type(uint256).max);
}
```

The constructor runs **once**, at deployment:

1. **`Ownable(msg.sender)`** makes the deployer the owner — in practice, our backend's wallet, which will manage the vault address.
2. It stores the three addresses (`underlying`, `vault`, `router`) and caches `_self`.
3. It sets up **two standing approvals**, both for the maximum possible amount:
   - `approve(_self, max)` — the bridge approves *itself* to move its own underlying. This is what lets the internal `transferFrom(_self, ...)` calls work later without a fresh approval each time.
   - `approve(router, max)` — the bridge approves the Uniswap router to pull tokens during a swap.

That infinite-approval pattern (`type(uint256).max`) is the same one we'll discuss with the deposit flow: pay the "approve" cost once at deployment, then never again. It's convenient and gas-efficient, and it concentrates trust in the approved spender — here, the router and the contract itself — which we'll weigh up as we go.

> **`setVault` is an owner lever.** Notice the owner can repoint `vault` at any time. Since the vault is what decides whose signatures authorise withdrawals, whoever owns the bridge ultimately controls who can sign off on moving money out. That's a significant power, and it's the kind of thing a security review of this contract would zero in on. We'll flag a few more of these as we meet them.

---

That's the shape of the contract: a custody contract that holds one stablecoin, logs deposits against Cosmos account names, and wears an ERC20-shaped mask. Next we follow the money *in* — the deposit functions, and the Uniswap swap that runs when the deposited token isn't the underlying.
