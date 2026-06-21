## For fun: a poker hand in Solidity

We spent the whole bridge section explaining why poker *shouldn't* run on-chain for cost, latency, privacy. All true. But "you shouldn't" and "you can't" are different claims, and the second one is a great way to learn Solidity properly. So, purely for fun (and with no intention of ever deploying this to mainnet), let's try to write a single hand of Texas Hold'em as a smart contract.

We'll build it up over several pages, and we'll hit every one of those "why not on-chain" problems head-on as we go which is exactly the point. By the end you'll understand the constraints not as abstract warnings but as walls you ran into yourself.

This page sets up the table: the contract, the buy-in rules, the nine seats, and the `join` function that takes a player's money and sits them down.

### The shape of a table

A poker table has a few fixed properties before anyone sits down:

- **A stake** — the buy-in. Most tables set a *minimum* and a *maximum* you can bring (a "$1/$2 with $100–$300 buy-in" table). Bring too little and you can't cover the blinds; bring too much and you tower over the table.
- **A number of seats** — we'll fix ours at **nine**, the classic full-ring Hold'em table.
- **A currency** — chips. On-chain, that's an ERC20 stablecoin, the same kind we bridged in the last section.

So the constructor needs three things: the token, the minimum buy-in, and the maximum buy-in. The seat count is a constant.

### The skeleton

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

contract PokerTable {
    uint8 public constant MAX_PLAYERS = 9;

    IERC20 public immutable token;
    uint256 public immutable minBuyIn;
    uint256 public immutable maxBuyIn;

    struct Player {
        address wallet;
        uint256 stack;   // chips currently in front of the player
        bool seated;
    }

    Player[MAX_PLAYERS] public seats;
    uint8 public playerCount;

    event PlayerJoined(address indexed player, uint8 seat, uint256 buyIn);

    constructor(address token_, uint256 minBuyIn_, uint256 maxBuyIn_) {
        require(minBuyIn_ > 0, "min buy-in must be > 0");
        require(maxBuyIn_ >= minBuyIn_, "max must be >= min");

        token = IERC20(token_);
        minBuyIn = minBuyIn_;
        maxBuyIn = maxBuyIn_;
    }
}
```

Let's walk the pieces.

**`MAX_PLAYERS = 9`** is a `constant` — fixed at compile time, baked into the bytecode, costs no storage. Using a named constant instead of a bare `9` scattered through the code means the table size is stated once and the intent is obvious everywhere it's used.

**`token`, `minBuyIn`, `maxBuyIn`** are all `immutable` — set once in the constructor, never changed. A table's currency and stakes don't change mid-life; locking them in makes that a guarantee, not a convention, and immutables are cheaper to read than storage.

**The `Player` struct** bundles what we need to know about each seat: the player's `wallet` (where their winnings eventually go), their `stack` (the chips in front of them right now), and a `seated` flag so we can tell an empty seat from an occupied one. A fresh `Player` in an unfilled seat is all zeroes — `address(0)`, `0` stack, `seated == false` — which is a convenient "empty" sentinel.

**`Player[MAX_PLAYERS] public seats`** is a *fixed-size array* of nine `Player`s. Fixed-size (rather than a dynamic `Player[]`) matters for poker: seat 3 is always seat 3, positions are stable, and we can't accidentally grow the table beyond nine. The `public` keyword gives us a free getter to read any seat.

**`playerCount`** tracks how many seats are filled, so we can cheaply check "is the table full?" without scanning all nine.

**The constructor** validates the stakes — a zero minimum makes no sense, and the maximum can't be below the minimum — then stores the token and the buy-in bounds. The two `require`s reject a nonsensical deployment up front; a misconfigured table should fail at birth, not surprise us later.

### Sitting down: join

To join, a player commits a buy-in within the allowed range. Their stablecoin moves into the contract, and they're given a seat with a chip stack equal to what they paid in.

```solidity
function joinAtSeat(uint256 buyIn, uint8 seat) public returns (uint8) {
    require(buyIn >= minBuyIn, "below minimum buy-in");
    require(buyIn <= maxBuyIn, "above maximum buy-in");
    require(seat < MAX_PLAYERS, "seat out of range");
    require(!seats[seat].seated, "seat taken");

    // Pull the player's stablecoin into the contract.
    require(
        token.transferFrom(msg.sender, address(this), buyIn),
        "transfer failed"
    );

    seats[seat] = Player({ wallet: msg.sender, stack: buyIn, seated: true });
    playerCount++;

    emit PlayerJoined(msg.sender, seat, buyIn);
    return seat;
}
```

`joinAtSeat` takes a *specific* seat. Reading it top to bottom:

1. **The buy-in is in range.** Two `require`s enforce `minBuyIn <= buyIn <= maxBuyIn`.
2. **The seat is valid and free.** `seat < MAX_PLAYERS` keeps the index inside the array (seat 9 would be out of bounds on a nine-seat table indexed 0–8), and `!seats[seat].seated` rejects a seat that's already occupied.
3. **Take the money.** `token.transferFrom(msg.sender, address(this), buyIn)` pulls the stablecoin from the player into the contract. This is the **approve-first** pattern again: the player must have called `approve` on the token granting this contract an allowance *before* calling, or the `transferFrom` reverts. We wrap it in a `require` so a failed transfer aborts the whole join rather than silently seating a player who never paid.
4. **Seat them.** Write a fresh `Player` into the chosen seat, bump `playerCount`, emit `PlayerJoined`.

Notice the **order**: we check everything, *then* pull the money, *then* update seat state. We don't want to seat a player and *then* discover the transfer failed. (We'll refine this ordering when we talk about re-entrancy — for now the `require` on the transfer keeps us honest.)

> **A note on `transferFrom`'s return value.** We wrapped the call in a `require` because ERC20's `transferFrom` is *supposed* to return a `bool`. In practice some well-known tokens (USDT among them) don't return anything, which makes a naive `require(token.transferFrom(...))` revert even on success. Production code uses OpenZeppelin's **`SafeERC20`** (`safeTransferFrom`) to paper over these inconsistencies. We're keeping it raw here for clarity, but flag it in your head — it's one of the most common real-world ERC20 footguns.

### A convenience overload: join()

Most players don't care *which* seat they get, they just want to sit down. So we keep the original `join(buyIn)` as a thin convenience wrapper that finds the first open seat and hands off to `joinAtSeat`:

```solidity
function join(uint256 buyIn) external returns (uint8 seat) {
    require(playerCount < MAX_PLAYERS, "table full");
    seat = _findEmptySeat();
    return joinAtSeat(buyIn, seat);
}

function _findEmptySeat() private view returns (uint8) {
    for (uint8 i = 0; i < MAX_PLAYERS; i++) {
        if (!seats[i].seated) {
            return i;
        }
    }
    revert("no empty seat");
}
```

`join` does the "is there room?" check, then `_findEmptySeat` walks the array and returns the first seat where `seated` is false. All the real work — validation, the transfer, seating — lives in `joinAtSeat`, called once with the seat we found. This is the **don't-repeat-yourself** payoff of factoring the core out: one place pulls money and mutates state, and both entry points funnel through it.

### Loops cost gas

`_findEmptySeat` contains a `for` loop, and in Solidity that should always make you pause. Here's why loops are a recurring source of trouble on-chain:

- **Every iteration costs gas.** Each `SLOAD` (storage read), each comparison, each step of the counter is billed. A loop that reads storage on every pass — like this one, touching `seats[i]` — is *especially* expensive, because storage reads are among the priciest operations in the EVM.
- **The danger is an *unbounded* loop.** If the number of iterations grows with how much data users have put into the contract — looping over *every* depositor, *every* token holder, *every* past bet — then the gas cost grows with it. Push enough entries in and the loop costs more gas than fits in a single block's **gas limit**. At that point the function can *never* be called again: it reverts every time, and any funds or logic gated behind it are frozen. This is a real, catastrophic class of bug, sometimes called a "gas-limit DoS," and it has bricked live contracts.

So why is *this* loop fine? Because it is **bounded by a compile-time constant**. `_findEmptySeat` runs at most `MAX_PLAYERS` — nine — iterations, *ever*, no matter how many players come and go over the table's lifetime. Nine storage reads in the worst case is a trivial, fixed cost that can't grow and can't ever approach the block gas limit. The seat array is small *by design*, and that design choice is exactly what makes the loop safe.

The rule of thumb: **a loop over a fixed-size structure is fine; a loop over a user-growable one is a landmine.** When you see a loop in Solidity, the question to ask isn't "is there a loop?" but "what bounds it, and can a user make that bound grow?" Here the bound is the constant `9`, set by us, untouchable by any caller so we can loop with a clear conscience.

### Where we are

We have a table that nine players can buy into, with their stablecoin held in escrow by the contract and a chip stack recorded for each. That's already a working multiplayer state machine — it just doesn't *do* anything yet.

Next we'll deal with the hard part that makes poker poker: positions and blinds, then the betting itself — and, eventually, the genuinely thorny question of how you deal *hidden* cards on a public blockchain. That last one is where the "why not on-chain poker" warnings stop being theoretical.
