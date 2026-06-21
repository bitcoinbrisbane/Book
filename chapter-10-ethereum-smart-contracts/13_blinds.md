## Positions and blinds

We have nine seats and players sitting in them. To start a *hand*, poker needs structure that the buy-in didn't give us: **who is the dealer**, **who is forced to bet first**, and **whose turn it is**. That structure is the dealer button and the blinds.

### A two-minute refresher

Before any cards are dealt, two players are *forced* to put chips in, so there's always something to play for:

- The **dealer button** is a marker that rotates one seat clockwise every hand. It anchors position — every other role is defined relative to it.
- The **small blind** is the first occupied seat clockwise of the button. They post a half-bet.
- The **big blind** is the next occupied seat clockwise. They post a full bet — conventionally twice the small blind.
- Action then begins to the *left of the big blind*, the seat known as "under the gun."

Two subtleties matter for the code. First, "the next seat" means the next *occupied* seat — our nine-seat array is full of gaps, and blinds skip empty chairs. Second, posting a blind is **not** a token transfer: the money is already inside the contract from the buy-in. Posting a blind just moves chips from a player's `stack` into the `pot`.

### New stakes, new state

The blinds are part of a table's stakes, so they belong in the constructor next to the buy-in bounds:

```solidity
uint256 public immutable smallBlind;
uint256 public immutable bigBlind;

constructor(
    address token_,
    uint256 minBuyIn_,
    uint256 maxBuyIn_,
    uint256 smallBlind_,
    uint256 bigBlind_
) {
    require(minBuyIn_ > 0, "min buy-in must be > 0");
    require(maxBuyIn_ >= minBuyIn_, "max must be >= min");
    require(bigBlind_ > smallBlind_, "big blind must exceed small");
    require(minBuyIn_ >= bigBlind_, "buy-in must cover big blind");

    token = IERC20(token_);
    minBuyIn = minBuyIn_;
    maxBuyIn = maxBuyIn_;
    smallBlind = smallBlind_;
    bigBlind = bigBlind_;
}
```

Two new `require`s earn their place. `bigBlind > smallBlind` is the definition of the structure. And `minBuyIn >= bigBlind` guarantees that *anyone* allowed to sit can at least cover the big blind — without it you could seat a player too poor to post, and the hand logic would have to cope with a degenerate case on its very first action. Reject it at deployment instead.

We also need some per-hand state to track where we are:

```solidity
uint8 public buttonSeat;       // seat index of the dealer button
uint256 public pot;            // chips wagered this hand
uint256 public currentBet;     // amount a player must match to stay in
uint8 public actingSeat;       // whose turn it is to act
bool public handInProgress;

event HandStarted(uint8 button, uint8 smallBlindSeat, uint8 bigBlindSeat);
event BlindPosted(uint8 seat, uint256 amount);
```

`buttonSeat` survives *between* hands — it's how the button remembers where to rotate from. `pot`, `currentBet`, and `actingSeat` describe the hand currently in progress. `handInProgress` is a guard so we can't start a second hand on top of a running one.

### Finding the next occupied seat

Everything about position is "the next occupied seat clockwise from here," so we write that once:

```solidity
function _nextOccupiedSeat(uint8 from) private view returns (uint8) {
    for (uint8 i = 1; i <= MAX_PLAYERS; i++) {
        uint8 candidate = (from + i) % MAX_PLAYERS;
        if (seats[candidate].seated) {
            return candidate;
        }
    }
    revert("no occupied seat");
}
```

This walks clockwise from `from`, wrapping around the table with `% MAX_PLAYERS`, and returns the first seat that's occupied. Starting the loop at `i = 1` means we always move *past* the starting seat — we never return `from` itself unless we've gone all the way around (which we won't, given we require multiple players).

The modular arithmetic is the whole trick: `(from + i) % 9` turns the linear array into a *ring*. Seat 8's "next" is seat 0, exactly like a real table where the last chair wraps back to the first. And the loop is bounded by `MAX_PLAYERS` — nine iterations, worst case — so it's the same safe, fixed-cost loop we justified on the previous page. If a table somehow had *no* occupied seats it would revert, but `startHand` will guarantee there are at least three before we ever call this.

### Starting the hand

```solidity
function startHand() external {
    require(!handInProgress, "hand already running");
    require(playerCount >= 3, "need at least 3 players");

    // Rotate the button to the next occupied seat.
    buttonSeat = _nextOccupiedSeat(buttonSeat);

    uint8 sbSeat = _nextOccupiedSeat(buttonSeat);
    uint8 bbSeat = _nextOccupiedSeat(sbSeat);

    _postBlind(sbSeat, smallBlind);
    _postBlind(bbSeat, bigBlind);

    currentBet = bigBlind;
    actingSeat = _nextOccupiedSeat(bbSeat);  // under the gun
    handInProgress = true;

    emit HandStarted(buttonSeat, sbSeat, bbSeat);
}

function _postBlind(uint8 seat, uint256 amount) private {
    Player storage p = seats[seat];
    uint256 posted = amount > p.stack ? p.stack : amount;  // all-in for less

    p.stack -= posted;
    pot += posted;

    emit BlindPosted(seat, posted);
}
```

`startHand` is the choreography of everything above:

1. **Guard the preconditions.** No hand already running, and at least three players. We require **three** so we can sidestep the heads-up special case for now: with exactly two players the button *is* the small blind and the pre-flop order inverts. Three or more, and the button / SB / BB are three distinct seats and the rules are uniform. (We'll come back for heads-up.)
2. **Rotate the button.** `buttonSeat = _nextOccupiedSeat(buttonSeat)` advances it one occupied seat clockwise. On the very first hand `buttonSeat` is `0`, so the button lands on the first occupied seat at or after seat 1 — good enough for now; a real client would randomise the opening button.
3. **Derive the blind seats.** Small blind is the next occupied seat after the button; big blind the next after that. Three chained calls to the same helper — readable precisely because we factored it out.
4. **Post the blinds.** `_postBlind` moves chips from `stack` to `pot`. Note this is pure bookkeeping — **no `token.transfer`** — because the chips are already custodied in the contract. This is the payoff of holding the buy-in: in-game money moves as plain arithmetic on `uint256`s, fast and gas-cheap, with no ERC20 round-trip.
5. **Set the opening bet and turn.** `currentBet` becomes the big blind — the amount everyone must match to stay in. `actingSeat` is the seat left of the big blind, under the gun, who acts first.

### A nod to the short stack

Look again at one line in `_postBlind`:

```solidity
uint256 posted = amount > p.stack ? p.stack : amount;
```

What if a player's stack is *smaller* than the blind they owe? In real poker they post what they have and are **all-in for less**. This `min(amount, stack)` quietly does the right thing on the money side — it never lets a stack go negative. But it doesn't yet *track* that the player is all-in, and a correct hand needs to: an all-in player can't be asked to act again, and the pot may need to split into a main pot and side pots. We've handled the arithmetic and deferred the bookkeeping — a deliberate, *labelled* shortcut rather than a silent bug. We'll return to all-ins and side pots when we build the betting round.

### Where we are

The table can now start a hand: the button rotates, the blinds are posted into the pot, and we know whose turn it is and how much they must match. We've turned a set of seated players into the opening position of a poker hand — entirely with arithmetic and modular seat-walking, no token transfers in sight.

What we *haven't* done is let anyone act. Next comes the betting round: fold, call, raise — and the surprisingly fiddly question of how a contract knows when a round of betting is *over*.
