## A passive machine, and the first action

Before we let players bet, it's worth noticing something about the contract we've built — something that's true of *every* contract, and that trips up people coming from ordinary programming.

### The contract does nothing on its own

Our `PokerTable` can seat players and start a hand. But it has no heartbeat. There is no loop running, no timer counting down, no background process waiting to deal the next card. Between transactions the contract is completely **inert** — a frozen lump of state sitting at an address. The blinds we so carefully posted will sit in the `pot` for a minute, a year, or a decade, exactly as we left them, until *someone sends a transaction*.

This is fundamental to the EVM. A smart contract is not a running program; it's a set of functions that only execute when an external account **pokes** them by sending a transaction (and paying the gas). Nothing happens "by itself." If `startHand` is never called, the hand never starts. If, after the blinds are posted, no player ever acts, the contract waits forever — the chips are stuck mid-hand for all eternity, with no force in the system that will ever move them along.

Contrast that with the off-chain game we built in the rest of the book: that has a real process, a real loop, real timers. It can decide on its own that a player has timed out and fold them. Our contract cannot. It can only *react*. There's no `setTimeout`, no cron, no scheduler inside the EVM.

> **"But what about time-based logic?"** Contracts *can* read the current block timestamp (`block.timestamp`) and gate behaviour on it — "this auction ends after time T." But reading the time is not the same as *acting* at a time. Even a contract that knows the deadline has passed still needs someone to send a transaction that triggers the consequence. The chain will not call you; you must call it. This is why so many protocols rely on external **keepers** or **bots** whose entire job is to poke contracts at the right moment (close an auction, liquidate a position, advance a game). The autonomy lives *outside* the contract.

So our poker table is a passive state machine. It's a very honest model to hold in your head: the contract is a referee who only speaks when spoken to, and only ever says one thing per transaction.

### The simplest action: folding

Given that, let's add the smallest possible action a player can take — **folding**. Folding needs no money to move and no bet to match; the player simply gives up the hand. It's the perfect first action because it lets us wire up *turn-taking* and *winning* without yet touching the complexity of bets, calls, and raises.

First, a player can be folded, so the `Player` struct gains a flag, and we track how many players are still live:

```solidity
struct Player {
    address wallet;
    uint256 stack;
    bool seated;
    bool folded;   // has given up the current hand
}

uint8 public activeInHand;   // players who have not folded this hand
```

`startHand` now resets the per-hand flags and seeds the active count:

```solidity
pot = 0;
for (uint8 i = 0; i < MAX_PLAYERS; i++) {
    if (seats[i].seated) {
        seats[i].folded = false;
    }
}
activeInHand = playerCount;
```

(That loop is, once again, bounded by `MAX_PLAYERS` — nine — so it's the safe kind.)

Now `fold` itself:

```solidity
function fold() external {
    require(handInProgress, "no hand in progress");

    uint8 seat = actingSeat;
    require(seats[seat].wallet == msg.sender, "not your turn");
    require(!seats[seat].folded, "already folded");

    seats[seat].folded = true;
    activeInHand--;

    emit PlayerFolded(seat);

    if (activeInHand == 1) {
        _settleToLastPlayer();
    } else {
        actingSeat = _nextActiveSeat(seat);
    }
}
```

Reading it:

1. **A hand must be running** — you can't fold into thin air.
2. **It must be your turn.** `actingSeat` is whose action it is; we check the caller owns that seat. This is how the contract enforces *order* without a scheduler — it simply refuses any action from anyone but the one seat it's currently waiting on. Turn-taking falls out of a single `require`.
3. **You can't fold twice.**
4. **Record the fold** and decrement the live count.
5. **Then the key branch:** if only *one* player is left standing, the hand is over and that player wins the pot. Otherwise, pass the action to the next live player clockwise.

### Winning by everyone else folding

In real poker, the most common way a hand ends isn't a showdown — it's everyone folding to one player. We can settle that case completely, even though we haven't dealt a single card:

```solidity
function _settleToLastPlayer() private {
    uint8 winner = _firstActiveSeat();

    uint256 amount = pot;
    pot = 0;
    seats[winner].stack += amount;
    handInProgress = false;

    emit HandSettled(winner, amount);
}
```

The last player standing wins the pot. We zero the `pot` *before* crediting it to the winner's stack (effects in a safe order), bump their stack, mark the hand finished, and emit. Note the money never leaves the contract — it just moves from the `pot` accumulator into a player's `stack`. The real stablecoin only moves when a player eventually cashes out and leaves the table (a function we'll add later).

The helpers are the same skip-the-gaps ring walks as before, now also skipping folded players:

```solidity
function _firstActiveSeat() private view returns (uint8) {
    for (uint8 i = 0; i < MAX_PLAYERS; i++) {
        if (seats[i].seated && !seats[i].folded) {
            return i;
        }
    }
    revert("no active seat");
}

function _nextActiveSeat(uint8 from) private view returns (uint8) {
    for (uint8 i = 1; i <= MAX_PLAYERS; i++) {
        uint8 candidate = (from + i) % MAX_PLAYERS;
        if (seats[candidate].seated && !seats[candidate].folded) {
            return candidate;
        }
    }
    revert("no active seat");
}
```

### A complete (if pointless) hand

Put it together and we have, for the first time, a hand that begins and *ends*:

1. Three players join.
2. Someone calls `startHand` — blinds posted, action on the under-the-gun seat.
3. Players `fold` in turn.
4. When all but one have folded, the pot is pushed to the survivor and the hand closes.

It's poker with no cards — you win purely by being the last one who didn't give up — which is gloriously pointless as a *game* but a complete *state machine*: deposit, seat, start, act, settle. Every transition is driven by an external transaction poking an otherwise-inert contract, exactly as the EVM demands.

> **One pot, no side pots.** We made a deliberate simplification back at the blinds: there's a single `pot` and we don't split it into side pots for short-stacked all-ins. Real poker needs that; our toy doesn't, and leaving it out keeps the focus on the Solidity rather than the poker math.

### Where we are

The contract is a passive referee that seats players, posts blinds, takes folds in turn, and awards the pot to a last player standing. What's still missing is the *interesting* action — calling and raising, where chips actually move between stacks and the "is the betting round over?" question gets genuinely subtle — and, looming behind all of it, the part we've been dancing around since the start: **the cards themselves**, and the impossibility of dealing them secretly on a public chain. That's where this stops being a Solidity exercise and becomes a lesson in what blockchains fundamentally *can't* do.
