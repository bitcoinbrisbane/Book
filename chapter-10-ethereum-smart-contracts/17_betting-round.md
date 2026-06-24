## The betting round

Folding got us turn-taking and a way to win. Now the real action: **checking, calling, and raising** — where chips actually move between stacks, and where the deceptively simple question *"is the betting round over?"* turns out to need careful bookkeeping.

### What each action means

When it's your turn, you face an outstanding bet — `currentBet`, the amount every live player must have in the pot to stay in this round. Your own contribution so far this round is your `committed`. The gap between them is what you owe:

- **Check** — only legal when you owe nothing (`committed == currentBet`). You pass the action along without putting in chips.
- **Call** — pay the gap (`currentBet - committed`) to match the bet and stay in.
- **Raise** — increase `currentBet`, putting in your call *plus* the raise. This **reopens** the action: everyone who already acted now owes more and must act again.
- **Fold** — give up (already built).

To support this, two fields join the `Player` struct, both reset at the start of every hand:

```solidity
struct Player {
    address wallet;
    uint256 stack;
    bool seated;
    bool folded;
    bool hasActed;      // has acted since the last bet/raise this round
    uint256 committed;  // chips put into the pot this betting round
}
```

`committed` tracks the per-round contribution (the blinds seed it). `hasActed` is the key to detecting the end of the round, as we'll see.

### A shared turn guard

All four actions begin the same way: there must be a hand running, and the caller must own the seat whose turn it is. We factor that into one helper rather than repeat it four times:

```solidity
function _requireTurn() private view returns (uint8 seat) {
    require(handInProgress, "no hand in progress");
    seat = actingSeat;
    require(seats[seat].wallet == msg.sender, "not your turn");
    require(!seats[seat].folded, "already folded");
}
```

This is the contract enforcing order with nothing but a `require`: it simply refuses any action from anyone but the one seat it's currently waiting on. `fold` now uses it too.

### Check and call

```solidity
function check() external {
    uint8 seat = _requireTurn();
    require(seats[seat].committed == currentBet, "must call or fold");

    seats[seat].hasActed = true;
    emit PlayerChecked(seat);

    _advanceAfterAction(seat);
}

function call() external {
    uint8 seat = _requireTurn();
    Player storage p = seats[seat];

    uint256 owed = currentBet - p.committed;
    require(owed > 0, "nothing to call");
    require(owed <= p.stack, "insufficient stack"); // toy: no all-in/side pots

    p.stack -= owed;
    p.committed += owed;
    pot += owed;
    p.hasActed = true;

    emit PlayerCalled(seat, owed);

    _advanceAfterAction(seat);
}
```

`check` is only allowed when you owe nothing — otherwise you must call or fold. `call` computes the gap, moves exactly that many chips from `stack` to `pot`, and updates `committed`. Both mark the player as having `hasActed` and then hand off to `_advanceAfterAction`.

Notice the `require(owed <= p.stack)` in `call`: in real poker, a player who can't cover the bet goes **all-in for what they have**, and the pot splits into side pots. We decided back at the blinds that our toy skips side pots, so here we simply *reject* a call the player can't afford. It's a labelled limitation, not an oversight — flagged right in the comment.

### Raise, and reopening the action

```solidity
function raise(uint256 raiseBy) external {
    uint8 seat = _requireTurn();
    Player storage p = seats[seat];

    // Enforce the min-raise: a raise must be at least the size of the last one.
    require(raiseBy >= lastRaiseSize, "raise below minimum");

    uint256 newBet = currentBet + raiseBy;
    uint256 owed = newBet - p.committed; // call amount plus the raise
    require(owed <= p.stack, "insufficient stack");

    p.stack -= owed;
    p.committed += owed;
    pot += owed;

    currentBet = newBet;
    lastRaiseSize = raiseBy;

    // A raise reopens the action: everyone else must respond again.
    _resetActedExcept(seat);
    p.hasActed = true;

    emit PlayerRaised(seat, newBet);

    _advanceAfterAction(seat);
}
```

Two things make `raise` more than a bigger call:

**The min-raise rule.** Poker doesn't let you raise by a trivial amount to stall the game — a raise must be at least as large as the previous raise (and the big blind sets the opening minimum, which `startHand` does with `lastRaiseSize = bigBlind`). We enforce it with a single `require(raiseBy >= lastRaiseSize)`, then record the new raise size so the *next* raiser faces the updated floor.

**Reopening the action.** This is the heart of betting logic. When someone raises, every other live player who had already called is now *short* — they owe the difference — so they must get another turn. We model "still owes a decision" with the `hasActed` flag, and a raise clears it for everyone else:

```solidity
function _resetActedExcept(uint8 keep) private {
    for (uint8 i = 0; i < MAX_PLAYERS; i++) {
        if (seats[i].seated && !seats[i].folded && i != keep) {
            seats[i].hasActed = false;
        }
    }
}
```

The raiser themselves *has* acted, so they keep their flag; everyone else is reset to "not yet acted since this raise." (Bounded loop over nine seats — the safe kind, again.)

### "Is the round over?" — the subtle part

Here's the question that trips people up. A betting round is **not** over just because action has gone around once. It's over when **every live player has both acted since the last raise *and* matched the current bet.** That second condition is what lets a late raise drag everyone back for another lap.

```solidity
function _roundComplete() private view returns (bool) {
    for (uint8 i = 0; i < MAX_PLAYERS; i++) {
        if (seats[i].seated && !seats[i].folded) {
            if (!seats[i].hasActed || seats[i].committed != currentBet) {
                return false;
            }
        }
    }
    return true;
}
```

Walk every live player: if even one hasn't acted, or hasn't matched `currentBet`, the round continues. Only when all of them clear both bars is it done. After each action we check it:

```solidity
function _advanceAfterAction(uint8 seat) private {
    if (_roundComplete()) {
        emit BettingRoundComplete(pot);
        // A real game would now deal the next street (flop/turn/river) and
        // open a fresh betting round. Our toy stops here.
    } else {
        actingSeat = _nextActiveSeat(seat);
    }
}
```

### The big blind's option

This model gets a famous edge case right for free. After the blinds are posted, suppose everyone just *calls* around to the big blind. Has the round ended? No — the big blind still gets their **option** to check or raise, because the big blind was forced money, not a voluntary action.

Our logic handles this without any special case. The big blind starts the round with `committed == bigBlind == currentBet` (already matched) but `hasActed == false` (they never chose anything — the blind was posted *for* them). So `_roundComplete` returns false until the big blind actually acts. When everyone calls around, action lands back on the big blind, who can check to close the round or raise to reopen it. The general rule — *acted **and** matched* — produces the correct poker behaviour with no carve-out. That's the sign the model is right.

### Where the hand stops

Run a full round — under-the-gun calls, others call, the big blind checks — and you'll see `BettingRoundComplete` fire with the pot collected and everyone level. And then... our toy stops.

A real hand would now deal the **flop**, open a second betting round, then the **turn**, the **river**, and finally a **showdown** comparing hands. We won't deal those streets, for the reason that's haunted this whole section: dealing the flop means dealing *cards*, and we've already seen that a public chain can't keep them secret without the commit–reveal, threshold, or zero-knowledge machinery from the last two pages. The betting logic is genuinely on-chain-friendly — it's pure bookkeeping on integers, exactly what the EVM is good at. The *cards* are the wall.

We can, however, write the part that comes *after* the cards are revealed: deciding who won and paying them. That is the next page, where a hand-ranking library scores the showdown and settles the pot.

### What we built

Step back and look at what the contract now is: a complete, correct **single betting round** as an on-chain state machine. Players join with real stablecoin, blinds post, and the table walks turn by turn through checks, calls, raises (with min-raise enforced) and folds — reopening the action when someone raises, awarding the pot when everyone else folds, and correctly recognising when the round is settled, big-blind option and all. Every transition is driven by an external transaction poking an otherwise-inert referee.

That's a lot of poker, and a lot of Solidity, for something we opened by insisting you should never actually do. Which was always the point: you now understand *exactly* where the line is — what a blockchain handles beautifully (custody, ordering, settlement, bookkeeping) and what it fundamentally cannot (secrecy) — not because a book told you, but because you built right up to the wall and felt it.
