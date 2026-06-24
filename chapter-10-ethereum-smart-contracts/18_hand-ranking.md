## Ranking hands

The betting round left us at the wall: dealing the flop means dealing cards, and a public chain can't keep them secret on its own. But suppose we'd solved secrecy with one of the schemes from the "hiding the deck" page, and the cards were finally face up at showdown. We'd still need to answer the oldest question in poker: *who won?*

That turns out to be a pure, deterministic computation on integers, exactly the kind of thing the EVM is good at. So it earns its own library, a sibling to `Shuffle`.

### The job

In Texas Hold'em each player makes their best five-card hand out of seven: their two hole cards plus the five community cards. So the evaluator has two parts:

1. Score any **five** specific cards into a number.
2. Try all twenty-one five-card combinations out of the seven and keep the best.

We want the score to be a single `uint256` with one property: **a better hand always scores higher.** Then the entire showdown collapses to `scoreA > scoreB`. No special-case comparison logic, no branching on hand type at the call site. Just a number.

### Encoding a score so bigger always wins

Poker hands have a category (pair, flush, full house) and then *tiebreakers within that category* (a pair of aces beats a pair of kings; ace-high flush beats king-high flush). We pack both into one integer by giving the category the most significant bits and the tiebreakers the bits below it:

```
[ category ][ kicker1 ][ kicker2 ][ kicker3 ][ kicker4 ][ kicker5 ]
   high bits  4 bits      4 bits     4 bits     4 bits     4 bits
```

A rank fits in four bits (there are thirteen of them, 0..12). The category sits above all five kickers, so *any* flush outscores *any* straight before a single kicker is consulted. Within the same category, the kickers are laid out in order of importance, high to low, so the comparison falls through to them exactly the way a human reads a hand. This is the whole trick: lexicographic ordering for free, because that's what integer `>` already does on the packed bits.

```solidity
function _pack(uint256 cat, uint8 k1, uint8 k2, uint8 k3, uint8 k4, uint8 k5)
    private
    pure
    returns (uint256)
{
    return (cat << 20) | (uint256(k1) << 16) | (uint256(k2) << 12) | (uint256(k3) << 8)
        | (uint256(k4) << 4) | uint256(k5);
}
```

The nine categories are plain constants, ordered weakest to strongest:

```solidity
uint256 internal constant HIGH_CARD      = 0;
uint256 internal constant PAIR           = 1;
uint256 internal constant TWO_PAIR       = 2;
uint256 internal constant TRIPS          = 3;
uint256 internal constant STRAIGHT       = 4;
uint256 internal constant FLUSH          = 5;
uint256 internal constant FULL_HOUSE     = 6;
uint256 internal constant QUADS          = 7;
uint256 internal constant STRAIGHT_FLUSH = 8;
```

### Scoring five cards

The core of the evaluator counts how many cards fall on each rank and each suit. Almost every category is a question about those two histograms. Five of one suit is a flush. Five consecutive ranks is a straight. A rank that appears four times is quads, three-plus-two is a full house, and so on.

```solidity
uint8[13] memory rankCount;
uint8[4] memory suitCount;
for (uint8 i = 0; i < 5; i++) {
    rankCount[hand[i] % 13]++;
    suitCount[hand[i] / 13]++;
}
```

Notice the decoding: `hand[i] % 13` is the rank and `hand[i] / 13` is the suit, the exact same encoding the `Shuffle` library produces. The two libraries speak the same card language, so a card dealt by `Shuffle` drops straight into `HandRank` with no translation.

From there it's a waterfall of checks, strongest first, returning the moment one matches. The interesting cases are the ones that need the histograms read in a particular order:

```solidity
// Walk ranks from Ace (12) down so higher ranks land first.
for (uint8 r = 13; r > 0; r--) {
    uint8 rank = r - 1;
    uint8 c = rankCount[rank];
    if (c == 4) {
        quad = rank + 1;
    } else if (c == 3) {
        trip = rank + 1;
    } else if (c == 2) {
        if (pairHi == 0) { pairHi = rank + 1; } else { pairLo = rank + 1; }
    } else if (c == 1) {
        kickers[kn++] = rank + 1;
    }
}
```

We store ranks as `rank + 1` so that zero can mean "none." Walking from the ace down means the first pair we record is always the higher pair, and the kickers come out already sorted high to low, which is exactly the order `_pack` wants them in.

### The straight, and the wheel

Straights have one famous wrinkle: the ace plays both high (`T-J-Q-K-A`) and low (`A-2-3-4-5`, the "wheel"). The low straight is the weakest of all straights, and its high card for ranking purposes is the **five**, not the ace.

```solidity
function _straightHigh(uint8[13] memory rankCount) private pure returns (uint8) {
    // Five consecutive ranks each present. Top card runs from Ace down to 6.
    for (uint8 top = 12; top >= 4; top--) {
        bool run = true;
        for (uint8 d = 0; d < 5; d++) {
            if (rankCount[top - d] == 0) { run = false; break; }
        }
        if (run) { return top + 1; }
    }
    // The wheel: A-2-3-4-5, high card is the five.
    if (rankCount[12] > 0 && rankCount[0] > 0 && rankCount[1] > 0
        && rankCount[2] > 0 && rankCount[3] > 0) {
        return 4; // five-high straight
    }
    return 0;
}
```

The main loop scans every window of five consecutive ranks from the top down. The wheel is handled as an explicit afterthought, because it's the one straight where the ace sits at the bottom rather than the top. A straight that's also a flush is checked first of all and returns `STRAIGHT_FLUSH`, which quietly covers the royal flush too: a royal is just the ace-high straight flush, the highest score the packing can produce.

### Picking the best five from seven

With `score5` in hand, `best7` is brute force, and proudly so:

```solidity
function best7(uint8[7] memory cards) internal pure returns (uint256 best) {
    // Choose the two cards to LEAVE OUT; score the remaining five.
    for (uint8 a = 0; a < 7; a++) {
        for (uint8 b = a + 1; b < 7; b++) {
            uint8[5] memory hand;
            uint8 k = 0;
            for (uint8 i = 0; i < 7; i++) {
                if (i != a && i != b) { hand[k++] = cards[i]; }
            }
            uint256 s = score5(hand);
            if (s > best) { best = s; }
        }
    }
}
```

Seven choose five is twenty-one combinations, which we enumerate by picking the two cards to *drop*. Twenty-one evaluations is nothing. This is the same lesson as the seating loops from earlier in the chapter: an unbounded loop on a public chain is a gas-limit footgun, but a loop over a small, fixed bound is perfectly safe. Twenty-one is fixed and tiny, so we reach for the clearest code rather than a clever bit-twiddling evaluator. (There are famous lookup-table hand evaluators that score a hand in a single array read. They're a beautiful optimisation, and entirely unnecessary here.)

### Wiring it into the table

Back on the `PokerTable` contract, the betting round used to simply stop once everyone had matched. Now it has somewhere to go. When `_advanceAfterAction` sees the round is complete, it sets a `bettingClosed` flag alongside the `BettingRoundComplete` event. That flag is the gate for a new `showdown` function:

```solidity
function showdown(uint8[5] calldata board, uint8[2][MAX_PLAYERS] calldata holeCards)
    external
{
    require(handInProgress, "no hand in progress");
    require(bettingClosed, "betting still open");

    uint8 bestSeat = MAX_PLAYERS; // sentinel: no winner yet
    uint256 bestScore;

    for (uint8 i = 0; i < MAX_PLAYERS; i++) {
        if (seats[i].seated && !seats[i].folded) {
            uint8[7] memory seven = [
                holeCards[i][0], holeCards[i][1],
                board[0], board[1], board[2], board[3], board[4]
            ];
            uint256 s = HandRank.best7(seven);
            emit ShowdownScored(i, s);

            if (bestSeat == MAX_PLAYERS || s > bestScore) {
                bestSeat = i;
                bestScore = s;
            }
        }
    }

    uint256 amount = pot;
    pot = 0;
    seats[bestSeat].stack += amount;
    handInProgress = false;
    bettingClosed = false;

    emit HandSettled(bestSeat, amount);
}
```

It walks every live seat, builds that player's seven cards from their two hole cards plus the shared board, scores each with `best7`, and keeps the highest. The pot moves to the winner's stack and the hand ends. It reuses the same `HandSettled` event the fold-to-last-player path emits, so a client watching the table doesn't need to care whether the hand ended by everyone folding or by a showdown.

Two honest shortcuts are labelled right in the code. The strict `>` means an exact tie leaves the whole pot with the earlier seat rather than splitting it, the same single-pot simplification we made back at the blinds. And more fundamentally, the function simply **trusts the cards it is handed**. That is the secrecy wall showing up one last time. A public chain cannot keep the hole cards hidden between the deal and the reveal, so a real game would have each player prove their revealed cards against a commitment made before the betting, the way the "hiding the deck" page describes. What lives on-chain comfortably is everything *after* the reveal: ranking the hands and moving the money.

### What we built

So we now have, in plain Solidity, every piece of a hand of Hold'em except the one the EVM fundamentally cannot give us. The chips, the turn order, the blinds, the betting, the shuffle, the evaluation of who wins, and the settlement of the pot are all here and all on-chain-friendly. The cards staying secret between the deal and the showdown is the single thing that has to live somewhere else. That gap, and not the poker, has been the real subject of this whole section.
