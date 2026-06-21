
## Shuffling a deck

We've been dancing around the cards since the start of this section. Time to face them. A hand of Hold'em needs a shuffled 52-card deck, and shuffling turns out to split into two very different problems:

1. **The mechanics** — how do you actually permute 52 cards in Solidity? This is easy.
2. **The secret** — where does the randomness come from, and how do you keep the order *hidden* from players who can read every byte of the chain? This is hard, and it's the subject of the next page.

This page tackles the easy half, and we'll write it as a reusable **library** — because shuffling is a self-contained, stateless bit of logic that other contracts (and other chapters) might want.

### Solidity libraries

A `library` in Solidity is like a contract, but with restrictions that make it a pure unit of *code* rather than *state*: a library has no storage of its own, can't hold ether, and can't be inherited from. What it can do is bundle up reusable functions. Mark them `internal` and the compiler *inlines* the library's code directly into any contract that uses it — no separate deployment, no external call, just shared source.

That's exactly what a shuffle is: take a seed, return a permuted deck. No state, no money, no identity. A perfect fit.

```solidity
library Shuffle {
    uint8 internal constant DECK_SIZE = 52;

    function shuffledDeck(uint256 seed) internal pure returns (uint8[52] memory deck) {
        // ...
    }
}
```

The function is `pure` — it reads no state and writes none, it just maps an input seed to an output deck. A 52-card deck is represented as a `uint8[52]`, where each entry is a card index `0..51`. We'll decode those indices into ranks and suits at the end.

### Fisher–Yates

The right algorithm for an unbiased shuffle is **Fisher–Yates**. The idea: walk through the deck from the last card to the first, and at each position `i`, swap that card with a randomly chosen card somewhere at or before it. Done correctly, every one of the 52! possible orderings is equally likely.

```solidity
function shuffledDeck(uint256 seed) internal pure returns (uint8[52] memory deck) {
    // Start with the identity deck 0,1,2,...,51.
    for (uint8 i = 0; i < DECK_SIZE; i++) {
        deck[i] = i;
    }

    // Fisher-Yates: walk from the last index down, swapping each card with
    // a randomly-chosen card at or before it.
    for (uint8 i = DECK_SIZE - 1; i > 0; i--) {
        uint256 r = uint256(keccak256(abi.encode(seed, i)));
        uint8 j = uint8(r % (uint256(i) + 1)); // 0 <= j <= i

        (deck[i], deck[j]) = (deck[j], deck[i]);
    }
}
```

Three things worth calling out:

- **We start from an ordered deck** `0,1,...,51` and shuffle *that*, rather than trying to draw cards into empty slots. Starting ordered guarantees the result is always a valid permutation — every card present exactly once — no matter how the randomness falls.
- **We derive a fresh random value per step:** `keccak256(abi.encode(seed, i))`. If we used the raw `seed` for all 51 swaps we'd be reusing one number, which biases the result. Hashing the seed together with the loop index `i` gives each step its own well-distributed value, cheaply. `keccak256` is the EVM's built-in hash and is effectively uniform over its output range.
- **`r % (i + 1)`** maps that big hash down to a valid swap target `0..i`. (There's a *tiny* modulo bias here because 2²⁵⁶ isn't an exact multiple of `i+1` — utterly negligible for a card deck, but the kind of thing that matters if you ever reuse this pattern for high-stakes randomness.)
- **The tuple swap** `(deck[i], deck[j]) = (deck[j], deck[i])` exchanges two entries in one line — Solidity evaluates the right side first, so no temporary variable is needed.

### Reading a card

Card indices `0..51` are compact but meaningless to a human. A quick decoder turns an index into a rank and a suit:

```solidity
function card(uint8 index) internal pure returns (uint8 rank, uint8 suit) {
    require(index < DECK_SIZE, "card out of range");
    rank = index % 13;   // 0..12  -> 2,3,...,10,J,Q,K,A
    suit = index / 13;   // 0..3   -> clubs, diamonds, hearts, spades
}
```

Thirteen ranks per suit, four suits. Index `0` is the two of clubs; index `51` is the ace of spades. Integer division gives the suit, the remainder gives the rank — a clean way to pack 52 distinct cards into a single byte each.

### Using it

A contract pulls the library in with `using Shuffle for ...` or just calls it directly:

```solidity
import { Shuffle } from "./Shuffle.sol";

uint8[52] memory deck = Shuffle.shuffledDeck(seed);
(uint8 rank, uint8 suit) = Shuffle.card(deck[0]);
```

Because the functions are `internal`, the library's code is compiled straight into the calling contract — there's nothing to deploy separately.

### The catch nobody can ignore

We have a working shuffle. Give it a seed, get back a perfectly fair permutation of 52 cards. So are we done?

Not even close — and the reason is the single most important idea in this whole exercise:

> **The shuffle is only as secret as the seed, and on a blockchain there are no secrets.** Every input we could reach for inside the contract — `block.timestamp`, `blockhash`, `block.prevrandao`, the contract's own state — is *public* and, worse, often *predictable or manipulable* by the very miners/validators who order transactions. If the seed is visible, anyone can run `Shuffle.shuffledDeck(seed)` themselves and read off every hole card before a single bet. The shuffle being mathematically fair doesn't matter one bit if the deck order is computable by the players.

That's not a bug in the library — the library is fine. It's a property of the platform. A public, deterministic machine has nowhere to hide a card. The entire difficulty of on-chain poker lives in that one sentence, and the next page is about the ways people try to escape it.
