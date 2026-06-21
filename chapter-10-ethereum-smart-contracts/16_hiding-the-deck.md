## Hiding the deck

The previous page left us with a fair shuffle and an impossible-looking problem: a blockchain is public and deterministic, so wherever we get the seed, the players can read it and compute the deck. There is no `private` that hides data from someone reading the chain state; `private` only stops *other contracts* from reading it, not humans with an archive node.

So how does anyone deal cards in a trust-minimised way? There are four families of answers, and a book is the right place to walk all four, because each makes a different trade between **trust**, **cost**, and **complexity** — and you'll meet these same trade-offs again in other chapters (randomness, oracles, privacy, rollups).

Here's the map before we walk it:

| Approach | Who holds the secret | Trust assumption | Cost / complexity |
|---|---|---|---|
| **Commit–reveal (off-chain dealer)** | A dealer/server, off-chain | Trust the dealer not to peek or collude | Low — ships everywhere today |
| **Mental poker** | No one — split across all players | Trustless between players | High — multi-round protocol |
| **Threshold / MPC** | Split across N operators | Trust fewer than *t* of *N* collude | High infrastructure |
| **ZK shuffle** | A prover, proven correct in zero knowledge | Trustless, cryptographic | Very high — but the modern "real" answer |

### 1. Commit–reveal with an off-chain dealer

The pragmatic baseline. Don't shuffle on-chain at all — shuffle *off-chain*, where secrets can exist, and use the chain only to make the dealer *accountable*.

The flow:

1. Off-chain, the dealer generates a secret seed `s` and shuffles the deck.
2. The dealer publishes a **commitment** on-chain — `keccak256(s)` — *before* the hand. This pins down the shuffle: the dealer can't change their mind later, because any other seed produces a different hash.
3. Cards are dealt off-chain (the dealer tells each player their hole cards privately).
4. At showdown, the dealer **reveals** `s` on-chain. Anyone can now recompute `keccak256(s)`, check it matches the commitment, run the same `Shuffle.shuffledDeck(s)`, and verify every card was dealt honestly.

```solidity
// During setup, before the hand:
bytes32 public deckCommitment;
function commitDeck(bytes32 commitment) external onlyDealer {
    deckCommitment = commitment;       // = keccak256(seed)
}

// At showdown:
function revealDeck(uint256 seed) external onlyDealer {
    require(keccak256(abi.encode(seed)) == deckCommitment, "seed mismatch");
    // ...now the chain (and everyone) can reconstruct and verify the deal.
}
```

**What this buys you:** the dealer can't *retroactively* cheat — they're bound to the deck they committed to. Tampering is detectable by anyone.

**What it doesn't:** the dealer *can still see every card during the hand*. They know the seed the whole time. Commit–reveal proves the deal was consistent; it does **not** stop a malicious or compromised dealer from leaking hole cards to a colluding player. You are trusting the dealer with the secret — you've just made them unable to lie about *which* secret it was.

This is, bluntly, roughly what most real-money online poker has always done: a central server you trust, with cryptographic receipts. It's cheap, simple, and the trust assumption (an honest operator) is one people accept every day. It's also the natural fit for our architecture, where the off-chain game already *is* the trusted operator.

> **Don't forget the front-running trap.** Even commit–reveal has a subtlety: the reveal transaction puts the seed in the public mempool *before* it's mined. A bot watching the mempool could, in some designs, act on the revealed seed before the reveal is finalised. Reveals must only matter *after* all betting is closed, or be paired with the commitment so an early reveal changes nothing.

### 2. Mental poker

The cryptographer's dream: deal a fair hand with **no trusted dealer at all**, where the secret is split so thoroughly that *no single party* — not even the house — ever knows the deck.

The classic construction uses **commutative encryption**: encryption where the order of locking and unlocking doesn't matter, so `Encrypt_A(Encrypt_B(card))` can be decrypted by A and B in either order. The original scheme (Shamir–Rivest–Adleman, 1979) builds this from modular exponentiation — each player picks a secret exponent `e`, and because exponents *multiply*, the locks commute. In Python pseudo-code:

```python
P = 2_147_483_647            # a large shared prime, agreed up front

class Player:
    def __init__(self, e):
        self.e = e
        self.d = pow(e, -1, P - 1)        # the inverse exponent: undoes e
    def encrypt(self, card):
        return pow(card, self.e, P)       # card ** e   (mod P)
    def decrypt(self, card):
        return pow(card, self.d, P)       # card ** d   (mod P)

alice, bob = Player(65537), Player(101)

card   = 42
locked = bob.encrypt(alice.encrypt(card))   # locked by BOTH players

# The magic: Alice can peel HER layer first, even though Bob locked last.
half   = alice.decrypt(locked)              # Bob's lock still on
final  = bob.decrypt(half)                  # 42 — fully unlocked

assert final == card                        # order of unlocking didn't matter
```

The whole game hinges on that last line: a card encrypted by everyone can be decrypted by everyone *in any order*, so the layers can be removed one at a time, by different parties, to reveal exactly one card to exactly one player. The deal goes roughly:

1. Every player encrypts the whole deck with their own secret key and shuffles it, then passes it on. After all players have done this, the deck is shuffled and locked by everyone — no one knows where any card is.
2. To deal a card to a player, every *other* player removes their encryption layer, leaving only the recipient's layer, which the recipient alone can strip to see their card.

Because every player contributed a shuffle and a key, no one can know a card unless everyone else cooperates to reveal it. It's genuinely trustless *between the players*.

The price is steep: it's a chatty, multi-round protocol with a lot of cryptographic operations and messages, and naively doing it all on-chain would be eye-wateringly expensive in gas. It's beautiful, it's decades old (Shamir, Rivest, Adleman proposed it in 1979), and it's mostly impractical to run fully on-chain for a real-time game — but it's the conceptual ancestor of everything that follows.

### 3. Threshold decryption / MPC

A middle path between "one trusted dealer" and "trust no one." Instead of a single dealer holding the secret, split it across **N operators** using a **threshold** scheme, so that any *t* of them can cooperate to decrypt a card, but any group smaller than *t* learns nothing.

The deck is encrypted to a shared public key whose private key is split across the N parties (via secret sharing). Revealing a card requires *t* of them to each contribute a partial decryption. As long as fewer than *t* of the operators collude, the deck stays secret; as long as at least *t* are honest and online, the game can proceed.

This is **multi-party computation (MPC)** applied to card dealing. It weakens the trust assumption from "trust this one server" to "trust that not too many of these N servers are corrupt" — a much easier thing to believe, especially if the operators are independent. The cost is real infrastructure: N parties running a coordinated protocol, liveness requirements, and key-management complexity. Several blockchain gaming and randomness projects use threshold schemes for exactly this reason.

### 4. Zero-knowledge shuffle

The modern, genuinely trustless on-chain answer — and the one most likely to show up again in this book's chapters on rollups and privacy.

The insight of a **zero-knowledge proof** is that you can prove a statement is *true* without revealing *why*. Applied to shuffling: a party shuffles the (encrypted) deck and produces a **zk-SNARK proof** that the new order is a valid permutation of the old one — that they didn't add, drop, or duplicate any card — **without revealing the permutation itself**. A smart contract verifies the proof on-chain (proof verification is cheap, even though generating the proof is expensive), and accepts the shuffle as honest while learning nothing about the order.

Combine that with encrypting each card to its eventual recipient, and you get a deck that is:

- **Provably fair** — the contract has verified the shuffle is a real permutation.
- **Actually hidden** — no one but the intended recipient can read a given card, and the contract itself never sees the order.
- **Trustless** — no dealer, no honest-majority assumption; the math is the guarantee.

This is what projects like **zkHoldem / zkShuffle** are built on. The catch is in the table above: *very high complexity*. You need a circuit that encodes "this is a valid shuffle," a proving system, off-chain proof generation, and an on-chain verifier — a serious cryptographic engineering effort, far beyond a single Solidity file. We won't implement a SNARK circuit here; the point is to know that this is the frontier and roughly how it escapes the impossibility: **proofs let the chain check correctness without seeing the secret.**

### So which would *we* use?

For our poker project, the honest answer is **commit–reveal with our off-chain game as the dealer** — option 1. We already run a trusted off-chain VM that holds the real game state; making it commit to its shuffles on-chain gives players a cryptographic receipt against retroactive cheating, at almost no cost, and it fits the architecture we've built. The stronger schemes (3 and 4) remove trust we've *already chosen to place* in our own operator, so they'd be solving a problem we don't have — at a cost we'd rather not pay.

But that's a *design decision*, not a law. The whole reason to know all four is that the right choice depends entirely on **who you're willing to trust**:

- Trust an operator? Commit–reveal.
- Trust a majority of several operators? Threshold/MPC.
- Trust no one, and willing to pay for cryptography? Mental poker or, in its modern form, ZK.

That spectrum — trade trust for cost and complexity — is one of the deepest recurring themes in this whole field, and you'll see it again every time a blockchain has to handle something it can't simply put in plain sight.

### Where this leaves our "for fun" contract

We set out to write poker in Solidity and we've run face-first into the wall the early chapters warned about: a public chain can custody money beautifully, sequence actions fairly, and settle outcomes verifiably — but it *cannot hide a card by itself*. Every realistic on-chain card game is, at its heart, a scheme for borrowing secrecy from somewhere the chain can't see, and then proving — by commitment, by threshold, or by zero-knowledge — that nobody cheated while the lights were off.

That's a fitting place to end the "for fun" detour. We learned a great deal of Solidity — constants, immutables, structs, fixed arrays, bounded loops, libraries, the approve/transfer dance, events, the passive state machine, commitments — and we learned *why* the real game lives off-chain, not as an abstract warning but as a wall we hit ourselves. Which is exactly what we promised at the start.
