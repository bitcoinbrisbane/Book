## Proof of Stake

Before we get to the mechanics of Proof of Stake (PoS), it's worth establishing *why* anyone would invent it. The Nakamoto paper's PoW design is elegant and battle-tested, but it pays for that elegance in real-world cost — energy, hardware, and a fixed ten-minute block time that's hard to move.

## Why decentralised consensus in the first place

While PayPal "works" — to a degree — both consumers and merchants are at the mercy of PayPal's terms and conditions, plus the other problems that arise when a third party holds funds in custody: account freezes, insolvency, jurisdictional disputes, corruption. By using a tamper-proof distributed ledger like Bitcoin, **counterparty risk is mitigated**.

But like all engineering projects, this comes with trade-offs. They have a name: the **decentralisation trilemma** — *decentralisation*, *security*, and *scalability*. Of those three properties, the conventional wisdom is that you can only have two at a time.

> There's an old engineering joke in mechanical engineering: *of cost, weight, and durability you can only have two out of three*. The trilemma is the blockchain version of the same observation.

Bitcoin sits firmly in the *decentralisation + security* corner. Tens of thousands of full nodes verify every block; the security budget is measured in terawatt-hours; and the cost is that the network can process maybe seven transactions per second worldwide.

## Why PoW isn't the only answer

Since the Nakamoto paper there have been other consensus algorithms invented to ease some of the aforementioned trade-offs in the trilemma. Most of them focus on **speed** and **throughput** at the expense of (some) decentralisation.

To be fair, I think this is often a decent trade. While I'm an advocate of decentralisation, it cannot be at the expense of usability — otherwise the system is useless. A blockchain nobody uses defends *nothing*. The interesting design question is *how much* decentralisation can you trade away before the security guarantee starts to evaporate, and at what point does the resulting system stop being meaningfully different from the centralised one it was supposed to replace?

## Quick PoW recap

Recall how PoW achieves "fair" agreement. Bitcoin defines a proof-of-work algorithm to update each node on the network every ten minutes on average. How do we *prove* that something — a computer, or someone — has done some work?

The Nakamoto consensus relies on a property of cryptographic hash functions: given an input, they produce one and only one output, and there is no correlation between two distinct inputs. The output looks random.

If we think about it like rolling a die: from statistics class we know that rolling a six requires, on average, six rolls. If we equate one roll with one unit of work, then a participant who has rolled a six has used (on average) six units of work. Hashing is the same — finding an output below an arbitrary target is *evidence* of how much computation was done, even though no single computation is itself "proof".

To make this concrete with [SHA-256](https://en.wikipedia.org/wiki/SHA-2):

```
sha256("1") = 6b86b273ff34fce19d6b804eff5a3f5747ada4eaa22f1d49c01e52ddb7875b4b
sha256("2") = d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35
```

Two adjacent inputs produce wildly different outputs. No pattern. No shortcut. Bitcoin's miners are, in effect, trying inputs and hoping the next one rolls a low enough number.

## So what does PoS do differently?

PoW asks: *who did the most work?* — and rewards that participant with the right to write the next block.

PoS asks: *who has the most at stake?* — and rewards that participant with the right to write the next block. Instead of burning electricity to prove honesty, validators lock up capital. If they cheat, the protocol *slashes* their stake — they lose money. The economic security model is preserved; the energy cost mostly isn't.

That single sentence is the whole pivot. The mechanics — how validators are selected, how slashing conditions are defined, how forks are resolved, how rewards are paid out — are what fills the rest of this chapter, and we'll cover them on the next pages.
