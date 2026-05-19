## Other consensus mechanisms

Our chain picked a Proof-of-Stake variant with a fixed validator set and deterministic round-robin selection. That's *one* point on a wide design surface. Other production chains have made different choices, and reading the trade-offs they accepted is the fastest way to understand why our design looks the way it does.

This page is a survey, not a deep dive. Each mechanism below could fill a chapter; here we're just mapping the territory.

## Delegated Proof of Stake (DPoS)

Used by EOS, Tron, and (in spirit) early BitShares. Token holders **vote** for a small fixed set of *block producers* — typically 21 to 100 — who take turns producing blocks. Voting power is proportional to staked tokens; under-performing producers get voted out.

- **Strength**: very fast block times (sub-second on EOS), high throughput.
- **Weakness**: power concentrates among a handful of producers. Voter turnout is consistently low, which lets a small coalition entrench itself. Critics argue it is *de facto* permissioned.

## Proof of Authority (PoA)

The chain explicitly designates a small list of validators by identity — typically corporate or named entities. There is no token-weighted election; validators are chosen because they're trusted. POA Network, VeChain, and (originally) Binance Smart Chain use this model.

- **Strength**: cheap, fast, predictable. Excellent for permissioned enterprise chains where the participants are known to each other.
- **Weakness**: trust is baked into the identity list. If the operators collude, the chain has no defence. There is no point pretending PoA is decentralised — it isn't, and that's often the right answer for the use case.

## Practical Byzantine Fault Tolerance (PBFT) and variants

The classical academic answer to the Byzantine generals problem, dating back to 1999. A small set of validators run a multi-round voting protocol — *pre-prepare*, *prepare*, *commit* — that produces a single agreed block. Variants include Tendermint (Cosmos, BNB Chain), IBFT (Quorum), and HotStuff (used by Aptos, Sui, Diem).

- **Strength**: **immediate finality**. Once a block is committed, it cannot be reorganised. No "wait for six confirmations" — the block is final the moment it's signed.
- **Weakness**: communication complexity is O(n²) in the number of validators. PBFT-style chains usually cap their validator set at low double-digits or low hundreds, which limits decentralisation.

## Avalanche-style consensus

The Avalanche family (Avalanche, Snow*, repeated random subsampling protocols) takes a different approach: every node repeatedly polls a *small random sample* of its peers and adopts the local majority answer. After enough rounds, the network converges with high probability.

- **Strength**: scales to thousands of validators without quadratic message overhead. Each node only ever talks to a small number of peers per round.
- **Weakness**: convergence is probabilistic, not deterministic. The probability of disagreement falls off exponentially with rounds, but it is never exactly zero — though the floor is low enough in practice that this is a theoretical concern, not an operational one.

## Proof of History (PoH)

Solana's signature contribution. Not a consensus mechanism on its own — Solana also uses a PoS-style stake-weighted validator set — but a *cryptographic clock* that lets validators agree on the order of events without exchanging timestamps. Each step is a hash of the previous step plus the time-locked input; the chain itself is a verifiable delay function.

- **Strength**: lets validators batch many events between coordination rounds, enabling Solana's ~50k TPS theoretical throughput.
- **Weakness**: requires uniformly powerful validators (high-end servers, gigabit links). The hardware floor is intentionally high, which excludes hobbyist nodes.

## Proof of Space / Time

Used by Chia. Instead of CPU work (PoW) or capital (PoS), validators commit **disk space** to the network. The protocol periodically challenges participants to prove they hold specific data, with the winning prover earning the right to extend the chain.

- **Strength**: orders of magnitude less energy than Bitcoin PoW. Spare disk capacity is, in many places, almost free.
- **Weakness**: the launch caused a measurable spike in global hard-drive prices and accelerated wear on consumer SSDs. "Environmentally friendly" was a partial story at best.

## Proof of Burn

A historical curiosity worth knowing about. Validators *destroy* tokens (send them to an unspendable address) and are rewarded with the right to mine. The economic logic is similar to PoW — a real cost is incurred — without the energy use.

Counter-Party used it; the mechanism has not seen serious modern adoption, but variants periodically resurface in academic papers and L2 designs.

## Where ours fits

Our consensus mechanism is a **bonded PoS with a deterministic round-robin** over a fixed validator set. In trilemma terms we trade some decentralisation (52 validators is small) for predictable block times and clean reasoning about slashing. We borrow:

- The *economic security model* from Ethereum-style PoS.
- The *deterministic next-leader function* from PBFT-style designs, simplified to a single integer modulo computation.
- The *bridge + L1 staking* split from optimistic-rollup designs, where the L1 holds the bond and the L2 does the work.

The result is intentionally simple. Simplicity is itself a security property — every additional consensus parameter is another surface where things can go wrong. Once the toy chain is running, the natural next step is to ask which of the above mechanisms we'd swap in for a production deployment. The answer almost certainly isn't "this exact design" — but it might well be "this design plus the slashing rules of Cosmos, and the validator-rotation of Avalanche".
