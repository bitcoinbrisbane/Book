## Validator selection

The consensus layer selects the next validator by a **random but deterministic** function. The recipe is short: take the modulus of the previous block hash by the number of active validators (52 in our default configuration), and the result is an integer between 0 and 51. That integer indexes into the deposit contract's registry of validators; whichever address sits at that index gets to propose the next block.

In pseudo-Solidity:

```solidity
function nextValidator() external view returns (address) {
    uint256 idx = uint256(blockhash(block.number - 1)) % 52;
    address candidate = validators[idx];
    require(stake[candidate] >= MIN_STAKE, "below minimum stake");
    return candidate;
}
```

Two properties matter:

- **Determinism.** Every node in the network can compute the same answer from the same block hash. There is no leader-election round, no committee voting, no extra messages. If two honest nodes ever disagreed on who the proposer is, they'd already be on different chains.
- **Unpredictability.** Future block hashes can't be known in advance, so the validator schedule can't be gamed by miners or proposers cherry-picking inclusion order. The current block hash *is* known when scheduling the *next* block, but that's intentional — it's how every node arrives at the same conclusion.

The contract also enforces that the selected validator has deposited the required capital. If their stake has fallen below the minimum (because of previous slashing, for example), we skip to the next candidate.

## When the chosen validator is unreachable

A perfectly determined leader is no good if they're offline. We need a fallback that doesn't require any extra coordination round (otherwise we've lost the "no leader election" property that made the design clean in the first place).

Our fallback is also deterministic: if the selected validator does not produce a block within `BLOCK_TIMEOUT` seconds, the next index *forward* in the validator list becomes the proposer, and the timeout begins again. Other nodes track the timeout locally; once it expires, they accept a block from `validators[idx + 1]` instead. The original validator can take their turn back the next time their index is selected.

This is a soft penalty — the missed-block validator earns no reward for that slot, but they aren't slashed. Slashing is reserved for *malicious* behaviour, not just unavailability.

## Slashable events

The penalties for a validator are harsh, and they need to be. A bond that can be lost only in dramatic circumstances has no deterrent value at the margin; a bond that can be lost too easily makes validating uneconomic. Calibration matters.

There are three slashable events in our model:

- **Double-signing.** A validator signs two distinct blocks for the same height. This is the canonical "*equivocation*" offence — they cannot have legitimately produced both blocks, so signing both is provably dishonest. Slash a substantial fraction of the stake.
- **Invalid state transition.** A validator includes a transaction whose state transition violates the rules — spending funds that don't exist, breaking a smart-contract invariant, or otherwise producing a block that fails validation by every other node. Slash a large fraction; this is intentional fraud.
- **Censorship of valid transactions.** A validator consistently omits transactions from the mempool that should obviously be included (e.g. they pay above the priority threshold and have a valid signature). This is harder to prove and the slash is correspondingly smaller — but persistent censorship over many blocks accumulates, and eventually triggers the same penalty as outright fraud.

Each of these can be proven on-chain via a fraud proof submitted by any other node. The fraud proof references the offending block, presents the evidence (two signatures, a re-execution trace, missed transactions), and on successful verification, the offender's stake on mainnet is reduced.

## What this design is *not* trying to do

It's worth being honest about the limits. Our 52-validator setup is closer to a federated chain than to Ethereum's tens-of-thousands-of-validators design. We're trading some decentralisation for predictable validator scheduling, low block times, and a cap on the size of the validator registry. That's a deliberate point on the trilemma, with the trade-offs we accepted in the *Proof of Stake* chapter front and centre.
