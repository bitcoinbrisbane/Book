## Introduction

A chain has rules. Governance is the question of *who gets to change them, and how*.

For a poker chain that's not academic: the stakes table, the rake, which game variants are legal, how disputes are settled, who can slash a misbehaving validator, and what happens when the protocol itself needs an upgrade — all of these are governance decisions. Get them wrong and the chain either ossifies (nobody can fix anything) or captures (a small group can change the rules out from under everyone else).

This chapter covers, in order:

- On-chain vs off-chain governance — what should be a vote, and what should be a hard fork.
- Voting mechanisms — token-weighted, one-account-one-vote, quadratic voting, and their failure modes (plutocracy, Sybil attacks, voter apathy).
- Proposals and timelocks — how a change goes from idea to enacted code, and why the delay between "passed" and "live" matters.
- Treasury and funding — paying for development without a foundation holding the keys.
- Upgradability — proxies, migration, and the tension between "immutable" and "fixable".
- Dispute resolution — tying governance back to the optimistic, dealer-as-validator model from Chapter 2.

The thread running through all of it: every governance mechanism is a trade-off between **agility** and **credible neutrality**. We'll build the poker chain's governance to be boring on purpose — predictable rules, slow changes, no surprises — because that's what players need to trust the table.
