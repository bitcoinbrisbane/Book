## Ethereum Improvement Proposals

The **Ethereum Improvement Proposal** (EIP) process is a fundamental component of how Ethereum evolves. EIPs are a standardised system for proposing and discussing changes, enhancements, and new features to the network. They are how the Ethereum community reaches consensus on important upgrades — submitted as pull requests against the EIPs repository at [github.com/ethereum/EIPs](https://github.com/ethereum/EIPs).

EIPs are typically authored by developers and other stakeholders in the Ethereum ecosystem, and they fall into three main types:

- **Core EIPs** propose changes to the Ethereum protocol itself — consensus mechanism updates, new opcodes, gas-schedule changes.
- **Networking EIPs** focus on the network layer — peer-to-peer communication, devp2p subprotocols, data synchronisation.
- **Interface EIPs** deal with standardisation that DApps and users rely on — APIs, naming conventions, and the application-level token standards we'll spend most of this chapter on.

For our poker project, we'll focus almost entirely on **contract standards** — the Interface track is where ERC20, ERC721, ERC1155 and friends live.

The EIP process is collaborative and transparent, allowing developers and the broader community to openly discuss, refine, and reach consensus on proposed changes. Once an EIP is accepted, it becomes part of the Ethereum improvement pipeline, and client teams work to implement it in the Ethereum client software. EIPs have been instrumental in shaping the network's evolution — from critical upgrades like the move to proof-of-stake (Chapter 5 covers the Merge) down to smaller improvements designed to enhance usability and security. They are a testament to the decentralised, community-driven nature of Ethereum's development.

That said, EIPs are community-run. It is up to developers, organisations, and DAOs to keep up to date and to consider adopting these standards in their contracts for the greater **network effect**. A contract that doesn't implement the standard interface forfeits the wallets, indexers, explorers, and exchanges that already know how to talk to that standard.

## The EIP lifecycle

The process to ratify an EIP is itself defined in the first one — [EIP-1](https://eips.ethereum.org/EIPS/eip-1). The stages, in order:

- **Idea** — pre-draft. Not yet tracked in the EIP repository.
- **Draft** — the first formally tracked stage of an EIP in development. An EIP is merged by an EIP Editor into the repository when properly formatted.
- **Review** — an Author marks an EIP as ready for and requesting peer review.
- **Last Call** — the final review window before moving to Final. An Editor assigns Last Call status and sets a review end date (`last-call-deadline`), typically 14 days later. If this period results in necessary normative changes, it reverts to Review.
- **Final** — the EIP represents the final standard. A Final EIP exists in a state of finality and should only be updated to correct errata and add non-normative clarifications.
- **Stagnant** — any EIP in Draft, Review, or Last Call inactive for ≥ 6 months is moved here. An EIP may be resurrected by Authors or Editors back to Draft or its earlier status. If not resurrected, a proposal may stay forever in this status.
- **Withdrawn** — the Author(s) have withdrawn the proposed EIP. This state has finality and cannot be resurrected under this EIP number. If the idea is pursued at a later date, it is treated as a new proposal.

EIP Authors are notified of any algorithmic change to the status of their EIP.

That status field is worth checking before you adopt anything: a *Draft* or *Stagnant* EIP is not the standard. *Final* is. Most production contracts in the wild reference final EIPs by number — `ERC20`, `ERC721`, `ERC4626` — and that traceability all the way back to a finalised proposal is what makes the standard worth following.
