# Book

## Chapters

The book is organised in four parts: foundations, building the chain, the application, and context & operations.

| # | Chapter | Abstract | Words |
|---|---------|----------|-------|
| | **Part I — Foundations** | | |
| 1 | [About](chapter-1-about/) | Author motivations and prerequisites: why runnable code samples matter, getting started with Linux, and PGP/GPG for secure communications. | 2,282 |
| 2 | [The General Idea](chapter-2-the-general-idea/) | The core concept — building a domain-specific blockchain with poker as the example. State management, consensus through game rules, and why a chain suits this use case. | 620 |
| 3 | [Crypto & Cryptography](chapter-3-crypto-cryptography/) | Randomness, hash functions, keys, Merkle trees, signing, encryption, replay attacks, encodings, and zero-knowledge proofs — the primitives the rest of the book builds on. | 19,101 |
| | **Part II — Building the Chain** | | |
| 4 | [Architecture](chapter-4-architecture/) | When you need a chain, what lives on- vs off-chain, and the node's layers — discovery, communications, persistence, mempool — built as a working toy Bitcoin node in C#. | 13,038 |
| 5 | [Consensus](chapter-5-consensus/) | Proof of work, proof of stake, and the chain's own optimistic consensus: staking, slashing, deposit/mint, bridging, and validator selection. | 6,122 |
| 6 | [Anti-Patterns](chapter-6-anti-patterns/) | What not to put on a chain, starting with email and messaging. | 497 |
| | **Part III — The Application** | | |
| 7 | [CardLang](chapter-7-cardlang/) | A domain-specific language for card games: primitive types, deck definitions, deterministic shuffling, draw and manipulation primitives, ranking, and game specs. | 5,048 |
| 8 | [Writing a Compiler](chapter-8-writing-a-compiler/) | Lexing through runtime — compiling CardLang into something a node can execute. | 170 |
| 9 | [The Desktop Client](chapter-9-desktop-client/) | The player-facing app: stack choice, wallet integration, network layer, state, rendering, and packaging. | 188 |
| | **Part IV — Context & Operations** | | |
| 10 | [Ethereum Smart Contracts](chapter-10-ethereum-smart-contracts/) | How others do it: EIPs, ERC20/721/777, DeFi, Uniswap v3, token mechanics, and account abstraction — prior art for our design. | 7,780 |
| 11 | [Governance](chapter-11-governance/) | Who changes the rules and how: voting, proposals, timelocks, treasury, upgradability, and dispute resolution. | 238 |
| 12 | [Legals](chapter-12-legals/) | Property, NFTs, and copyright considerations. | 79 |
| | **Appendices** | | |
| X | [Zero-Knowledge Deck Shuffle](chapter-x-zero-knowledge/) | Provably fair deck shuffling with zero-knowledge proofs: WASM, React integration, and on-chain verification. | 1,257 |

**Total chapter word count: ~55,000** (excluding appendices). Counts are approximate and refreshed during the editorial pass.

## Bibliography

See [bibliography.md](bibliography.md) for all references and citations.

## Sample Code

| Chapter | Sample Folder Contents |
|---------|----------------------|
| `chapter-1-about/sample/` | `pgp.js`, `p2p/index.js` |
| `chapter-3-crypto-cryptography/sample/` | `password.js`, `rc4.js`, `sha256.js`, `tripledes.js` |
| `chapter-5-consensus/sample/` | `coinflip.js` |
| `chapter-pvm/sample/` | `index.js`, `shuffle.js` |