# Book

## Chapters

| # | Chapter | Abstract | Words |
|---|---------|----------|-------|
| 1 | [About](chapter-1-about/) | Introduction to the book, author motivations, and prerequisites. Covers why runnable code samples matter, getting started with Linux distros, and PGP/GPG for secure communications. | 2,046 |
| 2 | [The General Idea](chapter-2-the-general-idea/) | Explains the core concept of building a domain-specific blockchain using poker as the example application. Covers state management, consensus through game rules, and why blockchain suits this use case. | 290 |
| 3 | [Architecture](chapter-3-architecture/) | When and why you need a blockchain for your application. | 7 |
| 4 | [Cryptography](chapter-4-crypto-cryptography/) | Fundamentals of cryptography including random number generation, pseudo-random number generators, and the importance of entropy for private keys. Includes RC4 cipher examples. | 229 |
| 7 | [Consensus](chapter-7-consensus/) | Deep dive into consensus mechanisms, starting with the coin flip over the phone problem and Blum's protocol. Demonstrates commitment schemes with JavaScript implementation. | 941 |
| - | [PVM](chapter-pvm/) | Provable Virtual Machine concepts with shuffle implementations. | - |
| X | [Zero Knowledge](chapter-x-zero-knowledge/) | Complete implementation of provably fair deck shuffling using Fisher-Yates algorithm with zero-knowledge proofs. Includes browser-based WASM, React integration, and on-chain verification. | 1,257 |

**Total word count: ~4,770**

## Bibliography

See [bibliography.md](bibliography.md) for all references and citations.

## Sample Code

| Chapter | Sample Folder Contents |
|---------|----------------------|
| `chapter-1-about/sample/` | `pgp.js`, `p2p/index.js` |
| `chapter-4-crypto-cryptography/sample/` | `password.js`, `rc4.js`, `sha256.js`, `tripledes.js` |
| `chapter-7-consensus/sample/` | `coinflip.js` |
| `chapter-pvm/sample/` | `index.js`, `shuffle.js` |