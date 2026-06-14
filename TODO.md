# Outstanding Work

A running tracker of unfinished sections, missing code, and loose ends in the book.
Generated 2026-06-14. Update as items are completed.

## Chapters with only an intro page (body unwritten)

These chapters have a polished "here's what's coming" introduction, but the
subsequent pages it promises don't exist yet.

| Chapter | Status | Issue | Promised but missing |
|---------|--------|-------|----------------------|
| Ch 8 — Writing a Compiler | intro only | [#3](https://github.com/bitcoinbrisbane/Book/issues/3) | Lexing, Parsing, Semantic analysis, IR, Code generation, Runtime/VM (one page each) |
| Ch 9 — The Desktop Client | intro only | [#4](https://github.com/bitcoinbrisbane/Book/issues/4) | Stack choice, Wallet integration, Network layer, State management, Game rendering, Packaging |
| Ch 11 — Governance | intro only | [#5](https://github.com/bitcoinbrisbane/Book/issues/5) | On/off-chain governance, Voting mechanisms, Proposals & timelocks, Treasury, Upgradability, Dispute resolution |

## Rough drafts needing a rewrite

Typos in these have been fixed; the structural gaps below remain.

| File | Issue | Remaining work |
|------|-------|----------------|
| `chapter-02-the-general-idea/02_the-idea.md` | [#6](https://github.com/bitcoinbrisbane/Book/issues/6) | Author notes `(Genius bill?)` and "part about other US legals" still to be written |
| `chapter-04-architecture/01_when-do-we-need-a-chain.md` | [#7](https://github.com/bitcoinbrisbane/Book/issues/7) | CAP sentence trails off; empty "Read vs Write" section |
| `chapter-04-architecture/03_writing-deterministic-code.md` | — | Typos fixed; filename corrected (was "writting"). Complete. |
| `chapter-12-legals/01_legals.md` | — | Typos fixed; still a note-form scratchpad — write up or cut |

## Missing / placeholder code

| File | Line | Issue | Item |
|------|------|-------|------|
| `chapter-03-crypto-cryptography/02_randomness.md` | 160 | [#8](https://github.com/bitcoinbrisbane/Book/issues/8) | `[TODO: GO CODE]` — Go sample never written |
| `chapter-03-crypto-cryptography/09_replay-attacks.md` | 140 | [#9](https://github.com/bitcoinbrisbane/Book/issues/9) | "placeholder for demonstration" — needs real example |
| `chapter-05-consensus/04_our-consensus.md` | 65–70 | — | Six hollow `/* TODO */` methods. **Intentional** — deferred to the "Tech Stack chapter" (which does not yet exist as a chapter) |

## Orphaned directories (not in SUMMARY.md)

Tracked in [#10](https://github.com/bitcoinbrisbane/Book/issues/10). These exist in the
tree but aren't wired into the table of contents. Decide: fold in, or delete.

| Path | Notes |
|------|-------|
| `chapter-5-domain-specific-language/` | Three prose files (`01-creating-our-own-langauge.md`, `02-a-simple-game.md`, `03-langauge-design.md`) — looks like an earlier draft of the CardLang/Compiler material (Ch 7/8). Filename typo: "langauge". Not referenced anywhere. |
| `chapter-4-crypto-cryptography/` | Only a `sample/` dir (signing code with committed `node_modules`). Duplicate numbering of Ch 3. |
| `chapter-pvm/` | Only a `sample/` dir; README references `chapter-pvm/sample/` but it's not in SUMMARY. |
| `src/` | `src/chap_5/node/README.md` is a 2-line stub; unclear how `src/` relates to `code/`. |

## Repo hygiene

- ~~Untrack committed `node_modules/`~~ — **done.** Only `src/chap_5/node/node_modules` (6 files) was actually tracked; removed from git, still on disk. The rest were already ignored.
- ~~`Its time to build (*).pdf` exports in repo root~~ — **done.** Untracked and added to `.gitignore`.
- `poker-formal-proof.log` is **not** tracked (already covered by `*.log`) — no action needed.
- **Dependabot: 124 vulnerabilities** (16 critical, 41 high) reported on push. These come from *untracked* `node_modules/` under the `sample/` dirs that GitHub still scanned from an earlier commit, or from the tracked `package-lock.json` files. Worth a `npm audit` pass per sample dir; not blocking.

## Notes

- Many short files are **complete, not stubs** — e.g. `08_cap-theorem.md`, `07_time-bound-nfts.md`, `04_other-eips.md`, `02_code-complete.md`, `03_prerequisites.md`, `07-cardlang/02_related-work.md`. They're brief by design; don't flag them.
