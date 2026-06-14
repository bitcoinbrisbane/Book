# Outstanding Work

A running tracker of unfinished sections, missing code, and loose ends in the book.
Generated 2026-06-14. Update as items are completed.

## Chapters with only an intro page (body unwritten)

These chapters have a polished "here's what's coming" introduction, but the
subsequent pages it promises don't exist yet.

| Chapter | Status | Promised but missing |
|---------|--------|----------------------|
| Ch 8 — Writing a Compiler | intro only | Lexing, Parsing, Semantic analysis, IR, Code generation, Runtime/VM (one page each) |
| Ch 9 — The Desktop Client | intro only | Stack choice, Wallet integration, Network layer, State management, Game rendering, Packaging |
| Ch 11 — Governance | intro only | On/off-chain governance, Voting mechanisms, Proposals & timelocks, Treasury, Upgradability, Dispute resolution |

## Rough drafts needing a rewrite

| File | Issue |
|------|-------|
| `chapter-02-the-general-idea/02_the-idea.md` | Heavy typos; contains literal author notes `(Genius bill?)` and "Need to have a part about other US legals" |
| `chapter-04-architecture/01_when-do-we-need-a-chain.md` | Skeleton with typos; "Cap theorom states that we can either have constanicey, or..." trails off; empty "Read vs Write" section |
| `chapter-04-architecture/03_writting-deterministic-code.md` | Typos in opening line (isoloate, contsctror, gettings, settters); filename also misspelled (writting) |
| `chapter-12-legals/01_legals.md` | Note-form scratchpad (typos: reguations, Vanlilla); needs to be written up or cut |

## Missing / placeholder code

| File | Line | Item |
|------|------|------|
| `chapter-03-crypto-cryptography/02_randomness.md` | 160 | `[TODO: GO CODE]` — Go sample never written |
| `chapter-03-crypto-cryptography/09_replay-attacks.md` | 140 | "placeholder for demonstration" — needs real example |
| `chapter-05-consensus/04_our-consensus.md` | 65–70 | Six hollow `/* TODO */` methods. **Intentional** — deferred to the "Tech Stack chapter" (which does not yet exist as a chapter) |

## Orphaned directories (not in SUMMARY.md)

These exist in the tree but aren't wired into the table of contents. Decide:
fold in, or delete.

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
