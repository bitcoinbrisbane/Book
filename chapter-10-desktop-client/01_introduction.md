## Introduction

A chain that nobody can play on is not a chain anybody plays on. This chapter covers the **desktop client** — the application that sits on a player's machine, signs their transactions, watches the chain for game state, and renders the poker table.

We'll cover, in order:

- Choosing a stack — Electron, Tauri, native, or web-wrapped.
- The wallet integration — key storage, signing, transaction submission.
- The network layer — RPC vs WebSocket, what to poll vs subscribe.
- State management — keeping the UI in sync with chain state without thrashing.
- Game rendering — the actual poker table, with cards, chips, and timers.
- Packaging and distribution — code signing, auto-update, the things that make a desktop app feel like a product rather than a prototype.

Some of the architectural decisions in this chapter echo the *book editor* we already built (an Electron + React app, committed to this repo) — that's not coincidence. The same trade-offs apply: cross-platform reach versus binary size, web tooling versus native APIs, and how much you let the user trust their own machine versus the chain.
