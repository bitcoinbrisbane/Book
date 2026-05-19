## Email and messaging on chain

In one of my talks, I made fun of messaging and email applications as an anti-pattern for blockchains. My reasoning is simple: this is more of a *public-key-infrastructure* (PKI) problem than something a blockchain actually solves.

Why would you want to store an email to Mum about Sunday dinner on a blockchain — forever, replicated across every full node on Earth? Given that email is free, why would you *pay per byte* for the privilege?

That's the core of why I claim a blockchain has no business facilitating peer-to-peer communications. They are *just that*: two parties sending a message over the wire. The message should be considered transient. Once read — or stored locally in the user's email client and rolled into whatever backup strategy they already have — does the network even care about it any more?

Users can run servers with a known DNS, do a key exchange, and communicate through a protocol that already exists. That has been the model since SMTP shipped in 1982, and it works.

## When it might still be a good teaching project

That said — *as a learning exercise* — building a blockchain-backed messaging app is one of the better entrées into the stack. It hits two non-trivial properties straight away:

- **Distributed.** There is no central server to point at. Messages flow through a peer-to-peer network, which forces you to wrestle with the same problems we covered in Chapter 3 — discovery, gossip, mempool, persistence.
- **Pay per message.** The unit economics are obvious. Every send maps to a fee. Every fee maps to a real network resource. The cost model is concrete in a way that "deploy a smart contract" rarely is for a beginner.

Email is one of the simplest real-world protocols there is. *Sender → recipient → done.* Re-implementing it on a chain forces you to confront most of the architectural decisions in this book without the complexity of an order book or a token bridge.

So: a real product? No. A first chain to write yourself? Maybe.

## The PKI shape underneath

The *honest* version of "messaging on a blockchain" is **messaging with on-chain identity discovery**. Bob publishes his public key on the chain. Alice resolves Bob's identity to a key. They then communicate **off-chain**, encrypted under that key, using whatever transport works — TCP, Tor, a libp2p stream, Matrix — anything.

The chain does the one job it is uniquely good at: *making a small piece of state — Bob's key — globally agreed and tamper-evident*. It does **not** see the messages. It doesn't need to. A key registry plus end-to-end encryption gives you everything else for free.

Once you frame it that way, "email on chain" disappears into two well-understood problems:

1. **Key distribution** — a PKI.
2. **Encrypted transport** — TLS, Noise, the Signal protocol, your pick.

Neither one needs your love letter to Mum replicated across 15,000 full nodes for the next century.
