## Our consensus

To achieve consensus among the layer of nodes in our network, every node operates in one of **two modes**:

- **Read-only nodes** hold a full or partial copy of the chain. They validate incoming blocks, serve queries, and gossip transactions and headers, but they do not produce blocks themselves.
- **Validator nodes** do everything a read-only node does, *plus* they propose and sign new blocks. They are the participants whose work the chain trusts; they are also the ones with capital at stake (per the PoS chapter on the previous page).

A validator node executes the same six operations on every block it produces. The blockchain class in the *Tech Stack* chapter will implement these in full; here we'll just establish the order of operations and measure how long each step takes.

## Order of operations

1. **Check the nonce.** Read the validator's own account nonce so the proposed block doesn't replay an earlier proposal.
2. **Verify the signature.** Confirm the validator's own key and check signatures on incoming material — both the most recent committed block and the candidate transactions about to be included.
3. **Find and sort unconfirmed transactions from the mempool.** Order them by effective gas tip / fee, respecting nonce gaps per sender, until the block is full.
4. **Create the block structure.** Header (prev hash, merkle root, timestamp, etc.) plus the chosen transaction list.
5. **Sign the block.** Hash the header and sign with the validator's private key.
6. **Broadcast the block.** Push it to peers over the p2p layer.

Every step is independently measurable, and that matters: the bottleneck shifts depending on what the network is doing. In normal traffic, *signing* and *broadcast* dominate; under DoS conditions, *mempool selection* can balloon as the pool fills with chaff.

## Measuring it — a JavaScript sketch

Here is a small JavaScript class that wraps each step in `console.time` / `console.timeEnd` so we can see, per block produced, where the budget is going. Other languages — Go, Rust, C# — will be faster on the same workload, but this gives a like-for-like comparison between operations *within* a single run.

```javascript
class BlockProducer {
  constructor({ key, mempool, chain, network }) {
    this.key      = key;
    this.mempool  = mempool;
    this.chain    = chain;
    this.network  = network;
  }

  async produce() {
    console.time("produceBlock");

    console.time("checkNonce");
    const nonce = await this.checkNonce();
    console.timeEnd("checkNonce");

    console.time("verifySignature");
    await this.verifySignature();
    console.timeEnd("verifySignature");

    console.time("collectMempool");
    const txs = await this.collectMempoolTransactions();
    console.timeEnd("collectMempool");

    console.time("buildBlock");
    const block = this.buildBlock(txs, nonce);
    console.timeEnd("buildBlock");

    console.time("signBlock");
    const signed = this.signBlock(block);
    console.timeEnd("signBlock");

    console.time("broadcast");
    await this.broadcastBlock(signed);
    console.timeEnd("broadcast");

    console.timeEnd("produceBlock");
    return signed;
  }

  async checkNonce()                 { /* TODO */ }
  async verifySignature()            { /* TODO */ }
  async collectMempoolTransactions() { /* TODO */ }
  buildBlock(txs, nonce)             { /* TODO */ }
  signBlock(block)                   { /* TODO */ }
  async broadcastBlock(signed)       { /* TODO */ }
}
```

The skeleton above is intentionally hollow — every method is a `TODO`. The Tech Stack chapter will fill them in with real implementations; for now, the *order* and the *timing harness* are the point.

When you run a real implementation of this against a populated mempool, you should see numbers roughly like this (single-validator, no network latency):

| Step               | Typical duration       |
| ------------------ | ---------------------- |
| `checkNonce`       | < 1 ms (one DB read)   |
| `verifySignature`  | ~1–2 ms (one ECDSA op) |
| `collectMempool`   | 5–50 ms (depends on N) |
| `buildBlock`       | 1–5 ms (hashing + merkle) |
| `signBlock`        | ~1–2 ms (one ECDSA op) |
| `broadcast`        | 10–100 ms (network)    |

These numbers are illustrative. The exact values matter less than the *shape*: every step except mempool selection and broadcast is cheap. If a validator is falling behind, those are the two places to look first.
