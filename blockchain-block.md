# Bitcoin Block Structure

Understanding how Bitcoin blocks are structured is fundamental to grasping how the entire Bitcoin network operates. Each block is a carefully organized package of data that serves as both a record of transactions and a link in the blockchain.

## Block Components Overview

A Bitcoin block consists of two main parts:

1. **Block Header** (80 bytes) - Contains metadata about the block
2. **Transaction Data** (variable size) - Contains all the transactions

## Block Header Structure

The block header is exactly 80 bytes and contains six critical fields:

| Field | Size | Description |
|-------|------|-------------|
| **Version** | 4 bytes | Block version number indicating which rules to follow |
| **Previous Block Hash** | 32 bytes | SHA-256 hash of the previous block's header |
| **Merkle Root** | 32 bytes | Root hash of the Merkle tree of all transactions |
| **Timestamp** | 4 bytes | Block creation time (Unix timestamp) |
| **Difficulty Target** | 4 bytes | Current difficulty target for mining |
| **Nonce** | 4 bytes | Number used once - incremented during mining |

### Key Header Fields Explained

**Previous Block Hash**: This field creates the "chain" in blockchain. It contains the SHA-256 hash of the previous block's header, cryptographically linking blocks together. If someone tries to alter a previous block, its hash would change, breaking the chain.

**Merkle Root**: This is a single hash that represents all transactions in the block. The Merkle tree structure allows anyone to verify that a specific transaction is included in a block without downloading the entire block data.

**Nonce**: During mining, this is the field that miners increment while searching for a valid hash. When the SHA-256 hash of the entire block header meets the difficulty requirement, the block is considered successfully mined.

**Timestamp**: Must be greater than the median timestamp of the last 11 blocks and cannot be more than 2 hours in the future. This prevents manipulation of block timing.

**Difficulty Target**: Automatically adjusts every 2,016 blocks (approximately every two weeks) to maintain an average block time of 10 minutes, regardless of total network hash power.

## Transaction Data Structure

Following the 80-byte header comes the transaction data:

```
┌─────────────────────────────────────────┐
│ Transaction Count (1-9 bytes, VarInt)   │
├─────────────────────────────────────────┤
│ Transaction 1 (Coinbase Transaction)    │
├─────────────────────────────────────────┤
│ Transaction 2                           │
├─────────────────────────────────────────┤
│ Transaction 3                           │
├─────────────────────────────────────────┤
│ ...                                     │
├─────────────────────────────────────────┤
│ Transaction N                           │
└─────────────────────────────────────────┘
```

**Transaction Count**: A variable-length integer (VarInt) indicating how many transactions are in the block.

**Coinbase Transaction**: Always the first transaction, which creates new bitcoins and pays the mining reward to the miner.

**Regular Transactions**: All other transactions in the block, typically 2,000-3,000 transactions per block.

## Merkle Tree Structure

The Merkle tree is a binary tree structure that efficiently summarizes all transactions:

```
                    Merkle Root
                   /            \
              Hash AB          Hash CD
             /      \         /      \
        Hash A    Hash B  Hash C    Hash D
         |         |       |         |
      TX A       TX B    TX C      TX D
```

### How the Merkle Tree Works

1. Each transaction is hashed individually
2. Adjacent transaction hashes are paired and hashed together
3. This process continues up the tree until a single root hash remains
4. If there's an odd number of transactions at any level, the last hash is duplicated

This structure allows for efficient verification - to prove a transaction is in a block, you only need to provide the transaction and a small number of intermediate hashes (the "Merkle path").

## Block Size Specifications

| Metric | Value |
|--------|-------|
| **Block Header Size** | Exactly 80 bytes |
| **Maximum Block Size** | 1 MB (1,000,000 bytes) |
| **Average Block Size** | ~1.3 MB (with SegWit) |
| **Typical Transaction Count** | 2,000-3,000 transactions |
| **Average Transaction Size** | 250-400 bytes |

## Block Structure Example

Here's what a simplified block might look like:

```
┌─────────────────────────────────────────┐
│ BLOCK HEADER (80 bytes)                 │
│ ┌─────────────────────────────────────┐ │
│ │ Version: 0x20000000                 │ │
│ │ Prev Hash: 000000000000000000015... │ │
│ │ Merkle Root: 4a5e1e4baab89f3a325... │ │
│ │ Timestamp: 1609459200               │ │
│ │ Difficulty: 0x170d1c6c              │ │
│ │ Nonce: 3917165906                   │ │
│ └─────────────────────────────────────┘ │
├─────────────────────────────────────────┤
│ TRANSACTION DATA                        │
│ ┌─────────────────────────────────────┐ │
│ │ Tx Count: 2,847                    │ │
│ │ Coinbase: 4a5e1e4baab89f3a32518... │ │
│ │ Tx 1: 6359f0868171b1d194cbee1af2... │ │
│ │ Tx 2: e3bf3d07d4b0375638d5f1db52... │ │
│ │ ...                                 │ │
│ │ Tx 2,847: 9f2c4e5d8a7b3f1e6c9d... │ │
│ └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

## Why This Structure Matters

**Efficiency**: The block header is only 80 bytes, making it extremely efficient to transmit and verify the blockchain's integrity without downloading all transaction data.

**Security**: The cryptographic linking through previous block hashes makes the blockchain tamper-evident. Changing any historical data would require recalculating all subsequent blocks.

**Scalability**: The Merkle tree structure allows for efficient verification of individual transactions without requiring the full block data.

**Mining**: Miners only need to hash the 80-byte header repeatedly, making the mining process computationally focused and efficient.

## Key Takeaways

- The block header (80 bytes) contains all metadata and is what miners actually hash
- The Merkle root efficiently represents all transactions in a single hash
- Previous block hash creates the immutable chain linkage
- The nonce is the variable that miners adjust to find valid blocks
- Transaction data can be up to 1 MB but is separate from the header
- This structure enables Bitcoin's key properties: decentralization, security, and efficiency

The elegant simplicity of this structure—just 80 bytes of header data linking megabytes of transaction information—is one of Bitcoin's most ingenious design features. It allows the network to maintain security and consensus while remaining efficient enough for global operation.


Key Header Fields Explained
Previous Block Hash: This field creates the "chain" in blockchain. It contains the SHA-256 hash of the previous block's header, cryptographically linking blocks together. If someone tries to alter a previous block, its hash would change, breaking the chain.
Merkle Root: This is a single hash that represents all transactions in the block. The Merkle tree structure allows anyone to verify that a specific transaction is included in a block without downloading the entire block data.
Nonce: During mining, this is the field that miners increment while searching for a valid hash. When the SHA-256 hash of the entire block header meets the difficulty requirement, the block is considered successfully mined.
Timestamp: Must be greater than the median timestamp of the last 11 blocks and cannot be more than 2 hours in the future. This prevents manipulation of block timing.
Difficulty Target: Automatically adjusts every 2,016 blocks (approximately every two weeks) to maintain an average block time of 10 minutes, regardless of total network hash power.