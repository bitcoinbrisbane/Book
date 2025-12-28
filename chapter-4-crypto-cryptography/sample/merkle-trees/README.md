# Merkle Tree Examples

This directory contains implementations of Merkle trees (also called hash trees), a fundamental data structure in blockchain and cryptography.

## Files

### Core Implementations

- **merkle_tree.py** - Complete Python implementation of Merkle trees
  - Build tree from list of data
  - Generate Merkle root
  - Create and verify Merkle proofs
  - Detailed implementation with educational comments

- **merkle-whitelist.js** - JavaScript Merkle tree for whitelist verification
  - Practical example of Merkle trees in access control
  - Demonstrates efficient membership proofs

## Running the Examples

### Python Examples

```bash
# Run the Merkle tree implementation
python3 merkle_tree.py

# Example usage in your own code:
from merkle_tree import MerkleTree

data = ["tx1", "tx2", "tx3", "tx4"]
tree = MerkleTree(data)
root = tree.get_root()
proof = tree.get_proof(1)  # Get proof for "tx2"
```

### JavaScript Examples

```bash
# Run the whitelist example
node merkle-whitelist.js
```

## Key Concepts

### What is a Merkle Tree?

A Merkle tree is a binary tree where:
- **Leaf nodes**: Contain hashes of data blocks
- **Non-leaf nodes**: Contain hashes of their children
- **Root node**: The "Merkle root" - a single hash representing all data

```
           Root Hash
          /          \
    Hash(A+B)      Hash(C+D)
      /    \         /    \
  Hash(A) Hash(B) Hash(C) Hash(D)
    |       |       |       |
   Data A  Data B  Data C  Data D
```

### Properties

1. **Efficient Verification**: Verify data membership with O(log n) hashes
2. **Tamper Evidence**: Any change to data changes the root
3. **Compact Proofs**: Prove inclusion without revealing all data

### Merkle Proofs

To prove a leaf is in the tree, you only need:
- The leaf data
- Sibling hashes along the path to the root
- For tree with N items, proof size is O(log N)

Example: To prove "tx2" is in a 4-item tree:
1. Provide Hash(tx2)
2. Provide Hash(tx1) (sibling)
3. Provide Hash(C+D) (uncle)
4. Verify: Hash(Hash(Hash(tx1)+Hash(tx2)) + Hash(C+D)) = Root

### Applications in Blockchain

#### Bitcoin
- **Transaction verification**: Each block contains a Merkle root of all transactions
- **SPV (Simplified Payment Verification)**: Light clients verify transactions without full blockchain
- **Compact block relay**: Send only missing transactions

#### Ethereum
- **State trees**: Patricia Merkle Trees for account state
- **Receipt trees**: Verify transaction receipts
- **Storage trees**: Efficient state verification

#### General Use Cases
- **Git**: Commits form a Merkle DAG
- **IPFS**: Content addressing using Merkle DAGs
- **Certificate Transparency**: Append-only logs with Merkle trees
- **Whitelists**: Efficient membership proofs (as shown in merkle-whitelist.js)

## Example: Whitelist Application

The `merkle-whitelist.js` example shows how to use Merkle trees for access control:

```javascript
// Instead of storing entire whitelist on-chain:
const whitelist = ["0x123...", "0x456...", /* 1000 addresses */]

// Store only the Merkle root (32 bytes)
const root = merkleTree.getRoot()

// User proves they're whitelisted with a small proof
const proof = merkleTree.getProof(userAddress)
// Proof size: log₂(1000) ≈ 10 hashes = 320 bytes
// vs 1000 addresses = 20,000 bytes
```

## Security Considerations

### Second Pre-image Attack
- Problem: Attacker creates fake proof by treating internal node as leaf
- Solution: Prefix leaves and internal nodes differently

### Tree Construction
- Always pad to power of 2 for balanced tree
- Handle odd number of leaves carefully (duplicate last or use null)

### Hash Function
- Use cryptographically secure hash (SHA-256, Keccak-256)
- Ensure pre-image resistance and collision resistance

## Further Reading

- **Original Paper**: Merkle, R.C. (1988). "A Digital Signature Based on a Conventional Encryption Function"
- **Bitcoin Whitepaper**: Section on SPV (Simplified Payment Verification)
- **Ethereum Yellow Paper**: Patricia Merkle Tree specification

## Related Examples

- See `../hash-functions/` for SHA-256 implementation used in Merkle trees
- See `../poker/` for commitment schemes (similar cryptographic guarantees)
