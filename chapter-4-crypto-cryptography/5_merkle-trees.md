# Merkle Trees

Now that we have an understanding about hashing, there is one data structure or algorithm we need to understand and why its application matters - and that is Merkle trees. Merkle trees are a data structure patented by Ralph Merkle in 1979.

The tree is created by first hashing individual data items (leaves), then repeatedly hashing pairs of hashes together to form parent nodes, building upward until a single root hash remains.

## Example

Let's say we have the value "ACE OF SPADES". We will call this leaf 1 or L1. Our next leaf is "KING OF SPADES", set to L2. Assume the SHA-256 hash function for this example.

```
hash("ACE OF SPADES") = 6ee0afb99a734986c820025ef6d12812c43c10357779f05613be26cd8a5e9f12

hash("KING OF SPADES") = 4dd528b068d435b667089e909dea6711531349ad0d96ea7452b60033e75ef48c
```

Now we combine hash(L1) with hash(L2) by concatenating the hex strings and hashing the result:

```
hash(6ee0afb99a734986c820025ef6d12812c43c10357779f05613be26cd8a5e9f12 +
     4dd528b068d435b667089e909dea6711531349ad0d96ea7452b60033e75ef48c)
= a19335e738444f879b26e3ca5f01f3a95524cb25676b91a64fd5a74a55a27cca
```

### Python Implementation

Here's how we can implement this in Python:

```python
import hashlib

def sha256(data: str) -> str:
    """Compute SHA-256 hash of a string and return hex digest."""
    return hashlib.sha256(data.encode('utf-8')).hexdigest()

def hash_pair(left: str, right: str) -> str:
    """Hash two hex strings together."""
    return sha256(left + right)

# Our two cards
L1 = "ACE OF SPADES"
L2 = "KING OF SPADES"

# Hash each leaf
h1 = sha256(L1)
h2 = sha256(L2)

print(f"hash(L1) = {h1}")
print(f"hash(L2) = {h2}")

# Combine to get the Merkle root
root = hash_pair(h1, h2)
print(f"Merkle Root = {root}")
```

## Building a Full Merkle Tree

For a complete deck of cards, we need to build a full tree structure:

```python
class MerkleTree:
    """A simple Merkle Tree implementation."""

    def __init__(self, leaves: list):
        self.leaves = leaves
        self.leaf_hashes = [sha256(leaf) for leaf in leaves]
        self.tree = self._build_tree(self.leaf_hashes)
        self.root = self.tree[-1][0] if self.tree else None

    def _build_tree(self, leaf_hashes: list) -> list:
        """Build the tree level by level from leaves to root."""
        if not leaf_hashes:
            return []

        tree = [leaf_hashes]
        current_level = leaf_hashes

        while len(current_level) > 1:
            next_level = []
            for i in range(0, len(current_level), 2):
                left = current_level[i]
                # If odd number of nodes, duplicate the last one
                right = current_level[i + 1] if i + 1 < len(current_level) else left
                next_level.append(hash_pair(left, right))
            tree.append(next_level)
            current_level = next_level

        return tree

# Example with 4 cards
cards = ["ACE OF SPADES", "KING OF SPADES", "QUEEN OF SPADES", "JACK OF SPADES"]
tree = MerkleTree(cards)
print(f"Merkle Root: {tree.root}")
```

## Merkle Proofs

The real power of Merkle trees is the ability to prove membership without revealing the entire dataset:

```python
def get_proof(tree, index: int) -> list:
    """Generate a Merkle proof for a leaf at the given index."""
    proof = []
    current_index = index

    for level in tree.tree[:-1]:  # All levels except root
        if current_index % 2 == 0:
            sibling_index = current_index + 1
            direction = 'right'
        else:
            sibling_index = current_index - 1
            direction = 'left'

        if sibling_index < len(level):
            proof.append((level[sibling_index], direction))
        current_index = current_index // 2

    return proof

def verify_proof(leaf_value: str, proof: list, root: str) -> bool:
    """Verify a Merkle proof."""
    current_hash = sha256(leaf_value)

    for sibling_hash, direction in proof:
        if direction == 'left':
            current_hash = hash_pair(sibling_hash, current_hash)
        else:
            current_hash = hash_pair(current_hash, sibling_hash)

    return current_hash == root

# Prove that "KING OF SPADES" is in our tree
proof = get_proof(tree, 1)  # Index 1 = KING OF SPADES
is_valid = verify_proof("KING OF SPADES", proof, tree.root)
print(f"Proof valid: {is_valid}")  # True

# Try with a card NOT in the tree
is_valid = verify_proof("TWO OF HEARTS", proof, tree.root)
print(f"Proof valid for wrong card: {is_valid}")  # False
```

This is incredibly powerful - you can prove membership with only O(log n) hashes instead of revealing all n items!

## Properties and Applications

Now, what's so important about this tree? There are a few interesting properties or applications that we can use.

### Proof of Reserves

Let's say we want to know that our bitcoin exchange is solvent and won't misappropriate funds like the FTX exchange did. We can create a Merkle tree of your bitcoin address and the balance that the exchange shows.

The exchange can publish the merkle root (a 32-byte hash that commits to all balances) without disclosing the full liquidity or total bitcoin in custody. Each user can then verify their own balance is included by checking their merkle proof against this published root.

Any user can independently verify their balance, giving us confidence that the exchange actually holds the bitcoin they claim for each customer.

## Key Benefits

1. **Efficient Verification**: Demonstrating that a leaf node is part of a given binary hash tree requires computing a number of hashes proportional to the logarithm of the number of leaf nodes.

2. **Data Integrity**: Any change to a leaf node propagates up the tree, changing the root hash.

3. **Cryptographic Commitment**: The root of the tree serves as a commitment, and leaf nodes may be revealed and proven to be part of the original commitment.
