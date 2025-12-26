# Merkle Trees

Now that we have an understanding about hashing, there is one data structure or algorithm we need to understand and why its application matters - and that is Merkle trees. Merkle trees are a data structure patented by Ralph Merkle in 1979.

The tree is created by hashing together two values in a binary tree to form a node. That node is then hashed with another node and so on to form the tree.

## Example

Let's say we have the value "ACE OF SPADES". We will call this leaf 1 or L1. Our next leaf is "KING OF SPADES", set to L2. Assume the SHA-256 hash function for this example.

```
hash("ACE OF SPADES") = 6ee0afb99a734986c820025ef6d12812c43c10357779f05613be26cd

hash("KING OF SPADES") = 4dd528b068d435b667089e909dea6711531349ad0d96ea7452b60033e75ef48c
```

Now we combine hash(L1) with hash(L2) or hash(L1 + L2):

```
hash(6ee0afb99a734986c820025ef6d12812c43c10357779f05613be26cd +
     4dd528b068d435b667089e909dea6711531349ad0d96ea7452b60033e75ef48c)
= a19335e738444f879b26e3ca5f01f3a95524cb25676b91a64fd5a74a55a27cca
```

## Properties and Applications

Now, what's so important about this tree? There are a few interesting properties or applications that we can use.

### Proof of Reserves

Let's say we want to know that our bitcoin exchange is solvent, and won't rug us like the FTX exchange. We can create a Merkle tree of your bitcoin address and the balance that the exchange shows.

The exchange can then publish the proof which is an arbitrary bytes32 string like a hash. This does not disclose the full liquidity of the exchange or amount of bitcoin in custody, but the user can verify that this is correct.

We can argue that any random user can repeat the same and thus we can assume some certainty that the value held in your custodial bitcoin address is true.

## Key Benefits

1. **Efficient Verification**: Demonstrating that a leaf node is part of a given binary hash tree requires computing a number of hashes proportional to the logarithm of the number of leaf nodes.

2. **Data Integrity**: Any change to a leaf node propagates up the tree, changing the root hash.

3. **Cryptographic Commitment**: The root of the tree serves as a commitment, and leaf nodes may be revealed and proven to be part of the original commitment.
