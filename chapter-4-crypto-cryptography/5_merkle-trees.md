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

Let's say we want to know that our bitcoin exchange is solvent and won't misappropriate funds like the FTX exchange did. We can use Merkle trees to create a cryptographic proof of reserves.

#### How It Works

The exchange creates a Merkle tree where each leaf contains a customer's bitcoin address and their balance. The exchange then publishes only the merkle root (a 32-byte hash) which commits to all customer balances without revealing:
- Individual customer balances
- Total number of customers
- Total bitcoin in custody

Each customer can independently verify their balance is included by requesting a merkle proof from the exchange and checking it against the published root.

#### Worked Example

Let's walk through a concrete example with an exchange that has 4 customers:

```python
# Customer data: (address, balance in BTC)
customers = [
    ("bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh", 2.5),    # Alice
    ("bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4", 1.75),   # Bob
    ("bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3", 5.0),    # Carol
    ("bc1qar0srrr7xfkvy5l643lydnw9re59gtzzwf5mdq", 0.5)  # Dave
]

# Create leaf nodes by hashing address + balance
def create_leaf(address: str, balance: float) -> str:
    """Create a leaf node from address and balance."""
    leaf_data = f"{address}:{balance}"
    return sha256(leaf_data)

# Build the Merkle tree
leaves = [create_leaf(addr, bal) for addr, bal in customers]
tree = MerkleTree(leaves)

print(f"Published Merkle Root: {tree.root}")
# The exchange publishes this root hash on their website or blockchain
```

#### Verifying Your Balance

Now let's say you're Bob, and you want to verify your balance of 1.75 BTC is actually in the exchange's reserves:

```python
# Bob's information
bob_address = "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
bob_balance = 1.75
bob_index = 1  # Bob is the second customer (index 1)

# Bob creates his leaf hash
bob_leaf = create_leaf(bob_address, bob_balance)
print(f"Bob's leaf hash: {bob_leaf}")

# Bob requests a merkle proof from the exchange
# The exchange provides only the sibling hashes needed to reconstruct the root
bob_proof = get_proof(tree, bob_index)
print(f"\nMerkle proof for Bob (only {len(bob_proof)} hashes):")
for i, (hash_val, direction) in enumerate(bob_proof):
    print(f"  Level {i}: {hash_val[:16]}... (position: {direction})")

# Bob verifies the proof against the published root
bob_leaf_data = f"{bob_address}:{bob_balance}"
is_valid = verify_proof(bob_leaf_data, bob_proof, tree.root)
print(f"\nBob's balance verified: {is_valid}")
```

#### What Bob Learns

When Bob verifies his proof, he learns:
- ✅ His balance of 1.75 BTC is committed to in the published merkle root
- ✅ The exchange cannot change his balance without changing the root

#### What Bob Does NOT Learn

The merkle proof reveals nothing about:
- ❌ Alice's balance (2.5 BTC)
- ❌ Carol's balance (5.0 BTC)
- ❌ Dave's balance (0.5 BTC)
- ❌ The total reserves (9.75 BTC)
- ❌ How many other customers exist

Bob only receives the sibling hashes in his proof path, which are cryptographically derived but don't reveal the underlying customer data.

#### Attempting Fraud

If the exchange tries to lie about Bob's balance:

```python
# Exchange tries to claim Bob has less
fake_balance = 1.0  # Instead of actual 1.75 BTC
fake_leaf_data = f"{bob_address}:{fake_balance}"

# Bob verifies with the fake balance
is_valid = verify_proof(fake_leaf_data, bob_proof, tree.root)
print(f"Fake balance verified: {is_valid}")  # False!
```

The verification fails because the fake balance produces a different leaf hash, which won't match the committed merkle root.

#### Collective Verification

While individual users can only verify their own balances, if many random users all verify successfully, we gain statistical confidence that:
1. The exchange is honestly reporting customer balances
2. The merkle root represents actual customer data
3. Combined with on-chain proof that the exchange controls the Bitcoin addresses, we can verify solvency

### Ethereum Whitelist Application

Another powerful application of Merkle trees is managing whitelists in Ethereum smart contracts. Instead of storing thousands of addresses on-chain (which would be extremely expensive), we can store just the merkle root and let users prove they're on the whitelist.

#### Use Case: NFT Whitelist Mint

Imagine you're launching an NFT collection and want to allow 10,000 whitelisted addresses to mint before the public sale. Storing 10,000 addresses in a smart contract would cost tens of thousands of dollars in gas fees. With a Merkle tree, you only store one 32-byte hash!

#### Solidity Smart Contract

First, let's create a smart contract using OpenZeppelin's MerkleProof library:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "@openzeppelin/contracts/utils/cryptography/MerkleProof.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

/**
 * @title WhitelistNFT
 * @dev NFT contract with Merkle tree whitelist for presale
 */
contract WhitelistNFT is Ownable {
    // The merkle root of all whitelisted addresses
    bytes32 public merkleRoot;

    // Track which addresses have already minted
    mapping(address => bool) public hasMinted;

    // Events
    event WhitelistMint(address indexed user, uint256 tokenId);
    event MerkleRootUpdated(bytes32 newRoot);

    constructor(bytes32 _merkleRoot) Ownable(msg.sender) {
        merkleRoot = _merkleRoot;
    }

    /**
     * @dev Update the merkle root (only owner)
     * @param _newRoot The new merkle root
     */
    function setMerkleRoot(bytes32 _newRoot) external onlyOwner {
        merkleRoot = _newRoot;
        emit MerkleRootUpdated(_newRoot);
    }

    /**
     * @dev Mint function for whitelisted users
     * @param proof The merkle proof that the caller is whitelisted
     */
    function whitelistMint(bytes32[] calldata proof) external {
        require(!hasMinted[msg.sender], "Already minted");

        // Create leaf node from sender's address
        bytes32 leaf = keccak256(abi.encodePacked(msg.sender));

        // Verify the merkle proof
        require(
            MerkleProof.verify(proof, merkleRoot, leaf),
            "Invalid merkle proof - not whitelisted"
        );

        // Mark as minted
        hasMinted[msg.sender] = true;

        // Mint NFT logic here...
        emit WhitelistMint(msg.sender, 1); // tokenId would be dynamic
    }

    /**
     * @dev Check if an address is whitelisted
     * @param account The address to check
     * @param proof The merkle proof
     */
    function isWhitelisted(address account, bytes32[] calldata proof)
        external
        view
        returns (bool)
    {
        bytes32 leaf = keccak256(abi.encodePacked(account));
        return MerkleProof.verify(proof, merkleRoot, leaf);
    }
}
```

#### OpenZeppelin MerkleProof Library

For clarity, here's the core of OpenZeppelin's MerkleProof library that the contract uses:

```solidity
// Simplified version of OpenZeppelin's MerkleProof.sol
library MerkleProof {
    /**
     * @dev Returns true if a `leaf` can be proved to be part of a Merkle tree
     * defined by `root`. For this, a `proof` must be provided, containing
     * sibling hashes on the branch from the leaf to the root.
     */
    function verify(
        bytes32[] memory proof,
        bytes32 root,
        bytes32 leaf
    ) internal pure returns (bool) {
        return processProof(proof, leaf) == root;
    }

    /**
     * @dev Returns the rebuilt hash by traversing a Merkle tree up from `leaf`
     * using `proof`. A `proof` is valid if the final hash equals `root`.
     */
    function processProof(
        bytes32[] memory proof,
        bytes32 leaf
    ) internal pure returns (bytes32) {
        bytes32 computedHash = leaf;
        for (uint256 i = 0; i < proof.length; i++) {
            computedHash = _hashPair(computedHash, proof[i]);
        }
        return computedHash;
    }

    /**
     * @dev Sorts the pair and hashes them together
     */
    function _hashPair(bytes32 a, bytes32 b) private pure returns (bytes32) {
        return a < b
            ? keccak256(abi.encodePacked(a, b))
            : keccak256(abi.encodePacked(b, a));
    }
}
```

#### Node.js Application

Now let's create a Node.js application to generate the Merkle tree and proofs:

```javascript
// merkle-whitelist.js
const { MerkleTree } = require("merkletreejs");
const keccak256 = require("keccak256");
const { ethers } = require("ethers");

/**
 * Generate a Merkle tree from a list of whitelisted addresses
 */
class WhitelistMerkleTree {
    constructor(addresses) {
        // Convert addresses to leaf nodes (hash each address)
        this.leaves = addresses.map((addr) =>
            keccak256(ethers.solidityPacked(["address"], [addr]))
        );

        // Create the Merkle tree
        this.tree = new MerkleTree(this.leaves, keccak256, {
            sortPairs: true // Important! Matches OpenZeppelin's behavior
        });

        this.addresses = addresses;
    }

    /**
     * Get the Merkle root (to store in smart contract)
     */
    getRoot = () => {
        return this.tree.getHexRoot();
    }

    /**
     * Get the Merkle proof for a specific address
     */
    getProof = (address) => {
        const leaf = keccak256(
            ethers.solidityPacked(["address"], [address])
        );
        return this.tree.getHexProof(leaf);
    }

    /**
     * Verify a proof (for testing)
     */
    verify = (address, proof) => {
        const leaf = keccak256(
            ethers.solidityPacked(["address"], [address])
        );
        const root = this.tree.getRoot();
        return this.tree.verify(proof, leaf, root);
    }

    /**
     * Generate a JSON file with all proofs
     */
    generateProofFile = () => {
        const proofs = {};

        this.addresses.forEach((address) => {
            proofs[address] = this.getProof(address);
        });

        return {
            merkleRoot: this.getRoot(),
            proofs: proofs
        };
    }
}

// Example usage
const main = async () => {
    // Whitelist of addresses (in practice, this could be 10,000+ addresses)
    const whitelist = [
        "0x5B38Da6a701c568545dCfcB03FcB875f56beddC4", // Alice
        "0xAb8483F64d9C6d1EcF9b849Ae677dD3315835cb2", // Bob
        "0x4B20993Bc481177ec7E8f571ceCaE8A9e22C02db", // Carol
        "0x78731D3Ca6b7E34aC0F824c42a7cC18A495cabaB", // Dave
        "0x617F2E2fD72FD9D5503197092aC168c91465E7f2", // Eve
    ];

    console.log(`Creating Merkle tree for ${whitelist.length} addresses...\n`);

    // Create the Merkle tree
    const merkleTree = new WhitelistMerkleTree(whitelist);

    // Get the root to store in the smart contract
    const root = merkleTree.getRoot();
    console.log("Merkle Root (store this in smart contract):");
    console.log(root);
    console.log();

    // Generate proof for Bob
    const bobAddress = "0xAb8483F64d9C6d1EcF9b849Ae677dD3315835cb2";
    const bobProof = merkleTree.getProof(bobAddress);

    console.log(`Merkle Proof for ${bobAddress}:`);
    console.log(bobProof);
    console.log();

    // Verify the proof
    const isValid = merkleTree.verify(bobAddress, bobProof);
    console.log(`Proof valid: ${isValid}`);
    console.log();

    // Try with an address NOT on the whitelist
    const hackerAddress = "0x0000000000000000000000000000000000000000";
    const hackerProof = merkleTree.getProof(hackerAddress);
    const hackerValid = merkleTree.verify(hackerAddress, hackerProof);
    console.log(`Hacker proof valid: ${hackerValid}`);
    console.log();

    // Generate proofs for all addresses
    const proofsJson = merkleTree.generateProofFile();
    console.log("Generated proofs for all whitelisted addresses");
    console.log(`Number of proofs: ${Object.keys(proofsJson.proofs).length}`);

    // In production, save this to a file:
    // const fs = require("fs");
    // fs.writeFileSync("whitelist-proofs.json", JSON.stringify(proofsJson, null, 2));
};

main().catch(console.error);
```

#### Installing Dependencies

```bash
yarn add merkletreejs keccak256 ethers
```

#### Complete Frontend Integration

Here's how a user would interact with the contract from a web application:

```javascript
// frontend-integration.js
import { ethers } from "ethers";
import whitelistProofs from "./whitelist-proofs.json";

const mintWhitelistNFT = async () => {
    // Connect to MetaMask
    const provider = new ethers.BrowserProvider(window.ethereum);
    await provider.send("eth_requestAccounts", []);
    const signer = await provider.getSigner();
    const userAddress = await signer.getAddress();

    // Load contract
    const contractAddress = "0x..."; // Your deployed contract
    const contract = new ethers.Contract(
        contractAddress,
        ["function whitelistMint(bytes32[] calldata proof)"],
        signer
    );

    // Get user's proof from the JSON file
    const proof = whitelistProofs.proofs[userAddress];

    if (!proof) {
        alert("You are not whitelisted!");
        return;
    }

    try {
        console.log("Minting with proof:", proof);
        const tx = await contract.whitelistMint(proof);
        console.log("Transaction sent:", tx.hash);

        await tx.wait();
        console.log("Successfully minted!");
    } catch (error) {
        console.error("Mint failed:", error);
    }
};
```

#### Gas Savings Comparison

Let's compare the gas costs:

**Traditional Approach (storing addresses on-chain):**
- Storing 10,000 addresses: ~20,000 gas per address = ~200M gas
- At 50 gwei and $3,000 ETH: ~$30,000+ just to store the whitelist!

**Merkle Tree Approach:**
- Storing the merkle root: ~20,000 gas (one-time)
- Each user's mint verification: ~5,000-10,000 gas
- At 50 gwei and $3,000 ETH: ~$3 to store, users pay their own verification costs

The savings are enormous! This is why Merkle trees are the standard for NFT whitelists, airdrops, and any scenario where you need to verify membership in a large set.

## Key Benefits

1. **Efficient Verification**: Demonstrating that a leaf node is part of a given binary hash tree requires computing a number of hashes proportional to the logarithm of the number of leaf nodes.

2. **Data Integrity**: Any change to a leaf node propagates up the tree, changing the root hash.

3. **Cryptographic Commitment**: The root of the tree serves as a commitment, and leaf nodes may be revealed and proven to be part of the original commitment.
