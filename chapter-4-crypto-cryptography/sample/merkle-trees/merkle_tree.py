"""
Merkle Tree Implementation

A Merkle tree is a binary tree where each leaf node contains the hash of a data block,
and each non-leaf node contains the hash of its children. This allows efficient and
secure verification of data contents.
"""

import hashlib
from typing import List, Optional, Tuple


def sha256(data: str) -> str:
    """Compute SHA-256 hash of a string and return hex digest."""
    return hashlib.sha256(data.encode('utf-8')).hexdigest()


def hash_pair(left: str, right: str) -> str:
    """Hash two hex strings together."""
    return sha256(left + right)


class MerkleTree:
    """A simple Merkle Tree implementation."""

    def __init__(self, leaves: List[str]):
        """
        Build a Merkle tree from a list of leaf values.

        Args:
            leaves: List of string values to hash as leaves
        """
        self.leaves = leaves
        self.leaf_hashes = [sha256(leaf) for leaf in leaves]
        self.tree = self._build_tree(self.leaf_hashes)
        self.root = self.tree[-1][0] if self.tree else None

    def _build_tree(self, leaf_hashes: List[str]) -> List[List[str]]:
        """
        Build the tree level by level from leaves to root.

        Returns:
            List of levels, where each level is a list of hashes
        """
        if not leaf_hashes:
            return []

        tree = [leaf_hashes]
        current_level = leaf_hashes

        while len(current_level) > 1:
            next_level = []

            # Process pairs of nodes
            for i in range(0, len(current_level), 2):
                left = current_level[i]
                # If odd number of nodes, duplicate the last one
                right = current_level[i + 1] if i + 1 < len(current_level) else left
                next_level.append(hash_pair(left, right))

            tree.append(next_level)
            current_level = next_level

        return tree

    def get_proof(self, index: int) -> List[Tuple[str, str]]:
        """
        Generate a Merkle proof for a leaf at the given index.

        Args:
            index: Index of the leaf to prove

        Returns:
            List of (hash, direction) tuples where direction is 'left' or 'right'
        """
        if index < 0 or index >= len(self.leaves):
            raise ValueError(f"Index {index} out of range")

        proof = []
        current_index = index

        for level in self.tree[:-1]:  # All levels except root
            # Determine sibling index
            if current_index % 2 == 0:
                sibling_index = current_index + 1
                direction = 'right'
            else:
                sibling_index = current_index - 1
                direction = 'left'

            # Handle edge case where sibling doesn't exist (odd number of nodes)
            if sibling_index < len(level):
                proof.append((level[sibling_index], direction))
            else:
                proof.append((level[current_index], direction))

            # Move to parent index
            current_index = current_index // 2

        return proof

    @staticmethod
    def verify_proof(leaf_value: str, proof: List[Tuple[str, str]], root: str) -> bool:
        """
        Verify a Merkle proof.

        Args:
            leaf_value: The original leaf value (not hashed)
            proof: List of (hash, direction) tuples
            root: Expected Merkle root

        Returns:
            True if the proof is valid
        """
        current_hash = sha256(leaf_value)

        for sibling_hash, direction in proof:
            if direction == 'left':
                current_hash = hash_pair(sibling_hash, current_hash)
            else:
                current_hash = hash_pair(current_hash, sibling_hash)

        return current_hash == root

    def print_tree(self):
        """Print a visual representation of the tree."""
        print("\nMerkle Tree Structure:")
        print("=" * 70)

        for i, level in enumerate(reversed(self.tree)):
            level_name = "Root" if i == 0 else f"Level {len(self.tree) - i - 1}"
            print(f"\n{level_name}:")
            for j, h in enumerate(level):
                # Show truncated hash for readability
                print(f"  [{j}] {h[:16]}...{h[-8:]}")


def demo_basic():
    """Basic demonstration with card values."""
    print("=" * 70)
    print("MERKLE TREE DEMO - Playing Cards")
    print("=" * 70)

    # Create a Merkle tree with playing cards
    cards = [
        "ACE OF SPADES",
        "KING OF SPADES",
        "QUEEN OF SPADES",
        "JACK OF SPADES"
    ]

    print("\nLeaf values (cards):")
    for i, card in enumerate(cards):
        print(f"  L{i}: {card}")

    # Build tree
    tree = MerkleTree(cards)

    print("\nLeaf hashes:")
    for i, h in enumerate(tree.leaf_hashes):
        print(f"  L{i}: {h}")

    tree.print_tree()

    print(f"\nMerkle Root: {tree.root}")


def demo_proof():
    """Demonstrate Merkle proof generation and verification."""
    print("\n" + "=" * 70)
    print("MERKLE PROOF DEMO")
    print("=" * 70)

    cards = [
        "ACE OF SPADES",
        "KING OF SPADES",
        "QUEEN OF SPADES",
        "JACK OF SPADES"
    ]

    tree = MerkleTree(cards)

    # Generate proof for "KING OF SPADES" (index 1)
    target_index = 1
    target_card = cards[target_index]

    print(f"\nProving membership of: '{target_card}' (index {target_index})")

    proof = tree.get_proof(target_index)

    print("\nMerkle Proof:")
    for i, (h, direction) in enumerate(proof):
        print(f"  Step {i + 1}: {h[:16]}... ({direction})")

    # Verify the proof
    is_valid = MerkleTree.verify_proof(target_card, proof, tree.root)
    print(f"\nProof valid: {is_valid}")

    # Try to verify with wrong value
    wrong_card = "TWO OF HEARTS"
    is_valid_wrong = MerkleTree.verify_proof(wrong_card, proof, tree.root)
    print(f"Proof valid for '{wrong_card}': {is_valid_wrong}")


def demo_tamper_detection():
    """Demonstrate how Merkle trees detect tampering."""
    print("\n" + "=" * 70)
    print("TAMPER DETECTION DEMO")
    print("=" * 70)

    original_cards = ["ACE", "KING", "QUEEN", "JACK"]
    tampered_cards = ["ACE", "KING", "QUEEN", "JOKER"]  # JACK -> JOKER

    original_tree = MerkleTree(original_cards)
    tampered_tree = MerkleTree(tampered_cards)

    print("\nOriginal cards:", original_cards)
    print(f"Original root:  {original_tree.root}")

    print("\nTampered cards:", tampered_cards)
    print(f"Tampered root:  {tampered_tree.root}")

    print(f"\nRoots match: {original_tree.root == tampered_tree.root}")
    print("(Even a single character change produces a completely different root!)")


def demo_two_cards():
    """Demo matching the book's example with two cards."""
    print("\n" + "=" * 70)
    print("TWO-CARD EXAMPLE (from book)")
    print("=" * 70)

    cards = ["ACE OF SPADES", "KING OF SPADES"]

    print(f"\nL1 = '{cards[0]}'")
    print(f"L2 = '{cards[1]}'")

    h1 = sha256(cards[0])
    h2 = sha256(cards[1])

    print(f"\nhash(L1) = {h1}")
    print(f"hash(L2) = {h2}")

    root = hash_pair(h1, h2)
    print(f"\nhash(L1 + L2) = {root}")

    # Verify with MerkleTree class
    tree = MerkleTree(cards)
    print(f"\nMerkleTree root: {tree.root}")
    print(f"Roots match: {root == tree.root}")


if __name__ == "__main__":
    demo_two_cards()
    demo_basic()
    demo_proof()
    demo_tamper_detection()
