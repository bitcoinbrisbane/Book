#!/usr/bin/env python3
"""
Demonstrating the Avalanche Effect in Cryptographic Hash Functions
Shows how small input changes create dramatically different hash outputs.
"""

import hashlib

def count_different_bits(hash1: bytes, hash2: bytes) -> int:
    """Count the number of different bits between two byte arrays."""
    different_bits = 0
    for b1, b2 in zip(hash1, hash2):
        xor = b1 ^ b2
        # Count set bits in XOR result
        while xor:
            different_bits += xor & 1
            xor >>= 1
    return different_bits

def demonstrate_avalanche(inputs: list[str], title: str = "Avalanche Effect Demo"):
    """Demonstrate the avalanche effect with a list of inputs."""
    print(f"{title}")
    print("=" * len(title))
    print()

    hashes = []

    for input_str in inputs:
        hash_obj = hashlib.sha256(input_str.encode())
        hash_bytes = hash_obj.digest()
        hash_hex = hash_obj.hexdigest()
        hashes.append(hash_bytes)

        print(f"Input:   \"{input_str}\"")
        print(f"SHA-256: {hash_hex}")
        print()

    # Compare first input with each subsequent input
    if len(inputs) > 1:
        print("Bit Difference Analysis")
        print("=" * 23)
        print()

        total_bits = len(hashes[0]) * 8

        for i in range(1, len(hashes)):
            different_bits = count_different_bits(hashes[0], hashes[i])
            percentage = (different_bits / total_bits) * 100

            print(f"Comparing \"{inputs[0]}\" vs \"{inputs[i]}\":")
            print(f"  Bits changed: {different_bits} out of {total_bits} ({percentage:.2f}%)")
            print()

def main():
    # Example 1: Single character changes
    inputs = [
        "Hello, World!",
        "Hello, World?",  # Changed last character
        "hello, World!",  # Changed first character to lowercase
        "Hello, World!!", # Added one character
    ]

    demonstrate_avalanche(inputs, "Demonstrating the Avalanche Effect with SHA-256")

    # Example 2: Bitcoin-related demonstration
    print("\n" + "=" * 60)
    block_data = [
        "Block: 123456, Nonce: 1000000",
        "Block: 123456, Nonce: 1000001",  # Nonce changed by 1
        "Block: 123457, Nonce: 1000000",  # Block number changed by 1
    ]

    demonstrate_avalanche(block_data, "Bitcoin Block Hash Example")

    # Example 3: Demonstrate with similar passwords
    print("=" * 60)
    passwords = [
        "MyP@ssw0rd123",
        "MyP@ssw0rd124",  # Last digit changed
        "myp@ssw0rd123",  # Case changed
    ]

    demonstrate_avalanche(passwords, "Password Hash Example (DO NOT use plain SHA-256 for passwords!)")

    print("\nKey Takeaway:")
    print("The avalanche effect ensures that even tiny changes in input")
    print("produce completely different hashes, making it impossible to")
    print("predict or reverse-engineer the original data.")
    print("\nNote: For password hashing, use dedicated algorithms like")
    print("bcrypt, scrypt, or Argon2, NOT plain SHA-256!")

if __name__ == "__main__":
    main()
