#!/usr/bin/env python3
"""
Mersenne Twister Random Number Generation Example
Python's random module uses the Mersenne Twister algorithm by default.
"""

import random

def main():
    print("Mersenne Twister Random Number Generation")
    print("==========================================\n")

    # Generate some random numbers with current time as seed
    print("Random integers (1-100):")
    for i in range(5):
        print(f"  {i+1}: {random.randint(1, 100)}")

    print("\nRandom floats (0.0-1.0):")
    for i in range(5):
        print(f"  {i+1}: {random.random():.6f}")

    # Demonstrate reproducibility with same seed
    print("\nDemonstrating reproducibility with seed=42:")
    random.seed(42)
    print("First run:")
    for i in range(3):
        print(f"  {random.randint(1, 1000)}")

    random.seed(42)
    print("Second run (same seed):")
    for i in range(3):
        print(f"  {random.randint(1, 1000)}")

    # Show different random methods
    print("\nOther random functions:")
    random.seed()  # Reset seed
    colors = ['red', 'blue', 'green', 'yellow', 'purple']
    print(f"  Random choice from list: {random.choice(colors)}")
    print(f"  Random sample of 3: {random.sample(colors, 3)}")
    print(f"  Gaussian distribution (μ=0, σ=1): {random.gauss(0, 1):.4f}")

    print("\n⚠️  WARNING: This is NOT cryptographically secure!")
    print("For cryptographic use, use the secrets module instead:")
    print("  import secrets")
    print("  token = secrets.token_hex(32)")
    print("  secure_number = secrets.randbelow(100)")

if __name__ == "__main__":
    main()
