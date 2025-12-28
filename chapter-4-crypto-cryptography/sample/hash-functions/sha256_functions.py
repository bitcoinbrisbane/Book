"""
SHA-256 Choosing (Ch) and Majority (Ma) Functions

These functions are core components of the SHA-256 compression function,
used in each of the 64 rounds of processing.
"""


def ch(x: int, y: int, z: int) -> int:
    """
    Choosing function (Ch) - "Choose" between y and z based on x.

    For each bit position:
    - If the bit in x is 1, take the bit from y
    - If the bit in x is 0, take the bit from z

    Formally: Ch(x, y, z) = (x AND y) XOR (NOT x AND z)

    Args:
        x, y, z: 32-bit integers

    Returns:
        32-bit integer result
    """
    return (x & y) ^ (~x & z)


def ma(x: int, y: int, z: int) -> int:
    """
    Majority function (Ma) - Returns the "majority" vote of three bits.

    For each bit position:
    - If at least 2 of the 3 bits are 1, the result is 1
    - Otherwise, the result is 0

    Formally: Ma(x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z)

    Args:
        x, y, z: 32-bit integers

    Returns:
        32-bit integer result
    """
    return (x & y) ^ (x & z) ^ (y & z)


def mask_32bit(n: int) -> int:
    """Mask to ensure 32-bit result (handle Python's arbitrary precision)."""
    return n & 0xFFFFFFFF


def demo():
    """Demonstrate the Ch and Ma functions with examples."""

    # Example with simple binary values for clarity
    # Using 8-bit examples for readability, same logic applies to 32-bit
    x = 0b11110000
    y = 0b11001100
    z = 0b10101010

    print("Choosing (Ch) and Majority (Ma) Functions Demo")
    print("=" * 50)
    print(f"\nInputs (8-bit for readability):")
    print(f"  x = {x:08b} ({x})")
    print(f"  y = {y:08b} ({y})")
    print(f"  z = {z:08b} ({z})")

    # Choosing function
    ch_result = ch(x, y, z) & 0xFF  # Mask to 8 bits for display
    print(f"\nChoosing Ch(x, y, z):")
    print(f"  Where x=1, take y; where x=0, take z")
    print(f"  Result = {ch_result:08b} ({ch_result})")

    # Step by step for Ch
    print(f"\n  Step by step:")
    print(f"    x AND y     = {(x & y):08b}")
    print(f"    NOT x       = {(~x & 0xFF):08b}")
    print(f"    NOT x AND z = {(~x & z & 0xFF):08b}")
    print(f"    XOR result  = {ch_result:08b}")

    # Majority function
    ma_result = ma(x, y, z) & 0xFF  # Mask to 8 bits for display
    print(f"\nMajority Ma(x, y, z):")
    print(f"  For each bit, return 1 if majority of inputs are 1")
    print(f"  Result = {ma_result:08b} ({ma_result})")

    # Step by step for Ma
    print(f"\n  Step by step:")
    print(f"    x AND y = {(x & y):08b}")
    print(f"    x AND z = {(x & z):08b}")
    print(f"    y AND z = {(y & z):08b}")
    print(f"    XOR all = {ma_result:08b}")

    # Now with actual SHA-256 style 32-bit values
    print("\n" + "=" * 50)
    print("Example with SHA-256 initial hash values:")

    h0 = 0x6a09e667
    h1 = 0xbb67ae85
    h2 = 0x3c6ef372

    ch_32 = mask_32bit(ch(h0, h1, h2))
    ma_32 = mask_32bit(ma(h0, h1, h2))

    print(f"\n  H0 = 0x{h0:08x}")
    print(f"  H1 = 0x{h1:08x}")
    print(f"  H2 = 0x{h2:08x}")
    print(f"\n  Ch(H0, H1, H2) = 0x{ch_32:08x}")
    print(f"  Ma(H0, H1, H2) = 0x{ma_32:08x}")


if __name__ == "__main__":
    demo()
