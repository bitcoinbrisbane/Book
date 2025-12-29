# Hash Functions

Hash functions are "one way" algorithms that take an input (or 'message') and return a fixed-size string of bytes. The output, typically called the hash value or digest, appears random and unique for each unique input. Reversing the data is not possible. However, as mentioned in the chapter before, changing even one byte or character, results in a completely random output.

Here are some more properties of the hash function:

1. **Deterministic**: The same input will always produce the same output hash value.
2. **Fixed Output Length**: Regardless of the input size, the output hash is of a fixed length (e.g., 256 bits for SHA-256).
3. **Efficient**: Hash functions can process input data quickly.
4. **Pre-image Resistance**: It should be infeasible to generate the original input data given only the hash value.
5. **Collision Resistance**: It should be infeasible to find two different inputs that produce the same hash value.
6. **Avalanche Effect**: A small change in the input should produce a significantly different hash value.

There are different types of Hash algorithms, but we will focus on the Secure Hash Algorithm 2, which includes SHA-256 and KECCAK-256.

Note: The security strengths of the functions are defined at https://csrc.nist.gov/projects/hash-functions

## SHA-256 Examples

```
sha256("Hello, World!") = dffd6021bb2bd5b0af676290809ec3a53191dd81c7f70a4b28688a362182986f
```

Note, the output of the function is in hexadecimal (base16) so it's case insensitive.

Remove the comma and we get the following output:

```
sha256("Hello World!") = 7f83b1657ff1fc53b92dc18148a1d65dfc2d4b1fa3d677284addd200126d9069
```

Hash functions are used to store passwords in a database, so that they're not stored in plain text. An attacker or even an inside employee, should not be able to look up a user's password and login to the system. We will discuss Rainbow table attacks later.

## How Do Hash Functions Work?

So, how do hash functions actually work and why can't they be reversed? You'll find many bad examples on the internet of an implementation of a hash function, or just using libs or native functions in the languages. If you're not interested in the algorithm, feel free to skip this part, but for me I had to know. It's critical to bitcoin mining, so I love to understand the algorithm in full.

First we start with two sets of magic numbers. The initial hash values in hexadecimal, we will call H0 to H7 which are derived from the first 32 bytes of the fractional parts of the square roots of the first 8 primes resulting in irrational numbers: 2, 3, 5, 7, 11, 13, 17, 19

```
H0 = 0x6a09e667
H1 = 0xbb67ae85
H2 = 0x3c6ef372
H3 = 0xa54ff53a
H4 = 0x510e527f
H5 = 0x9b05688c
H6 = 0x1f83d9ab
H7 = 0x5be0cd19
```

sqrt(2) = 1.4142135623730951 thus the fractional part to eight decimals is 0.4142135623730951. Now we convert that number to binary.

```
bin(0.4142135623730951) = 01101010000010011110011001100111
```

And convert to hexadecimal:

```
hex(01101010000010011110011001100111) = 0x6a09e667
```

The next magic numbers are sixty-four 32-bit words derived from the fractional parts of the cube roots of the first 64 prime numbers (2 ... 311). We will call these K0 ... K63.

The algorithm also uses the Choosing function (Ch) and the Majority function (Ma), which operate on the working variables during each round of compression.

### The Choosing Function (Ch)

The Choosing function "chooses" between two values based on a third. For each bit position:
- If the bit in x is 1, take the bit from y
- If the bit in x is 0, take the bit from z

Formally defined as:

```
Ch(x, y, z) = (x AND y) XOR (NOT x AND z)
```

### The Majority Function (Ma)

The Majority function returns the "majority vote" of three bits. For each bit position:
- If at least 2 of the 3 bits are 1, the result is 1
- Otherwise, the result is 0

Formally defined as:

```
Ma(x, y, z) = (x AND y) XOR (x AND z) XOR (y AND z)
```

### Python Implementation

```python
def ch(x: int, y: int, z: int) -> int:
    """
    Choosing function (Ch) - "Choose" between y and z based on x.

    For each bit position:
    - If the bit in x is 1, take the bit from y
    - If the bit in x is 0, take the bit from z
    """
    return (x & y) ^ (~x & z)


def ma(x: int, y: int, z: int) -> int:
    """
    Majority function (Ma) - Returns the "majority" vote of three bits.

    For each bit position:
    - If at least 2 of the 3 bits are 1, the result is 1
    - Otherwise, the result is 0
    """
    return (x & y) ^ (x & z) ^ (y & z)


# Example with 8-bit values for readability
x = 0b11110000
y = 0b11001100
z = 0b10101010

print(f"x = {x:08b}")
print(f"y = {y:08b}")
print(f"z = {z:08b}")

ch_result = ch(x, y, z) & 0xFF
ma_result = ma(x, y, z) & 0xFF

print(f"Ch(x,y,z) = {ch_result:08b}")
print(f"Ma(x,y,z) = {ma_result:08b}")
```

Output:
```
x = 11110000
y = 11001100
z = 10101010
Ch(x,y,z) = 11001010
Ma(x,y,z) = 11101000
```

For the Ch result, notice how the first 4 bits come from y (where x=1) and the last 4 bits come from z (where x=0). For the Ma result, each bit is 1 only where at least two of the three inputs have a 1.

## Coinflip Over the Phone?

As a practical application, hash functions can be used for commitment schemes - like fairly flipping a coin over the phone where neither party can cheat.
