"""
Mental poker, the core trick: commutative encryption.

This is a teaching illustration of the SRA scheme (Shamir-Rivest-Adleman, 1979).
Each player has their own secret key, but because encryption *commutes*, the
layers can be peeled off in ANY order. That's what lets a deck be locked by
everyone and then a single card unlocked for one player, without any single
party ever seeing the whole deck.

Run:  python3 mental_poker.py
"""

# A shared prime modulus, agreed by all players up front.
# (Tiny here for readability; a real scheme uses a large safe prime.)
P = 2_147_483_647  # a Mersenne prime, 2**31 - 1


class Player:
    """Each player holds a secret encryption exponent e and its inverse d,
    such that raising to e then to d (mod P) is the identity."""

    def __init__(self, name: str, e: int):
        self.name = name
        self.e = e
        # d is the modular inverse of e modulo (P-1), so (m**e)**d == m (mod P).
        self.d = pow(e, -1, P - 1)

    def encrypt(self, card: int) -> int:
        return pow(card, self.e, P)

    def decrypt(self, card: int) -> int:
        return pow(card, self.d, P)


def demo():
    alice = Player("Alice", e=65_537)
    bob = Player("Bob", e=101)

    card = 42  # think of this as one card's index

    # Alice locks, then Bob locks on top.
    locked = bob.encrypt(alice.encrypt(card))

    # The magic: Alice can remove HER layer first, even though Bob locked last.
    # Because the operations commute, order doesn't matter.
    half = alice.decrypt(locked)   # Bob's lock still on
    final = bob.decrypt(half)      # now fully unlocked

    assert final == card, "commutative decryption failed!"

    # And the reverse order works too:
    other = alice.decrypt(bob.decrypt(locked))
    assert other == card

    print(f"original card:           {card}")
    print(f"locked by both:          {locked}")
    print(f"after Alice unlocks:     {half}")
    print(f"after Bob also unlocks:  {final}")
    print("commutativity holds: peel the layers in any order, same card falls out.")


if __name__ == "__main__":
    demo()
