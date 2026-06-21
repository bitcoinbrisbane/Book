// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

/// @title Shuffle
/// @notice A tiny Fisher-Yates shuffle over a standard 52-card deck.
/// @dev TEACHING CODE. The shuffle is only as secret as the `seed`. If the seed
///      is derived from on-chain data (block.timestamp, blockhash, prevrandao),
///      it is PUBLIC and PREDICTABLE, and the resulting deck order can be
///      computed by anyone. A secure deck needs the seed to come from somewhere
///      the players cannot see or predict (see the "hiding the deck" page).
library Shuffle {
    uint8 internal constant DECK_SIZE = 52;

    /// @notice Return a freshly-shuffled deck: cards 0..51 in permuted order.
    /// @param seed The randomness driving the permutation.
    function shuffledDeck(uint256 seed) internal pure returns (uint8[52] memory deck) {
        // Start with the identity deck 0,1,2,...,51.
        for (uint8 i = 0; i < DECK_SIZE; i++) {
            deck[i] = i;
        }

        // Fisher-Yates: walk from the last index down, swapping each card with
        // a randomly-chosen card at or before it.
        for (uint8 i = DECK_SIZE - 1; i > 0; i--) {
            // Derive a fresh pseudo-random value for each step from the seed
            // and the index, so we don't reuse the same number 51 times.
            uint256 r = uint256(keccak256(abi.encode(seed, i)));
            uint8 j = uint8(r % (uint256(i) + 1)); // 0 <= j <= i

            (deck[i], deck[j]) = (deck[j], deck[i]);
        }
    }

    /// @notice Decode a card index (0..51) into its rank and suit.
    /// @return rank 0..12  (2,3,...,10,J,Q,K,A)
    /// @return suit 0..3   (clubs, diamonds, hearts, spades)
    function card(uint8 index) internal pure returns (uint8 rank, uint8 suit) {
        require(index < DECK_SIZE, "card out of range");
        rank = index % 13;
        suit = index / 13;
    }
}
