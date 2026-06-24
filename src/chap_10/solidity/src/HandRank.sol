// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

/// @title HandRank
/// @notice Evaluates the best 5-card Texas Hold'em hand out of 7 cards and
///         returns a single comparable score: a higher score always beats a
///         lower one, so the showdown is just `scoreA > scoreB`.
/// @dev Cards use the same encoding as the Shuffle library: a card index 0..51
///      decodes to rank = index % 13 (0..12 for 2,3,...,10,J,Q,K,A) and
///      suit = index / 13 (0..3). The five community cards plus two hole cards
///      give the classic 7-choose-5 problem; we evaluate all 21 combinations
///      and keep the best.
library HandRank {
    // Hand categories, ordered weakest to strongest. The category occupies the
    // top of the score so it dominates any tie-break below it.
    uint256 internal constant HIGH_CARD = 0;
    uint256 internal constant PAIR = 1;
    uint256 internal constant TWO_PAIR = 2;
    uint256 internal constant TRIPS = 3;
    uint256 internal constant STRAIGHT = 4;
    uint256 internal constant FLUSH = 5;
    uint256 internal constant FULL_HOUSE = 6;
    uint256 internal constant QUADS = 7;
    uint256 internal constant STRAIGHT_FLUSH = 8;

    /// @notice Score the best 5-card hand from seven card indices.
    /// @param cards Seven card indices in 0..51 (2 hole + 5 board).
    /// @return best The highest score over all 21 five-card subsets.
    function best7(uint8[7] memory cards) internal pure returns (uint256 best) {
        // Walk every 5-card subset by choosing the two cards to LEAVE OUT.
        for (uint8 a = 0; a < 7; a++) {
            for (uint8 b = a + 1; b < 7; b++) {
                uint8[5] memory hand;
                uint8 k = 0;
                for (uint8 i = 0; i < 7; i++) {
                    if (i != a && i != b) {
                        hand[k++] = cards[i];
                    }
                }
                uint256 s = score5(hand);
                if (s > best) {
                    best = s;
                }
            }
        }
    }

    /// @notice Score a single 5-card hand into one comparable integer.
    /// @dev The score is packed as: category in the high bits, then up to five
    ///      rank "kickers" in descending importance, each in a 4-bit nibble.
    ///      Layout: [category][k1][k2][k3][k4][k5], 4 bits per kicker.
    function score5(uint8[5] memory hand) internal pure returns (uint256) {
        // Count how many cards carry each rank (0..12) and each suit (0..3).
        uint8[13] memory rankCount;
        uint8[4] memory suitCount;
        for (uint8 i = 0; i < 5; i++) {
            rankCount[hand[i] % 13]++;
            suitCount[hand[i] / 13]++;
        }

        bool flush = false;
        for (uint8 s = 0; s < 4; s++) {
            if (suitCount[s] == 5) {
                flush = true;
            }
        }

        // Find the high card of a straight (returns rank+1 of the top card, or 0
        // if there is no straight). Handles the wheel A-2-3-4-5.
        uint8 straightHigh = _straightHigh(rankCount);

        if (straightHigh > 0 && flush) {
            return _pack(STRAIGHT_FLUSH, straightHigh, 0, 0, 0, 0);
        }

        // Gather ranks grouped by how many times they appear, high rank first.
        // quad/trip/pair hold the rank (1..13, i.e. stored rank+1 so 0 means
        // "none"); kickers collects the leftover singles, high to low.
        uint8 quad;
        uint8 trip;
        uint8 pairHi;
        uint8 pairLo;
        uint8[5] memory kickers;
        uint8 kn;
        // Walk ranks from Ace (12) down so higher ranks land first.
        for (uint8 r = 13; r > 0; r--) {
            uint8 rank = r - 1;
            uint8 c = rankCount[rank];
            if (c == 4) {
                quad = rank + 1;
            } else if (c == 3) {
                trip = rank + 1;
            } else if (c == 2) {
                if (pairHi == 0) {
                    pairHi = rank + 1;
                } else {
                    pairLo = rank + 1;
                }
            } else if (c == 1) {
                kickers[kn++] = rank + 1;
            }
        }

        if (quad > 0) {
            return _pack(QUADS, quad, kickers[0], 0, 0, 0);
        }
        if (trip > 0 && pairHi > 0) {
            return _pack(FULL_HOUSE, trip, pairHi, 0, 0, 0);
        }
        if (flush) {
            return _pack(FLUSH, kickers[0], kickers[1], kickers[2], kickers[3], kickers[4]);
        }
        if (straightHigh > 0) {
            return _pack(STRAIGHT, straightHigh, 0, 0, 0, 0);
        }
        if (trip > 0) {
            return _pack(TRIPS, trip, kickers[0], kickers[1], 0, 0);
        }
        if (pairHi > 0 && pairLo > 0) {
            return _pack(TWO_PAIR, pairHi, pairLo, kickers[0], 0, 0);
        }
        if (pairHi > 0) {
            return _pack(PAIR, pairHi, kickers[0], kickers[1], kickers[2], 0);
        }
        return _pack(HIGH_CARD, kickers[0], kickers[1], kickers[2], kickers[3], kickers[4]);
    }

    /// @dev Return the high card of a 5-card straight as rank+1 (1..13), or 0 if
    ///      the ranks don't form a straight. The Ace plays high (T-J-Q-K-A) and
    ///      low (A-2-3-4-5, the "wheel"), where the wheel's high card is the 5.
    function _straightHigh(uint8[13] memory rankCount) private pure returns (uint8) {
        // Five consecutive ranks each present exactly once. Scan windows whose
        // top card runs from Ace (12) down to 6 (the 6-high straight 2-3-4-5-6).
        for (uint8 top = 12; top >= 4; top--) {
            bool run = true;
            for (uint8 d = 0; d < 5; d++) {
                if (rankCount[top - d] == 0) {
                    run = false;
                    break;
                }
            }
            if (run) {
                return top + 1;
            }
        }
        // The wheel: A(12) 2(0) 3(1) 4(2) 5(3). High card is the 5 (rank 3).
        if (
            rankCount[12] > 0 && rankCount[0] > 0 && rankCount[1] > 0 && rankCount[2] > 0
                && rankCount[3] > 0
        ) {
            return 4; // five-high straight, stored as rank 3 + 1
        }
        return 0;
    }

    /// @dev Pack a category and up to five 4-bit kickers into one score.
    function _pack(uint256 cat, uint8 k1, uint8 k2, uint8 k3, uint8 k4, uint8 k5)
        private
        pure
        returns (uint256)
    {
        return (cat << 20) | (uint256(k1) << 16) | (uint256(k2) << 12) | (uint256(k3) << 8)
            | (uint256(k4) << 4) | uint256(k5);
    }
}
