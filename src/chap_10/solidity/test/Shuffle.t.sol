// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { Test } from "forge-std/Test.sol";
import { Shuffle } from "../src/Shuffle.sol";

contract ShuffleTest is Test {
    function test_isValidPermutation() public pure {
        uint8[52] memory deck = Shuffle.shuffledDeck(12345);

        // Every card 0..51 must appear exactly once.
        bool[52] memory seen;
        for (uint8 i = 0; i < 52; i++) {
            uint8 c = deck[i];
            assertLt(c, 52);
            assertFalse(seen[c], "duplicate card");
            seen[c] = true;
        }
        for (uint8 i = 0; i < 52; i++) {
            assertTrue(seen[i], "missing card");
        }
    }

    function test_deterministicForSameSeed() public pure {
        uint8[52] memory a = Shuffle.shuffledDeck(999);
        uint8[52] memory b = Shuffle.shuffledDeck(999);
        for (uint8 i = 0; i < 52; i++) {
            assertEq(a[i], b[i]);
        }
    }

    function test_differentSeedsDiffer() public pure {
        uint8[52] memory a = Shuffle.shuffledDeck(1);
        uint8[52] memory b = Shuffle.shuffledDeck(2);

        bool anyDifferent = false;
        for (uint8 i = 0; i < 52; i++) {
            if (a[i] != b[i]) {
                anyDifferent = true;
                break;
            }
        }
        assertTrue(anyDifferent, "seeds produced identical decks");
    }

    function test_cardDecoding() public pure {
        // index 0 -> rank 0, suit 0 (two of clubs)
        (uint8 rank, uint8 suit) = Shuffle.card(0);
        assertEq(rank, 0);
        assertEq(suit, 0);

        // index 51 -> rank 12, suit 3 (ace of spades)
        (rank, suit) = Shuffle.card(51);
        assertEq(rank, 12);
        assertEq(suit, 3);
    }
}
