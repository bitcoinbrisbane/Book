// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { Test } from "forge-std/Test.sol";
import { HandRank } from "../src/HandRank.sol";

contract HandRankTest is Test {
    // Card index helper: rank 0..12 (2..A), suit 0..3.
    function c(uint8 rank, uint8 suit) internal pure returns (uint8) {
        return suit * 13 + rank;
    }

    // Rank shorthands (rank index, not face value).
    uint8 constant TWO = 0;
    uint8 constant THREE = 1;
    uint8 constant FOUR = 2;
    uint8 constant FIVE = 3;
    uint8 constant SIX = 4;
    uint8 constant SEVEN = 5;
    uint8 constant EIGHT = 6;
    uint8 constant NINE = 7;
    uint8 constant TEN = 8;
    uint8 constant JACK = 9;
    uint8 constant QUEEN = 10;
    uint8 constant KING = 11;
    uint8 constant ACE = 12;

    uint8 constant CLUBS = 0;
    uint8 constant DIAMONDS = 1;
    uint8 constant HEARTS = 2;
    uint8 constant SPADES = 3;

    function score(uint8[5] memory h) internal pure returns (uint256) {
        return HandRank.score5(h);
    }

    function test_categoryOrdering() public pure {
        // One representative hand per category, weakest to strongest.
        uint256 highCard =
            score([c(ACE, CLUBS), c(KING, DIAMONDS), c(NINE, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        uint256 pair =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(NINE, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        uint256 twoPair =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(NINE, HEARTS), c(NINE, SPADES), c(TWO, CLUBS)]);
        uint256 trips =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(ACE, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        uint256 straight =
            score([c(FIVE, CLUBS), c(SIX, DIAMONDS), c(SEVEN, HEARTS), c(EIGHT, SPADES), c(NINE, CLUBS)]);
        uint256 flush =
            score([c(ACE, CLUBS), c(KING, CLUBS), c(NINE, CLUBS), c(SEVEN, CLUBS), c(TWO, CLUBS)]);
        uint256 fullHouse =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(ACE, HEARTS), c(SEVEN, SPADES), c(SEVEN, CLUBS)]);
        uint256 quads =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(ACE, HEARTS), c(ACE, SPADES), c(TWO, CLUBS)]);
        uint256 straightFlush =
            score([c(FIVE, CLUBS), c(SIX, CLUBS), c(SEVEN, CLUBS), c(EIGHT, CLUBS), c(NINE, CLUBS)]);

        assertLt(highCard, pair);
        assertLt(pair, twoPair);
        assertLt(twoPair, trips);
        assertLt(trips, straight);
        assertLt(straight, flush);
        assertLt(flush, fullHouse);
        assertLt(fullHouse, quads);
        assertLt(quads, straightFlush);
    }

    function test_wheelStraight() public pure {
        // A-2-3-4-5 is a five-high straight, weaker than 2-3-4-5-6.
        uint256 wheel =
            score([c(ACE, CLUBS), c(TWO, DIAMONDS), c(THREE, HEARTS), c(FOUR, SPADES), c(FIVE, CLUBS)]);
        uint256 sixHigh =
            score([c(TWO, CLUBS), c(THREE, DIAMONDS), c(FOUR, HEARTS), c(FIVE, SPADES), c(SIX, CLUBS)]);
        assertLt(wheel, sixHigh);
        // But the wheel still beats ace-high nothing.
        uint256 aceHigh =
            score([c(ACE, CLUBS), c(KING, DIAMONDS), c(NINE, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        assertLt(aceHigh, wheel);
    }

    function test_kickerBreaksTie() public pure {
        // Both pairs of aces; the higher kicker wins.
        uint256 aceKing =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(KING, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        uint256 aceQueen =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(QUEEN, HEARTS), c(SEVEN, SPADES), c(TWO, CLUBS)]);
        assertGt(aceKing, aceQueen);
    }

    function test_best7PicksBestFive() public pure {
        // Seven cards containing a flush; best7 should find it over any pair.
        uint8[7] memory cards = [
            c(ACE, CLUBS),
            c(KING, CLUBS),
            c(NINE, CLUBS),
            c(SEVEN, CLUBS),
            c(TWO, CLUBS), // five clubs -> flush
            c(ACE, DIAMONDS),
            c(ACE, HEARTS) // also trip aces, but flush is stronger
        ];
        uint256 best = HandRank.best7(cards);

        uint256 flushOnly =
            score([c(ACE, CLUBS), c(KING, CLUBS), c(NINE, CLUBS), c(SEVEN, CLUBS), c(TWO, CLUBS)]);
        assertEq(best, flushOnly);
    }

    function test_fullHouseBeatsFlushInSeven() public pure {
        // Board + hole that make a full house but not a flush.
        uint8[7] memory cards = [
            c(ACE, CLUBS),
            c(ACE, DIAMONDS),
            c(ACE, HEARTS),
            c(KING, SPADES),
            c(KING, CLUBS),
            c(THREE, DIAMONDS),
            c(TWO, HEARTS)
        ];
        uint256 best = HandRank.best7(cards);
        uint256 expected =
            score([c(ACE, CLUBS), c(ACE, DIAMONDS), c(ACE, HEARTS), c(KING, SPADES), c(KING, CLUBS)]);
        assertEq(best, expected);
    }
}
