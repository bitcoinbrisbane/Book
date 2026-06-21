// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { Test } from "forge-std/Test.sol";
import { PokerTable } from "../src/PokerTable.sol";
import { ERC20 } from "@openzeppelin/contracts/token/ERC20/ERC20.sol";

contract MockUSD is ERC20 {
    constructor() ERC20("Mock USD", "mUSD") {}

    function mint(address to, uint256 amount) external {
        _mint(to, amount);
    }
}

contract PokerTableTest is Test {
    MockUSD token;
    PokerTable table;

    address alice = address(0xA11CE);
    address bob = address(0xB0B);
    address carol = address(0xCA401);

    uint256 constant MIN = 100e6;
    uint256 constant MAX = 1000e6;
    uint256 constant SB = 1e6;
    uint256 constant BB = 2e6;

    function setUp() public {
        token = new MockUSD();
        table = new PokerTable(address(token), MIN, MAX, SB, BB);

        _fund(alice);
        _fund(bob);
        _fund(carol);
    }

    function _fund(address who) internal {
        token.mint(who, MAX);
        vm.prank(who);
        token.approve(address(table), type(uint256).max);
    }

    function _seat(address who, uint8 seat) internal {
        vm.prank(who);
        table.joinAtSeat(MIN, seat);
    }

    function test_join_pullsTokensAndSeats() public {
        _seat(alice, 0);
        (address wallet, uint256 stack, bool seated,,,) = table.seats(0);
        assertEq(wallet, alice);
        assertEq(stack, MIN);
        assertTrue(seated);
        assertEq(token.balanceOf(address(table)), MIN);
    }

    function test_startHand_postsBlinds() public {
        _seat(alice, 0);
        _seat(bob, 1);
        _seat(carol, 2);

        table.startHand();

        // Button rotates from 0 to seat 1; SB = seat 2, BB = seat 0.
        assertEq(table.buttonSeat(), 1);
        assertEq(table.pot(), SB + BB);
        assertEq(table.currentBet(), BB);
        assertTrue(table.handInProgress());
    }

    function test_bettingRound_callsAroundAndBigBlindChecks() public {
        _seat(alice, 0);
        _seat(bob, 1);
        _seat(carol, 2);
        table.startHand();
        // button=1, SB=2 (carol), BB=0 (alice), UTG=1 (bob) acts first.

        // Bob (UTG) calls the big blind.
        vm.prank(bob);
        table.call();
        assertEq(table.actingSeat(), 2); // action to carol (small blind)

        // Carol owes BB - SB; she calls.
        vm.prank(carol);
        table.call();
        assertEq(table.actingSeat(), 0); // action to alice (big blind option)

        // Alice is the big blind with nothing to call; she checks to close.
        assertTrue(table.handInProgress());
        vm.prank(alice);
        table.check();

        // Everyone matched at the big blind; pot = 3 * BB.
        assertEq(table.pot(), 3 * BB);
        assertEq(table.currentBet(), BB);
    }

    function test_raise_reopensAction() public {
        _seat(alice, 0);
        _seat(bob, 1);
        _seat(carol, 2);
        table.startHand();
        // button=1, SB=2 (carol), BB=0 (alice), UTG=1 (bob) acts first.

        // Bob raises by one big blind: currentBet 2 -> 4.
        vm.prank(bob);
        table.raise(BB);
        assertEq(table.currentBet(), 2 * BB);
        assertEq(table.actingSeat(), 2); // carol

        // Carol calls the raise (owes 4 - 1 = 3).
        vm.prank(carol);
        table.call();
        assertEq(table.actingSeat(), 0); // alice

        // Alice (was BB) now owes 4 - 2 = 2 to call the raise; round not done.
        assertTrue(table.handInProgress());
        vm.prank(alice);
        table.call();

        // All matched at 2*BB; pot = 3 * (2*BB).
        assertEq(table.pot(), 3 * 2 * BB);
    }

    function test_foldToLastPlayer_settlesPot() public {
        _seat(alice, 0);
        _seat(bob, 1);
        _seat(carol, 2);

        table.startHand();

        // button=1, SB=2 (carol), BB=0 (alice), UTG=1 (bob) acts first.
        uint256 potBefore = table.pot();

        vm.prank(bob);
        table.fold(); // active 3 -> 2, action moves to carol (seat 2)

        vm.prank(carol);
        table.fold(); // active 2 -> 1, alice wins the pot

        assertFalse(table.handInProgress());

        // Alice posted BB (2) but wins SB+BB (3): net stack = MIN - BB + pot.
        (, uint256 aliceStack,,,,) = table.seats(0);
        assertEq(aliceStack, MIN - BB + potBefore);
    }
}
