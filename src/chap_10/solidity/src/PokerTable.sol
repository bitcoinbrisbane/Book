// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { HandRank } from "./HandRank.sol";

/// @title PokerTable
/// @notice A "for fun" on-chain Texas Hold'em table. NOT for production use:
///         it ignores card dealing, hidden information, and randomness, and
///         carries several deliberately-flagged shortcuts (see the book).
contract PokerTable {
    uint8 public constant MAX_PLAYERS = 9;

    IERC20 public immutable token;
    uint256 public immutable minBuyIn;
    uint256 public immutable maxBuyIn;
    uint256 public immutable smallBlind;
    uint256 public immutable bigBlind;

    struct Player {
        address wallet;
        uint256 stack; // chips currently in front of the player
        bool seated;
        bool folded; // has given up the current hand
        bool hasActed; // has acted since the last bet/raise this round
        uint256 committed; // chips put into the pot this betting round
    }

    Player[MAX_PLAYERS] public seats;
    uint8 public playerCount;

    // Per-hand state.
    uint8 public buttonSeat; // seat index of the dealer button
    uint256 public pot; // chips wagered this hand
    uint256 public currentBet; // amount a player must match to stay in
    uint256 public lastRaiseSize; // size of the last raise, for min-raise rule
    uint8 public actingSeat; // whose turn it is to act
    bool public handInProgress;
    bool public bettingClosed; // the final betting round has finished; awaiting showdown
    uint8 public activeInHand; // players who have not folded this hand

    event PlayerJoined(address indexed player, uint8 seat, uint256 buyIn);
    event HandStarted(uint8 button, uint8 smallBlindSeat, uint8 bigBlindSeat);
    event BlindPosted(uint8 seat, uint256 amount);
    event PlayerFolded(uint8 seat);
    event PlayerChecked(uint8 seat);
    event PlayerCalled(uint8 seat, uint256 amount);
    event PlayerRaised(uint8 seat, uint256 toAmount);
    event BettingRoundComplete(uint256 pot);
    event ShowdownScored(uint8 seat, uint256 score);
    event HandSettled(uint8 winnerSeat, uint256 amount);

    constructor(
        address token_,
        uint256 minBuyIn_,
        uint256 maxBuyIn_,
        uint256 smallBlind_,
        uint256 bigBlind_
    ) {
        require(minBuyIn_ > 0, "min buy-in must be > 0");
        require(maxBuyIn_ >= minBuyIn_, "max must be >= min");
        require(bigBlind_ > smallBlind_, "big blind must exceed small");
        require(minBuyIn_ >= bigBlind_, "buy-in must cover big blind");

        token = IERC20(token_);
        minBuyIn = minBuyIn_;
        maxBuyIn = maxBuyIn_;
        smallBlind = smallBlind_;
        bigBlind = bigBlind_;
    }

    // ---------------------------------------------------------------------
    // Joining a table (chapter page: "A poker hand in Solidity")
    // ---------------------------------------------------------------------

    function joinAtSeat(uint256 buyIn, uint8 seat) public returns (uint8) {
        require(buyIn >= minBuyIn, "below minimum buy-in");
        require(buyIn <= maxBuyIn, "above maximum buy-in");
        require(seat < MAX_PLAYERS, "seat out of range");
        require(!seats[seat].seated, "seat taken");

        // Pull the player's stablecoin into the contract.
        require(
            token.transferFrom(msg.sender, address(this), buyIn),
            "transfer failed"
        );

        seats[seat] = Player({
            wallet: msg.sender,
            stack: buyIn,
            seated: true,
            folded: false,
            hasActed: false,
            committed: 0
        });
        playerCount++;

        emit PlayerJoined(msg.sender, seat, buyIn);
        return seat;
    }

    function join(uint256 buyIn) external returns (uint8 seat) {
        require(playerCount < MAX_PLAYERS, "table full");
        seat = _findEmptySeat();
        return joinAtSeat(buyIn, seat);
    }

    function _findEmptySeat() private view returns (uint8) {
        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (!seats[i].seated) {
                return i;
            }
        }
        revert("no empty seat");
    }

    // ---------------------------------------------------------------------
    // Positions and blinds (chapter page: "Positions and blinds")
    // ---------------------------------------------------------------------

    function startHand() external {
        require(!handInProgress, "hand already running");
        require(playerCount >= 3, "need at least 3 players");

        // Reset per-hand state on every occupied seat.
        pot = 0;
        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (seats[i].seated) {
                seats[i].folded = false;
                seats[i].hasActed = false;
                seats[i].committed = 0;
            }
        }
        activeInHand = playerCount;

        // Rotate the button to the next occupied seat.
        buttonSeat = _nextOccupiedSeat(buttonSeat);

        uint8 sbSeat = _nextOccupiedSeat(buttonSeat);
        uint8 bbSeat = _nextOccupiedSeat(sbSeat);

        _postBlind(sbSeat, smallBlind);
        _postBlind(bbSeat, bigBlind);

        currentBet = bigBlind;
        lastRaiseSize = bigBlind; // the next raise must be at least one big blind
        actingSeat = _nextOccupiedSeat(bbSeat); // under the gun
        handInProgress = true;
        bettingClosed = false;

        emit HandStarted(buttonSeat, sbSeat, bbSeat);
    }

    // ---------------------------------------------------------------------
    // Acting: folding, and settling when one player remains
    // (chapter page: "A passive machine, and the first action")
    // ---------------------------------------------------------------------

    function fold() external {
        uint8 seat = _requireTurn();

        seats[seat].folded = true;
        activeInHand--;

        emit PlayerFolded(seat);

        if (activeInHand == 1) {
            _settleToLastPlayer();
        } else {
            _advanceAfterAction(seat);
        }
    }

    // ---------------------------------------------------------------------
    // The betting round: check, call, raise (chapter page: "The betting round")
    // ---------------------------------------------------------------------

    function check() external {
        uint8 seat = _requireTurn();
        // You may only check if you owe nothing — your committed already matches.
        require(seats[seat].committed == currentBet, "must call or fold");

        seats[seat].hasActed = true;
        emit PlayerChecked(seat);

        _advanceAfterAction(seat);
    }

    function call() external {
        uint8 seat = _requireTurn();
        Player storage p = seats[seat];

        uint256 owed = currentBet - p.committed;
        require(owed > 0, "nothing to call");
        require(owed <= p.stack, "insufficient stack"); // toy: no all-in/side pots

        p.stack -= owed;
        p.committed += owed;
        pot += owed;
        p.hasActed = true;

        emit PlayerCalled(seat, owed);

        _advanceAfterAction(seat);
    }

    function raise(uint256 raiseBy) external {
        uint8 seat = _requireTurn();
        Player storage p = seats[seat];

        // Enforce the min-raise: a raise must be at least the size of the last one.
        require(raiseBy >= lastRaiseSize, "raise below minimum");

        uint256 newBet = currentBet + raiseBy;
        uint256 owed = newBet - p.committed; // call amount plus the raise
        require(owed <= p.stack, "insufficient stack"); // toy: no all-in/side pots

        p.stack -= owed;
        p.committed += owed;
        pot += owed;

        currentBet = newBet;
        lastRaiseSize = raiseBy;

        // A raise reopens the action: everyone else must respond again.
        _resetActedExcept(seat);
        p.hasActed = true;

        emit PlayerRaised(seat, newBet);

        _advanceAfterAction(seat);
    }

    /// @dev Shared turn guard: hand running, caller owns the acting seat.
    function _requireTurn() private view returns (uint8 seat) {
        require(handInProgress, "no hand in progress");
        seat = actingSeat;
        require(seats[seat].wallet == msg.sender, "not your turn");
        require(!seats[seat].folded, "already folded");
    }

    /// @dev Clear hasActed on every active player except `keep`, so a raise
    ///      forces them all to act again.
    function _resetActedExcept(uint8 keep) private {
        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (seats[i].seated && !seats[i].folded && i != keep) {
                seats[i].hasActed = false;
            }
        }
    }

    /// @dev After any action, either close the round or pass to the next player.
    function _advanceAfterAction(uint8 seat) private {
        if (_roundComplete()) {
            // A real game would deal the next street (flop/turn/river) and open a
            // fresh betting round between each. Our toy collapses all of that into
            // a single round, then jumps straight to the showdown. Mark the betting
            // closed so the cards can be revealed and scored.
            bettingClosed = true;
            emit BettingRoundComplete(pot);
        } else {
            actingSeat = _nextActiveSeat(seat);
        }
    }

    /// @dev The round is complete once every active player has acted since the
    ///      last raise AND has matched the current bet.
    function _roundComplete() private view returns (bool) {
        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (seats[i].seated && !seats[i].folded) {
                if (!seats[i].hasActed || seats[i].committed != currentBet) {
                    return false;
                }
            }
        }
        return true;
    }

    // ---------------------------------------------------------------------
    // Showdown: reveal the cards and award the pot to the best hand
    // (chapter page: "Ranking hands")
    // ---------------------------------------------------------------------

    /// @notice Settle the hand by comparing the revealed cards of every player
    ///         still live, awarding the pot to the best 5-card hand.
    /// @param board The five community cards, as indices 0..51.
    /// @param holeCards Two hole cards per seat, indexed by seat. Only the seats
    ///        still in the hand are read; folded and empty seats are ignored.
    /// @dev TEACHING CODE. A public chain cannot keep the hole cards secret on
    ///      its own, so this function simply trusts whatever cards it is handed.
    ///      A real game would prove these cards against an earlier commitment
    ///      (see the "hiding the deck" page). The point here is only the
    ///      *ranking and settlement*, which is the part the EVM does well.
    function showdown(uint8[5] calldata board, uint8[2][MAX_PLAYERS] calldata holeCards)
        external
    {
        require(handInProgress, "no hand in progress");
        require(bettingClosed, "betting still open");

        uint8 bestSeat = MAX_PLAYERS; // sentinel: no winner found yet
        uint256 bestScore;

        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (seats[i].seated && !seats[i].folded) {
                uint8[7] memory seven = [
                    holeCards[i][0],
                    holeCards[i][1],
                    board[0],
                    board[1],
                    board[2],
                    board[3],
                    board[4]
                ];
                uint256 s = HandRank.best7(seven);
                emit ShowdownScored(i, s);

                // Strict > means ties leave the pot with the earlier seat. A
                // production game would split the pot; our toy keeps it simple
                // and flags the shortcut here.
                if (bestSeat == MAX_PLAYERS || s > bestScore) {
                    bestSeat = i;
                    bestScore = s;
                }
            }
        }

        require(bestSeat < MAX_PLAYERS, "no active seat");

        uint256 amount = pot;
        pot = 0;
        seats[bestSeat].stack += amount;
        handInProgress = false;
        bettingClosed = false;

        emit HandSettled(bestSeat, amount);
    }

    function _settleToLastPlayer() private {
        uint8 winner = _firstActiveSeat();

        uint256 amount = pot;
        pot = 0;
        seats[winner].stack += amount;
        handInProgress = false;

        emit HandSettled(winner, amount);
    }

    function _firstActiveSeat() private view returns (uint8) {
        for (uint8 i = 0; i < MAX_PLAYERS; i++) {
            if (seats[i].seated && !seats[i].folded) {
                return i;
            }
        }
        revert("no active seat");
    }

    function _nextActiveSeat(uint8 from) private view returns (uint8) {
        for (uint8 i = 1; i <= MAX_PLAYERS; i++) {
            uint8 candidate = (from + i) % MAX_PLAYERS;
            if (seats[candidate].seated && !seats[candidate].folded) {
                return candidate;
            }
        }
        revert("no active seat");
    }

    function _postBlind(uint8 seat, uint256 amount) private {
        Player storage p = seats[seat];
        uint256 posted = amount > p.stack ? p.stack : amount; // all-in for less

        p.stack -= posted;
        p.committed += posted;
        pot += posted;

        emit BlindPosted(seat, posted);
    }

    function _nextOccupiedSeat(uint8 from) private view returns (uint8) {
        for (uint8 i = 1; i <= MAX_PLAYERS; i++) {
            uint8 candidate = (from + i) % MAX_PLAYERS;
            if (seats[candidate].seated) {
                return candidate;
            }
        }
        revert("no occupied seat");
    }
}
