// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

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
    }

    Player[MAX_PLAYERS] public seats;
    uint8 public playerCount;

    // Per-hand state.
    uint8 public buttonSeat; // seat index of the dealer button
    uint256 public pot; // chips wagered this hand
    uint256 public currentBet; // amount a player must match to stay in
    uint8 public actingSeat; // whose turn it is to act
    bool public handInProgress;
    uint8 public activeInHand; // players who have not folded this hand

    event PlayerJoined(address indexed player, uint8 seat, uint256 buyIn);
    event HandStarted(uint8 button, uint8 smallBlindSeat, uint8 bigBlindSeat);
    event BlindPosted(uint8 seat, uint256 amount);
    event PlayerFolded(uint8 seat);
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

        seats[seat] = Player({ wallet: msg.sender, stack: buyIn, seated: true, folded: false });
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
        actingSeat = _nextOccupiedSeat(bbSeat); // under the gun
        handInProgress = true;

        emit HandStarted(buttonSeat, sbSeat, bbSeat);
    }

    // ---------------------------------------------------------------------
    // Acting: folding, and settling when one player remains
    // (chapter page: "A passive machine, and the first action")
    // ---------------------------------------------------------------------

    function fold() external {
        require(handInProgress, "no hand in progress");

        uint8 seat = actingSeat;
        require(seats[seat].wallet == msg.sender, "not your turn");
        require(!seats[seat].folded, "already folded");

        seats[seat].folded = true;
        activeInHand--;

        emit PlayerFolded(seat);

        if (activeInHand == 1) {
            _settleToLastPlayer();
        } else {
            actingSeat = _nextActiveSeat(seat);
        }
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
