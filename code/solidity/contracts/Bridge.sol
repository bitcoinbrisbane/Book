// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import IERC20 from "@openzeppelin/contracts/token/ERC20/IERC20.sol";

contract Bridge {

    mapping (address => uint256) public balances;
    IERC20 public token;

    constructor(address _token) {
        token = IERC20(_token);
    }

    function deposit(uint256 amount) external {
        token.transferFrom(msg.sender, address(this), amount);
        balances[msg.sender] += amount;
    }

    event Deposited(address indexed user, uint256 amount);
}
