// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

// The contract our toy compiler targets. We support a deliberately tiny subset
// of Solidity: one uint256 state variable, a constructor that seeds it, and
// getter/setter functions. That subset is enough to exercise every stage of a
// real compiler (lexing, parsing, codegen) and to emit bytecode that actually
// runs on the EVM.
contract Greeter {
    uint256 greeting;

    constructor() {
        greeting = 42;
    }

    function greet() public view returns (uint256) {
        return greeting;
    }

    function setGreeting(uint256 g) public {
        greeting = g;
    }
}
