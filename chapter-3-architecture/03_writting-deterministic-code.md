## Writing deterministic code

If you've written Typescript, Java or C# code (maybe others), writing your code in a paradigm that can be unit testable means you've all ready thought about determinstic code.

To isoloate parts of the system for unit tests, we write mocks or stubs, then inject these via contsctror injections or gettings and settters.

In Solidity, we do this same thing.

Fun fact, Waffle... I started waffle 2.

Let's write a Mock Solidity contract to demonstrate the point. Perhaps we want to test what would happen if there is too much slippage on Uniswap. We can create a mock Uniswap contract, implement the swap function, and set a slippage amount prior to testing.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title MockUniswapRouter
 * @notice A mock Uniswap router for testing slippage scenarios
 * @dev This contract allows us to simulate various slippage conditions
 *      by using setters to configure the behavior before each test
 */
contract MockUniswapRouter {
    // Configurable slippage percentage (in basis points: 100 = 1%)
    uint256 private slippagePercent;

    // Flag to simulate slippage revert scenarios
    bool private shouldRevertOnSlippage;

    // Track the last swap for verification in tests
    address public lastTokenIn;
    address public lastTokenOut;
    uint256 public lastAmountIn;
    uint256 public lastAmountOut;

    /**
     * @notice Set the slippage percentage for the next swap
     * @param _slippagePercent The slippage in basis points (e.g., 500 = 5%)
     */
    function setSlippage(uint256 _slippagePercent) external {
        slippagePercent = _slippagePercent;
    }

    /**
     * @notice Configure whether the swap should revert on slippage
     * @param _shouldRevert True to simulate a slippage revert
     */
    function setShouldRevertOnSlippage(bool _shouldRevert) external {
        shouldRevertOnSlippage = _shouldRevert;
    }

    /**
     * @notice Mock implementation of Uniswap's swapExactTokensForTokens
     * @param amountIn The amount of input tokens
     * @param amountOutMin The minimum amount of output tokens expected
     * @param path Array of token addresses representing the swap path
     * @param to Address to receive the output tokens
     * @param deadline Unix timestamp deadline for the swap
     * @return amounts Array of token amounts for each step in the path
     */
    function swapExactTokensForTokens(
        uint256 amountIn,
        uint256 amountOutMin,
        address[] calldata path,
        address to,
        uint256 deadline
    ) external returns (uint256[] memory amounts) {
        require(deadline >= block.timestamp, "MockUniswap: EXPIRED");
        require(path.length >= 2, "MockUniswap: INVALID_PATH");

        // Calculate output amount with slippage applied
        uint256 amountOut = amountIn - ((amountIn * slippagePercent) / 10000);

        // Simulate slippage protection - revert if output is below minimum
        if (shouldRevertOnSlippage && amountOut < amountOutMin) {
            revert("MockUniswap: INSUFFICIENT_OUTPUT_AMOUNT");
        }

        // Store swap details for test verification
        lastTokenIn = path[0];
        lastTokenOut = path[path.length - 1];
        lastAmountIn = amountIn;
        lastAmountOut = amountOut;

        // Return amounts array (simplified for mock)
        amounts = new uint256[](path.length);
        amounts[0] = amountIn;
        amounts[path.length - 1] = amountOut;

        return amounts;
    }

    /**
     * @notice Reset the mock to default state
     * @dev Useful for cleaning up between tests
     */
    function reset() external {
        slippagePercent = 0;
        shouldRevertOnSlippage = false;
        lastTokenIn = address(0);
        lastTokenOut = address(0);
        lastAmountIn = 0;
        lastAmountOut = 0;
    }
}
```

### Using the Mock in Tests

This mock contract enables deterministic testing of various slippage scenarios:

```solidity
// Example test case using the mock
contract TestTradingContract {
    MockUniswapRouter mockRouter;

    function setUp() public {
        mockRouter = new MockUniswapRouter();
    }

    function testHighSlippageScenario() public {
        // Configure the mock to simulate 10% slippage
        mockRouter.setSlippage(1000); // 1000 basis points = 10%
        mockRouter.setShouldRevertOnSlippage(true);

        // Attempt swap that should fail due to slippage
        address[] memory path = new address[](2);
        path[0] = address(0x1); // Token A
        path[1] = address(0x2); // Token B

        // This should revert because output (90 tokens) < minimum (95 tokens)
        vm.expectRevert("MockUniswap: INSUFFICIENT_OUTPUT_AMOUNT");
        mockRouter.swapExactTokensForTokens(
            100,  // amountIn
            95,   // amountOutMin
            path,
            address(this),
            block.timestamp + 300
        );
    }

    function testAcceptableSlippageScenario() public {
        // Configure the mock to simulate 2% slippage
        mockRouter.setSlippage(200); // 200 basis points = 2%

        // Attempt swap that should succeed
        address[] memory path = new address[](2);
        path[0] = address(0x1);
        path[1] = address(0x2);

        uint256[] memory amounts = mockRouter.swapExactTokensForTokens(
            100,  // amountIn
            95,   // amountOutMin
            path,
            address(this),
            block.timestamp + 300
        );

        // Verify output: 100 - (100 * 2%) = 98 tokens
        assertEq(amounts[1], 98, "Slippage calculation incorrect");
        assertEq(mockRouter.lastAmountOut(), 98, "Stored amount incorrect");
    }
}
```

### Key Benefits of This Approach

By using setters like `setSlippage()` and `setShouldRevertOnSlippage()`, we achieve deterministic testing where exact scenarios can be tested repeatedly with predictable outcomes. Our tests remain isolated from actual Uniswap state, external price feeds, or liquidity conditions—all of which are non-deterministic in production environments. This isolation enables us to easily test edge cases and extreme scenarios such as high slippage or deadline expiry that would be difficult or impossible to reproduce reliably on a live DEX. Additionally, mock contracts execute instantly without the overhead of setting up liquidity pools or token approvals, making our test suite fast and efficient. This pattern of using mocks with configurable setters is fundamental to writing robust, deterministic smart contract tests—the same principle applies when building deterministic blockchain applications.
