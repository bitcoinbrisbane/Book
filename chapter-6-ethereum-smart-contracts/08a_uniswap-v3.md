## Uniswap v3

The Uniswap v3 smart contracts are broken up into **two repositories** — *Core* and *Periphery*. The contracts are named well, so it's easy to navigate the solution once you know the split.

- [github.com/Uniswap/v3-core](https://github.com/Uniswap/v3-core)
- [github.com/Uniswap/v3-periphery](https://github.com/Uniswap/v3-periphery)

The separation is deliberate. Anything in *core* is consensus-critical and must never change after deployment — if a bug were found in `UniswapV3Pool`, every existing pool on the chain would be at risk and no upgrade is possible. Anything in *periphery* is the user-facing convenience layer; if a bug is found there, a new periphery contract is deployed and frontends are pointed at it. That kind of layering — *minimal, immutable, audited core; flexible periphery* — is a pattern worth borrowing in your own protocols.

## Core contracts

The core repo contains the contracts that deploy the pools, plus the pools themselves:

- **`UniswapV3PoolDeployer`** — the deployer is split out from the factory so that `UniswapV3Pool` can be constructed with deterministic addresses (`CREATE2`) and so the pool's constructor doesn't need to hold the factory's full state in memory.
- **`UniswapV3Factory`** — deploys new pools, one per `(tokenA, tokenB, feeTier)` triple. The factory enforces uniqueness — there is exactly one pool per pair per fee tier (0.05%, 0.3%, 1%, and later 0.01% for stablecoins).
- **`UniswapV3Pool`** — the pool itself. Holds the two-token reserves, the price (as a `sqrtPriceX96` in Q64.96 fixed-point), the active tick, the per-tick liquidity, and the entire concentrated-liquidity machinery. This is *the* contract that does the swap.

## Math libraries

Solidity does not support floating-point numbers. Even in modern versions (0.8.x and later), there are no native `float` or `double` types — every quantity is an integer. That makes overflow and underflow a real concern, and it makes price math, which inherently involves fractions, awkward.

Uniswap v3 solves this with a set of library contracts that perform their math in **fixed-point** representations — every quantity is an integer with an implicit scaling factor. The libraries perform overflow checks on every operation, and the order of multiplications and divisions is tuned to preserve as much precision as possible without ever spilling out of a 256-bit slot.

You don't need to understand the full math to deploy on top of Uniswap — but you do need to know that the price you read out of a pool is *not* a plain decimal. It's a square-root, fixed-point, integer-encoded number. Misreading that field is one of the more common ways a junior developer breaks a Uniswap integration.

> **Aside on the formats**: prices are stored as `sqrtPriceX96` — the square root of the price, in Q64.96 fixed-point. Fee growth uses Q128.128. You don't need either to *use* Uniswap, but you need both to *read* the right field from a pool.

## Periphery contracts

Periphery is the user-facing layer. It calls into the core but doesn't *contain* any consensus-critical state. The contracts you're most likely to meet:

- **`SwapRouter`** — the contract a wallet or aggregator calls to execute a swap. Handles slippage protection, multi-hop routing, and the `WETH9` wrap/unwrap dance when one leg of the trade is native ETH.
- **`NonfungiblePositionManager`** — turns each liquidity position into an ERC721 NFT. This is how v3 LP positions are represented and transferred. The NFT *is* the position; transfer the NFT, transfer the LP claim.
- **`Quoter`** / **`QuoterV2`** — off-chain helpers for computing what a swap *would* return without actually executing it. Called read-only from a frontend before sending a transaction.

The split lets the Uniswap team iterate on UX (better routing, MEV protection, gas optimisations) without ever touching the audited core.

## The MasterChef pattern

Another novel piece of the Uniswap ecosystem is the **MasterChef** contract. Deployed by SushiSwap (a Uniswap v2 fork) in 2020, it manages a pool of users who **stake their LP tokens** and earn additional rewards — typically a governance token like `SUSHI` — in return for keeping their liquidity locked.

The mechanic is simple in shape, dangerous in detail:

1. A user provides liquidity to a Uniswap pair and receives the pair's LP token.
2. The user deposits that LP token into the MasterChef contract.
3. MasterChef mints reward tokens to the user proportional to their share of the staked pool, accrued per block.
4. When the user wants their liquidity back, they unstake the LP token from MasterChef and burn it against the original Uniswap pair to redeem their underlying assets.

MasterChef kicked off the **yield-farming** wave of 2020–2021 — protocols competing on emission schedules, "vampire attacks" where one protocol bribed LPs to migrate from another, and the now-familiar dashboards full of three-digit APRs. The contract itself is short (a few hundred lines), but its emergent effects shaped DeFi for years. It is worth reading the source as a study in how a small piece of mechanism design can move billions of dollars.
