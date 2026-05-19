## Staking and slashing

Our consensus mechanism will allow users to **stake crypto as capital** on the Ethereum mainnet — a bond posted on Layer 1 that gives them the right to validate transactions on our chain. Each other master node in the network monitors the elected leader's transactions to confirm validity. Should the leader make a bad transaction, the other nodes can submit a **fraud proof** and **slash** the leader's balance on mainnet.

That is the bare-bones economic model: misbehave and lose your bond. The strength of the guarantee depends on how much capital is at stake — too little, and the cost of cheating is below the reward; too much, and validating becomes the privilege of whales. Calibration of these numbers is its own design problem.

## Where the stake lives

The stake doesn't live on our chain. It lives on the Ethereum mainnet, in a smart contract that holds the validator's ERC20 deposit until it is voluntarily withdrawn (after a cool-down) or involuntarily slashed (after a successful fraud proof).

Our staking contract will allow holders of our ERC20 to **stake** — or "vest" — by transferring tokens to the contract. The contract pulls tokens from the user's wallet rather than receiving them passively, which means the user has to *pre-authorise* the transfer via the standard ERC20 `approve` flow before calling our `deposit`. That's the cost of treating ERC20 as a *pull* primitive: every transfer of tokens you don't own requires explicit allowance.

We covered the mechanic in Chapter 6's ERC20 page; here it shows up as a real architectural constraint.

## What we're building

The full picture has two cooperating contracts, both on Ethereum mainnet:

- **Staking contract** — holds *validator capital*. This is the bond. Validators deposit ERC20 tokens here, the contract issues them a staked-share token in return, and the contract enforces lock-up, withdrawal, and slashing rules.
- **Bridging contract** — holds *network user funds*. Lets users deposit a stablecoin like USDC or USDT on mainnet so they have something to spend on our chain. The bridge is a separate concern from staking and gets its own page.

The next pages walk through each in turn. The staking contract first, because the bridge depends on validators being meaningfully bonded.
