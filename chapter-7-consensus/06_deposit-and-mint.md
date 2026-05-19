## Deposit and mint

The **deposit** function is the user-facing entry point for staking. It allows holders of our ERC20 to add their capital to the staking contract; the contract then locks their tokens for a set period and gives them a **share** in return. We use the ERC20 `mint` function to create a new supply of the **staked ERC20** — a separate token that represents the holder's claim on the underlying deposit plus any rewards accrued while it sits in the contract.

Three ERC20 primitives do all the heavy lifting:

- **`transferFrom`** — the contract *pulls* underlying tokens out of the user's wallet. This requires the user to have called `approve` on the underlying token beforehand, granting the staking contract an allowance.
- **`mint`** — creates new supply of the staked-token. Only the staking contract is authorised to mint.
- **`transfer`** — moves the freshly minted staked-token from the contract to the user.

Upon minting, the user's balance of the staked-token, and the staked-token's total supply, equal the underlying capital they deposited — *always one-to-one at the moment of deposit*. (Over time, rewards or slashing can make the share-to-underlying ratio drift from 1:1, but that's a topic for when we introduce yield accounting.)

The contract is now responsible for paying out: it pulls the tokens into custody and holds them until the user is allowed to release them. That release is only possible if the logic coded into the contract permits it — the lock period has expired, the validator hasn't been slashed, the cool-down window is closed. The contract is the rule book; the user has no recourse outside what the rule book allows.

## The flow, end to end

From a user's perspective, staking is two transactions:

1. **`approve`** on the underlying ERC20, granting the staking contract permission to move `amount` tokens.
2. **`deposit`** on the staking contract — which performs `transferFrom` to pull the tokens, then `mint`s the staked-token, then `transfer`s the freshly minted shares back.

That two-step dance — *approve, then act* — is the fundamental ERC20 pattern. It exists because ERC20 transfers are pull-based, not push-based, and the recipient contract needs explicit authorisation to take what it's owed.

## Validator eligibility

Each validator must stake at least a minimum amount of the underlying token to be eligible to validate. That minimum is set in the contract at deployment time and represents the security floor — anyone with capital below it cannot validate, and anyone with capital above it has *more at risk* than they have to gain from cheating. The selection algorithm uses this same bonded amount to decide who validates each block; we cover that on page 08.
