## Money out: signature-gated withdrawals

Deposits are permissionless — anyone can send tokens in. Withdrawals are the opposite: the bridge holds real money, so it must only release funds when the *game* says a player has genuinely won the right to them. The game can't call the contract directly (it lives off-chain), so instead it **signs** an authorisation, and the contract verifies that signature on-chain before paying out.

```solidity
function withdraw(uint256 amount, address receiver, bytes32 nonce, bytes calldata signature) external {
    require(!withdrawNonces[nonce], "withdraw: nonce already used");
    require(IERC20(underlying).balanceOf(_self) >= amount, "withdraw: insufficient balance");

    bytes32 messageHash = keccak256(abi.encodePacked(receiver, amount, nonce));
    address signer = recoverSignerAddress(getEthSignedMessageHash(messageHash), signature);

    require(IValidator(vault).isValidator(signer), "withdraw: invalid signature");

    withdrawNonces[nonce] = true;
    totalDeposits -= amount;

    IERC20(underlying).transfer(receiver, amount);

    emit Withdrawn(receiver, amount, nonce);
}
```

`withdraw` is `external` and takes no owner restriction — *anyone* can call it. The security doesn't come from *who calls*; it comes from the **signature** they have to bring. Walking through it:

1. **Replay check:** `require(!withdrawNonces[nonce], ...)`. Every authorisation carries a unique `nonce`. If this nonce has already been used, the withdrawal is a replay — reject it. (We flip the nonce to `true` further down, *before* paying out.)
2. **Liquidity check:** the bridge must actually hold at least `amount` of the underlying.
3. **Reconstruct the message:** `keccak256(abi.encodePacked(receiver, amount, nonce))`. The signer authorised a *specific* `(receiver, amount, nonce)` triple. By hashing the same three values, the contract rebuilds exactly what should have been signed. Change any of them — try to redirect the payout, or inflate the amount — and the hash changes, so the recovered signer won't match.
4. **Recover the signer:** `recoverSignerAddress(getEthSignedMessageHash(messageHash), signature)` works out which address produced this signature (more on the mechanics below).
5. **Authorise:** `require(IValidator(vault).isValidator(signer), ...)`. The recovered signer must be a registered **validator** — this is the call into the `Vault` we imported. A signature from anyone else is worthless.
6. **Effects, then interaction:** mark the nonce used, decrement `totalDeposits`, *then* `transfer` the underlying to `receiver`, and emit `Withdrawn`. Updating state before the external transfer is the **checks-effects-interactions** pattern — the standard defence against re-entrancy.

### Recovering the signer

Two helper functions do the cryptography. First, the Ethereum message prefix:

```solidity
function getEthSignedMessageHash(bytes32 message) private pure returns (bytes32) {
    return keccak256(abi.encodePacked("\x19Ethereum Signed Message:\n32", message));
}
```

This wraps the hash in the standard `"\x19Ethereum Signed Message:\n32"` prefix defined by EIP-191. The prefix exists so that a signed *message* can never be mistaken for a signed *transaction* — it makes the two domains unambiguous, so a signature collected for an off-chain message can't be replayed as a transaction.

Then the actual recovery:

```solidity
function recoverSignerAddress(bytes32 messageHash, bytes memory signature) private pure returns (address) {
    require(signature.length == 65, "Invalid signature length");

    bytes32 r;
    bytes32 s;
    uint8 v;

    assembly {
        r := mload(add(signature, 32))
        s := mload(add(signature, 64))
        v := byte(0, mload(add(signature, 96)))
    }

    require(uint256(s) <= 0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5D576E7357A4501DDFE92F46681B20A0, "Invalid signature 's' value");
    require(v == 27 || v == 28, "Invalid signature 'v' value");

    return ecrecover(messageHash, v, r, s);
}
```

An Ethereum signature is 65 bytes: two 32-byte values `r` and `s`, plus a one-byte recovery id `v`. The `assembly` block slices those three pieces out of the raw `bytes`. Then `ecrecover` — a built-in EVM precompile — takes the message hash and `(v, r, s)` and returns the address whose private key produced the signature. This is the same primitive that validates every Ethereum transaction; here we're using it directly on our own message.

The two `require`s before `ecrecover` matter more than they look:

- **The `s`-value bound** rejects any signature whose `s` is in the "upper half" of the curve order. This is **EIP-2's malleability fix**: for any valid signature `(r, s)` there's a second, equally valid `(r, curveOrder − s)`. If you don't pin `s` to the lower half, an attacker can take a valid signature and flip it into a *different* valid signature for the same message — and if your replay protection keyed off the signature bytes, the flipped one would sail through. Our replay protection keys off the *nonce*, not the signature, so we're covered there, but rejecting high-`s` signatures is correct hygiene and closes the door entirely.
- **`v == 27 || v == 28`** rejects malformed recovery ids. A bad `v` can make `ecrecover` return a garbage address or `address(0)`; constraining it to the two legal values avoids that class of bug.

### The emergency hatch

```solidity
function emergencyWithdraw() external {
    uint256 amount = IERC20(underlying).balanceOf(_self);
    if (amount == 0) return;
    uint256 delta = amount - totalDeposits;

    address owner = 0x9943d42D7a59a0abaE451130CcfC77d758da9cA0;
    IERC20(underlying).transferFrom(_self, owner, delta);
}
```

The idea here is reasonable: the bridge tracks how much it *owes* players in `totalDeposits`, so any balance *above* that (`delta`) is a surplus — stray tokens, accumulated swap dust, accidental transfers — that can be swept out without touching player funds. But the implementation has three problems worth flagging:

> **The recipient is a hardcoded address.** `0x9943d42D7a59a0abaE451130CcfC77d758da9cA0` is baked into the bytecode. If that key is ever lost or compromised, the sweep can't be redirected — it's frozen, or worse. A parameter, or the `Ownable` owner, would be the conventional choice. Hardcoded addresses are also a classic audit red flag because they're easy to miss in review.

> **`transferFrom(_self, owner, delta)` is the wrong call.** To move its *own* tokens, a contract uses `transfer(to, amount)`, not `transferFrom(self, to, amount)`. The `transferFrom` form spends an *allowance*, and it only works here because the constructor approved the contract to spend its own balance (`approve(_self, max)`). It's a roundabout way to do a plain transfer, and it relies on that earlier self-approval being in place.

> **No access control.** `emergencyWithdraw` is `external` with no `onlyOwner`. Anyone can call it — though since the funds always go to the hardcoded address, the worst a stranger can do is *trigger* the sweep, not redirect it. Still, an emergency function that anyone can fire is surprising, and not something you'd ship without a deliberate reason.

None of these is exploitable to steal *player* funds on its own, but together they're exactly the kind of finding a security review exists to catch — and a good illustration that "it compiles and the happy path works" is a long way from "this is safe to hold real money."

## The balance is an IOU

Step back from the code and look at what a deposit actually *creates*. Alice deposits 100 USDT; the bridge records `Deposit("cosmos1alice...", 100)` and the game chain credits her account with 100 chips. Those chips are not the USDT — the USDT is sitting in the bridge. The chips are a **claim** on that USDT: an IOU that says "the holder is owed 100 USDT from the pool."

That framing runs underneath a huge slice of DeFi. When you deposit into **Aave** you receive **aUSDC**, an ERC20 representing your deposit. Provide liquidity to **Uniswap** and you get an LP token. Stake ETH with **Lido** and you get **stETH**. In every case the protocol holds the real asset and hands you a transferable receipt for your share of it.

The interesting word is **transferable**. In those protocols the receipt is itself an ERC20, so the claim can move independently of the underlying — Alice can send her aUSDC to Bob, and now *Bob* can redeem the underlying, all without the underlying ever moving. Receipts can be traded, posted as collateral, or deposited into *other* protocols. That composability is a large part of what makes DeFi DeFi.

Our bridge does **not** do this. The claim lives as a `string`-keyed entry in the `deposits` log and as a balance on the game chain — it is *not* an on-chain ERC20, so it can't be transferred between Ethereum addresses or composed with other protocols. That's a deliberate trade: the whole point of the design is to keep the game's balances *off* the expensive public ledger.

> **A receipt is only as good as its issuer.** The flip side of "the balance is a claim" is counterparty risk. The chips are worth the underlying *only if* the bridge actually holds it and honours withdrawals. If the bridge is drained, the IOU is worthless — like a note from an insolvent bank. This is why the withdrawal path is signature-gated and why the rough edges in the last section matter: the credibility of every in-game chip rests on the integrity of this one contract.

## So why not ERC-4626?

If we're issuing a transferable claim on a pooled asset, we've described — almost exactly — a **tokenized vault**, and Ethereum has a standard for precisely this shape: **ERC-4626**, the Tokenized Vault Standard. A 4626 vault takes deposits of an underlying ERC20 and mints **shares** (themselves an ERC20) representing a proportional claim on the vault's holdings, behind a standard interface: `deposit`, `mint`, `withdraw`, `redeem`, and the conversions between assets and shares. Aave, Yearn, and most modern vaults either implement it or look very much like it.

Recall that `CosmosBridge` already *wears* an ERC20 mask — `name`, `symbol`, `decimals`, `totalSupply` — without being a token. ERC-4626 is what it would take to make that mask real: actual transferable shares, minted on deposit and burned on withdrawal, with on-chain accounting of who owns what.

So why didn't we just write a 4626 vault? Because of **where the accounting lives**. ERC-4626 keeps the share ledger *on-chain* — minting and burning shares is part of every deposit and withdrawal. Our design deliberately keeps the player's balance **off-chain**, on the game chain, because that's where the game already runs. Putting share accounting on-chain would pull game state back onto the slow, expensive, public ledger we spent the start of this section arguing *against*.

There are real trade-offs in that choice — composability, transparency, and trust assumptions all shift depending on where the IOU is issued — and ERC-4626 is worth understanding properly before deciding. We'll give it a full treatment of its own later, when we look at vault standards in depth. For now, the mental model is the thing to keep: **a deposit produces a claim on a pooled asset**, your in-game balance *is* that claim, and the same pattern — Aave's aTokens, Lido's stETH, Uniswap LP tokens — is everywhere in DeFi once you learn to see it.
