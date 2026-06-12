## Bridging

To allow our users to have tokens on our **Layer 2**, we use a method called **bridging**. Layer 2s like [Arbitrum](https://arbitrum.io/) and [Polygon](https://polygon.technology/) face the same problem: assets exist on Ethereum mainnet, but the user wants them on the L2. The mainstream solution is a *bridge contract* on mainnet plus a counterpart on the L2, with off-chain infrastructure that watches one and acts on the other.

Say you have USDC on Ethereum mainnet, but you want it on a Layer 2 like Arbitrum or Polygon. You can use a third-party bridge to do that. **Be aware that there is counterparty risk** — the bridge holds your funds on mainnet while the equivalent is minted on the L2; if the bridge operator is compromised, the mainnet funds may be drained without the L2 side ever notifying anyone. Bridge exploits (Ronin, Wormhole, Nomad) have collectively cost users over $2.5 billion. Whose security model the bridge follows matters as much as the chain you're bridging to.

For our chain, the mechanic is straightforward: a **Web2 process listens for `Deposited` events on our mainnet bridge contract** and performs the inverse operation on the Layer 2 — minting an equivalent amount of the wrapped token to the depositor's address, minus a small fee.

## The contract

Here's the basic bridge contract:

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;

import { ECDSA } from "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { IValidator } from "./Vault.sol";
import { IOracle } from "./Oracle.sol";

contract Bridge {
    using ECDSA for bytes32;

    address public  immutable underlying;
    address public  immutable vault;
    address private immutable oracle;
    address private immutable _self;
    uint256 public  immutable lockTime;

    mapping(address => uint256) public  lockTimes;
    mapping(address => uint256) public  balances;
    mapping(bytes32 => bool)    private usedNonces;

    constructor(address _underlying, address _vault, address _oracle, uint256 _lockTime) {
        underlying = _underlying;
        vault      = _vault;
        oracle     = _oracle;
        _self      = address(this);
        lockTime   = _lockTime;
    }

    function deposit(uint256 amount) external {
        IERC20 token = IERC20(underlying);

        lockTimes[msg.sender] = block.timestamp + lockTime;
        token.transferFrom(msg.sender, _self, amount);
        balances[msg.sender] += amount;

        emit Deposited(msg.sender, amount);
    }

    function withdraw(uint256 amount, address to, bytes32 nonce, bytes32 signature) external {
        require(!usedNonces[nonce], "Bridge: nonce already used");
        require(block.timestamp >= lockTimes[to], "Bridge: funds are locked");
        require(IERC20(underlying).balanceOf(to) >= amount, "Bridge: insufficient balance");

        bytes32 message = keccak256(abi.encodePacked(to, amount, nonce));
        address signer  = ECDSA.recover(message, signature);
        require(IValidator(vault).isValidator(signer), "Bridge: invalid signature");

        balances[to] -= amount;
        IERC20 token = IERC20(underlying);
        token.transfer(to, amount);

        emit Withdrawn(to, amount);
    }

    event Deposited(address indexed account, uint256 amount);
    event Withdrawn(address indexed account, uint256 amount);
}
```

Let's look at the parts.

## Pragma and compiler version

```solidity
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.24;
```

The first line is the **SPDX licence identifier** (covered in Chapter 10). `UNLICENSED` means we're explicitly claiming no licence — appropriate for a contract still in development, where you don't want anyone copying it yet. When we publish, we'll switch to `MIT` or `Apache-2.0`.

The pragma pins us to Solidity `0.8.24` or any compatible patch release. We're past the era when overflow / underflow had to be guarded explicitly — Solidity has had checked arithmetic since 0.8.0, which removes a category of subtle bugs at the language level. Pinning to a recent patch lets us pick up compiler fixes without inviting breakage.

## Imports

```solidity
import { ECDSA }     from "@openzeppelin/contracts/utils/cryptography/ECDSA.sol";
import { IERC20 }    from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { IValidator } from "./Vault.sol";
import { IOracle }   from "./Oracle.sol";
```

Two from [OpenZeppelin](https://www.openzeppelin.com/contracts) (a widely-audited library), two from our own project:

- **`ECDSA`** — utility for recovering an address from a signature. Withdraw operations are authorised by validator signatures; this is how we verify them.
- **`IERC20`** — the ERC20 interface. The bridge interacts with the underlying token (USDC, USDT, etc.) through this.
- **`IValidator`** — our own interface for asking the validator vault "is this address an active validator?".
- **`IOracle`** — our exchange-rate oracle. The constructor wires it in, but the deposit path doesn't read from it yet — that comes when we cover L2 accounting on a later page.

The rest of the contract — state variables, deposit, withdraw, events — is enough material for the next page. We'll walk through the state and the deposit function first, then the withdraw flow with its signature check, then the events that the off-chain Web2 process listens to.
