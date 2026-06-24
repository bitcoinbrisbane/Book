# Sepolia deployments

Live proof that the Chapter 8 Greeter compiler works end to end. Both contracts
are on the Sepolia testnet (chain id 11155111).

## Our toy-compiler contract

Deployed from the raw bytecode emitted by `python/greeter_compiler.py`, with no
`solc` in the pipeline.

| Field | Value |
|---|---|
| Address | `0x7e2b169a181484b5108b7579f3f0c10d40cc7651` |
| Deploy tx | `0x68aa0212a5ae43a306ee4ac79bbdaede82754cfba48204f9aa06299d9ea2a1fd` |
| Block | 11127522 |
| Gas used | 86718 |
| `setGreeting(99)` tx | `0xcb1ab4c4cf095dff7dfa4e2db1166210c1190bd23c279fa248d76efcefdab5ac` |

Explorer: https://sepolia.etherscan.io/address/0x7e2b169a181484b5108b7579f3f0c10d40cc7651

Behaviour confirmed on-chain: `greet()` returned `42` after deploy, then `99`
after `setGreeting(99)`.

## The solc twin

The same `Greeter.sol` compiled by `solc` 0.8.24 via `forge`, deployed so it can
be source-verified on Etherscan (our hand-rolled bytecode can't match solc's, so
the verified page lives on this twin).

| Field | Value |
|---|---|
| Address | `0xC5e798Bc34e1dd61AA021daeD1Ef4A17a62905A4` |
| Deploy tx | `0x32e36b0f9c79dbd3e5b4522796677e57d241b13163fcc5ac7bacfa1424b1494e` |

Explorer: https://sepolia.etherscan.io/address/0xC5e798Bc34e1dd61AA021daeD1Ef4A17a62905A4

Verify (needs an Etherscan API key):

```sh
forge verify-contract 0xC5e798Bc34e1dd61AA021daeD1Ef4A17a62905A4 \
  src/Greeter.sol:Greeter --chain sepolia \
  --etherscan-api-key $ETHERSCAN_API_KEY --watch
```

## Deployer

Address `0x13F439EF673E7A864A43e9dD401a25D53A793caC`, a throwaway key generated
with `openssl` for this testnet demo only. Not used for anything else, ever.
