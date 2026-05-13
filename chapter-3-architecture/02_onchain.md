## What's On-Chain, What's Off-Chain?

In our introduction in Chapter 2, we briefly discussed how blockchains manage state through consensus mechanisms. An important architectural decision when building blockchain applications is determining which data and logic should live on-chain versus off-chain.

**On-chain** data and logic is:
- Validated by all nodes in the network
- Permanently recorded in the blockchain
- Subject to consensus rules
- More expensive to store and execute
- Guaranteed to be tamper-proof and verifiable

**Off-chain** data and logic is:
- Processed independently by individual nodes or external systems
- Not permanently stored in the blockchain (though proofs may be anchored on-chain)
- Faster and cheaper to execute
- Suitable for high-frequency operations or large data sets

For our poker application, the critical state changes—such as player actions, chip transfers, and card reveals—need to be on-chain to ensure all players agree on the game state. However, elements like player matchmaking, chat systems, or UI preferences can remain off-chain.

## Consensus Off-Chain?

While consensus traditionally happens on-chain through validators reaching agreement on the blockchain state, there are emerging patterns for off-chain consensus. These include:

- **Optimistic rollups**: Transactions are processed off-chain and assumed valid unless challenged (similar to our poker dealer example)
- **State channels**: Participants agree on state changes off-chain and only settle final results on-chain
- **Sidechains**: Independent chains with their own consensus that periodically checkpoint to a main chain

These approaches allow for higher throughput and lower costs while maintaining security guarantees through on-chain settlement and dispute resolution.

## Cosmos SDK

The Cosmos SDK is "a modular, open-source framework for building secure, high-performance distributed ledgers"[^1]. It enables developers to create purpose-built blockchains with custom business logic that can natively communicate with other chains.

### Purpose and Key Features

The Cosmos SDK addresses several critical blockchain development challenges:

**Modular Architecture**: Instead of building blockchain infrastructure from scratch, developers can use pre-built modules for standard functionality like staking, governance, tokenization, and account management. Custom modules can be created for application-specific logic[^2].

**Native Interoperability**: Chains built with Cosmos SDK include native integration with the Inter-Blockchain Communication Protocol (IBC), enabling seamless cross-chain data transfer and communication without additional bridging infrastructure[^1].

**High Performance**: Built on CometBFT consensus (formerly Tendermint), Cosmos SDK chains can achieve up to 10,000 transactions per second while maintaining Byzantine Fault Tolerance and instant transaction finality[^1].

**Full Customizability**: Developers have complete control over their blockchain's rules, state machine, and governance mechanisms. This makes Cosmos SDK ideal for application-specific blockchains where generic smart contract platforms may be too restrictive.

The framework is battle-tested, powering over 200 production blockchains including the Cosmos Hub, Osmosis, and numerous other networks in the Cosmos ecosystem[^2].

For our purposes, understanding the Cosmos SDK's modular approach helps illustrate how blockchain applications can be architected with clear separation of concerns—a pattern we'll apply when building our poker game.

[^1]: Cosmos SDK Documentation. "Introduction to Cosmos SDK". https://docs.cosmos.network/
[^2]: Cosmos SDK GitHub Repository. https://github.com/cosmos/cosmos-sdk
