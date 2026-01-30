## Understanding Blockchains Through State Management

Blockchains are collections of connected nodes that listen for state changes and then replicate the data to all other nodes. This is similar to how a replicated database works, but with key differences. Blockchains have been referred to as a good way to transfer value, but more precisely, blockchains are a way for machines to change the state of a system, while allowing users to manage that state via transactions signed by their private keys.

State can represent a number, which in turn can represent value. State variables can be of typical programming language types, such as booleans, numbers and strings, or complex types, data structures and objects that represent or model ideas or real world objects.

In 2026 there are database designs such as CouchDB, MongoDB, Cassandra, and PostgreSQL with replication features that provide similar data synchronization capabilities. However, blockchains add cryptographic verification, immutability, and decentralized consensus to the equation.

## Why Poker?

To understand these concepts in practice, we'll use poker as our teaching example. Poker is a turn-based game where each player agrees, or should agree, on the rules of the game. Outside of a casino, for example, each player witnesses the game and agrees that each turn or action was within the agreed rules of the game.

Each legal turn updates the state of the game. With poker, the state is managed by the deck of cards, the dealt hole cards, the players' chips and the chips in the pot. In our application these will be represented by updating the state of the chain.

Like most card games, there are also home and location variances of the game. These rules can be agreed upon by the players before the game starts. This is a perfect parallel to blockchain consensus mechanisms: just as players must agree on the rules before playing, blockchain nodes must agree on the protocol rules before processing transactions. In this book we will code a working example of this consensus in action, building a decentralized poker game that demonstrates how blockchain state management works in practice.
