## Discovery layer

How will our nodes find each other in a mesh topology?

As our network of nodes is in a "mesh" style architecture, there must be a protocol to discover current nodes on the network.

The Bitcoin Core client has a set of DNS seed servers baked into the source code. They are custom implementations of the Berkeley (BIND) `chainparams.cpp`. Each client upon startup will call the node to find more peers. This is critical to the health of the network, so it's important the nodes are secure and have high availability.

Furthermore, the Bitcoin Core seed DNS policy can be found here: https://github.com/bitcoin/bitcoin/blob/master/doc/dnsseed-policy.md

## Hub and spoke vs mesh topology

Hub and spoke network topology and mesh topology represent two distinct approaches to organizing and connecting computer or network nodes. Each has its own advantages and vulnerabilities, and these differences become evident when considering their application in peer-to-peer networks, as demonstrated by the example of Napster.

A hub and spoke network topology is characterized by a central hub or server that connects to multiple spoke nodes. The spokes communicate with each other via the central hub or server(s). This design is simple, easy to manage, and often cost-effective. However, it comes with a single point of failure: if the hub goes down, the entire network may be disrupted. In the context of peer-to-peer networks like Napster, this centralized structure made it vulnerable to shutdown. Napster's centralized server was its Achilles' heel, allowing authorities to target and shut down the central server, effectively crippling the entire network.

On the other hand, a mesh network topology creates a decentralized and interconnected web of nodes, where each node can communicate directly with others. Mesh networks are resilient and fault-tolerant because there is no single point of failure. In the case of Napster, a mesh topology would have made it much more difficult to shut down. Even if one node or server were taken offline, other nodes could still communicate and share files independently, making it challenging for authorities to completely halt the network.

Napster's vulnerability due to its hub and spoke topology played a significant role in the platform's eventual legal troubles and shutdown. Subsequently, many decentralized and peer-to-peer networks, such as those based on blockchain technology, adopted mesh-like structures to avoid a single point of control and enhance their resilience against censorship and shutdown attempts.

## LimeWire and the Gnutella protocol

LimeWire, a popular peer-to-peer (P2P) file-sharing application, utilized peer discovery algorithms to enable users to connect with other peers and share music, as well as other types of files. The core purpose of these algorithms is to facilitate the process of locating and connecting to other users within the LimeWire network. Here's an overview of how LimeWire and similar P2P platforms employed these algorithms:

1. **Gnutella Protocol**: LimeWire was built upon the Gnutella protocol, which is a decentralized P2P network protocol. In Gnutella, each LimeWire client acts as both a client and a server, meaning it can both search for files on other users' computers and share files stored on its own. The Gnutella protocol utilizes a flooding-based search algorithm, where a search query from one user is flooded or broadcasted across the network. Other users receiving the query respond if they have matching files. This decentralized approach allowed LimeWire users to discover and connect with peers directly, without relying on a centralized server.

2. **Ultrapeer and Leaf Nodes**: In the Gnutella network, there is a distinction between Ultrapeer and Leaf nodes. Ultrapeers serve as more capable nodes that handle a larger number of connections and search queries. They maintain an index of shared files and help in routing search queries efficiently. Leaf nodes, on the other hand, are simpler and connect to Ultrapeers to access the network's resources. LimeWire's use of this hierarchical structure helped distribute the load and improve network efficiency.

3. **Query Routing**: When a user initiated a search for a specific file, the query was propagated through the network from Ultrapeer to Ultrapeer until a match was found. LimeWire's peer discovery algorithm facilitated this query routing, ensuring that users' requests reached their intended destinations efficiently. When a match was found, the corresponding file could be downloaded directly from the user who possessed it.

4. **Dynamic Connections**: LimeWire allowed dynamic connections to multiple peers simultaneously, further improving the efficiency and speed of file downloads. Users could connect to numerous peers, download parts of a file from each, and combine them to speed up the downloading process.

In essence, LimeWire's use of peer discovery algorithms, based on the Gnutella protocol, allowed users to connect directly to one another, search for files across the network, and share music and other content without relying on a central server. This decentralized approach was one of the key reasons for its popularity, as it enabled users to share and access a vast array of content easily, albeit within the boundaries of copyright and legal considerations.

## Discovering a node with libp2p

Let's write some code to discover a node on the network. For this, we can use the popular library libp2p. It provides the foundational tools and protocols needed for building decentralized, peer-to-peer applications. Originally developed by Protocol Labs, the same organization behind IPFS (InterPlanetary File System), libp2p aims to simplify and enhance the process of building distributed systems by offering a suite of components for networking.

```bash
yarn init -y

nvm install

yarn add p2plib

touch index.js
```

Let's create a Node.js file, add a seed. Then run this in a few different terminals to simulate. We will have 2 nodes in the bootstrap file and one other node to discover.

## What do Bitcoin and Ethereum use?

When searching for libraries, you'll find a lot of old libs. 7 to 12 years old.

Create our `index.js` file and add our imports:

Reference: https://docs.libp2p.io/guides/getting-started/javascript/

## Distributed hash tables?
