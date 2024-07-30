const Libp2p = require("libp2p");
const { TCP } = require("libp2p-tcp");
const { MPLEX } = require("libp2p-mplex");
const { SECIO } = require("libp2p-secio");
const { Noise } = require("libp2p-noise");
const { Multiaddr } = require("multiaddr");
const { MDNS } = require("libp2p-mdns");

async function createLibp2pNode() {
  const node = await Libp2p.create({
    modules: {
      transport: [TCP],
      streamMuxer: [MPLEX],
      connEncryption: [SECIO, Noise],
      peerDiscovery: [MDNS],
    },
    config: {
      peerDiscovery: {
        mdns: {
          interval: 1000,
          enabled: true,
        },
      },
    },
  });

  return node;
}

createLibp2pNode()
  .then((node) => {
    console.log("Libp2p node started");
    console.log("Listening on:", node.peerId.toB58String());
    node.listen(["/ip4/0.0.0.0/tcp/0"]); // Listen on a random available port

    // Discover peers and connect to them
    node.on("peer:discovery", (peerId) => {
      console.log("Discovered new peer:", peerId.toB58String());
      // Attempt to connect to the discovered peer
      node
        .dial(peerId)
        .catch((err) => console.error("Failed to connect to peer", err));
    });

    // Handle incoming connections
    node.connectionManager.on("peer:connect", (connection) => {
      console.log("Connected to peer:", connection.remotePeer.toB58String());
    });
  })
  .catch((err) => {
    console.error("Failed to start libp2p node", err);
  });
