import { createLibp2p } from "libp2p";
import { bootstrap } from "@libp2p/bootstrap";

const libp2p = await createLibp2p({
	peerDiscovery: [
		bootstrap({
			list: [
				// a list of bootstrap peer multiaddrs to connect to on node startup
				"/ip4/104.131.131.82/tcp/4001/ipfs/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ",
				"/dnsaddr/bootstrap.libp2p.io/ipfs/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN",
				"/dnsaddr/bootstrap.libp2p.io/ipfs/QmQCU2EcMqAqQPR2i9bChDtGNJchTbq5TbXJJ16u19uLTa",
			],
		}),
	],
});

libp2p.addEventListener("peer:discovery", (evt) => {
	console.log("found peer: ", evt.detail.toString());
});
