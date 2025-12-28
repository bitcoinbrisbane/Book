// merkle-whitelist.js
const { MerkleTree } = require("merkletreejs");
const keccak256 = require("keccak256");
const { ethers } = require("ethers");

/**
 * Generate a Merkle tree from a list of whitelisted addresses
 */
class WhitelistMerkleTree {
  constructor(addresses) {
    // Convert addresses to leaf nodes (hash each address)
    this.leaves = addresses.map((addr) =>
      keccak256(ethers.solidityPacked(["address"], [addr])),
    );

    // Create the Merkle tree
    this.tree = new MerkleTree(this.leaves, keccak256, {
      sortPairs: true, // Important! Matches OpenZeppelin's behavior
    });

    this.addresses = addresses;
  }

  /**
   * Get the Merkle root (to store in smart contract)
   */
  getRoot = () => {
    return this.tree.getHexRoot();
  };

  /**
   * Get the Merkle proof for a specific address
   */
  getProof = (address) => {
    const leaf = keccak256(ethers.solidityPacked(["address"], [address]));
    return this.tree.getHexProof(leaf);
  };

  /**
   * Verify a proof (for testing)
   */
  verify = (address, proof) => {
    const leaf = keccak256(ethers.solidityPacked(["address"], [address]));
    const root = this.tree.getRoot();
    return this.tree.verify(proof, leaf, root);
  };

  /**
   * Generate a JSON file with all proofs
   */
  generateProofFile = () => {
    const proofs = {};

    this.addresses.forEach((address) => {
      proofs[address] = this.getProof(address);
    });

    return {
      merkleRoot: this.getRoot(),
      proofs: proofs,
    };
  };
}

// Example usage
const main = async () => {
  // Whitelist of addresses (in practice, this could be 10,000+ addresses)
  const whitelist = [
    "0x5B38Da6a701c568545dCfcB03FcB875f56beddC4", // Alice
    "0xAb8483F64d9C6d1EcF9b849Ae677dD3315835cb2", // Bob
    "0x4B20993Bc481177ec7E8f571ceCaE8A9e22C02db", // Carol
    "0x78731D3Ca6b7E34aC0F824c42a7cC18A495cabaB", // Dave
    "0x617F2E2fD72FD9D5503197092aC168c91465E7f2", // Eve
  ];

  console.log(`Creating Merkle tree for ${whitelist.length} addresses...\n`);

  // Create the Merkle tree
  const merkleTree = new WhitelistMerkleTree(whitelist);

  // Get the root to store in the smart contract
  const root = merkleTree.getRoot();
  console.log("Merkle Root (store this in smart contract):");
  console.log(root);
  console.log();

  // Generate proof for Bob
  const bobAddress = "0xAb8483F64d9C6d1EcF9b849Ae677dD3315835cb2";
  const bobProof = merkleTree.getProof(bobAddress);

  console.log(`Merkle Proof for ${bobAddress}:`);
  console.log(bobProof);
  console.log();

  // Verify the proof
  const isValid = merkleTree.verify(bobAddress, bobProof);
  console.log(`Proof valid: ${isValid}`);
  console.log();

  // Try with an address NOT on the whitelist
  const hackerAddress = "0x0000000000000000000000000000000000000000";
  const hackerProof = merkleTree.getProof(hackerAddress);
  const hackerValid = merkleTree.verify(hackerAddress, hackerProof);
  console.log(`Hacker proof valid: ${hackerValid}`);
  console.log();

  // Generate proofs for all addresses
  const proofsJson = merkleTree.generateProofFile();
  console.log("Generated proofs for all whitelisted addresses");
  console.log(`Number of proofs: ${Object.keys(proofsJson.proofs).length}`);

  // In production, save this to a file:
  // const fs = require("fs");
  // fs.writeFileSync("whitelist-proofs.json", JSON.stringify(proofsJson, null, 2));
};

main().catch(console.error);
