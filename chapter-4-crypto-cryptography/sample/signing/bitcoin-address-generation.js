const crypto = require("crypto");

// Step 1: Generate or use a private key (32 bytes)
function generatePrivateKey() {
    // In practice, use crypto.randomBytes(32) for production
    // Using a fixed key for demonstration
    return Buffer.from(
        "18e14a7b6a307f426a94f8114701e7c8e774e7f9a47e2c2035db29a206321725",
        "hex"
    );
}

// Step 2: Derive public key from private key using secp256k1
// Note: This requires the "secp256k1" npm package
// npm install secp256k1
function getPublicKey(privateKey) {
    const secp256k1 = require("secp256k1");

    // Get uncompressed public key (65 bytes: 0x04 + x + y)
    const publicKeyUncompressed = secp256k1.publicKeyCreate(privateKey, false);

    // Get compressed public key (33 bytes: 0x02/0x03 + x)
    const publicKeyCompressed = secp256k1.publicKeyCreate(privateKey, true);

    console.log("Public Key (uncompressed):", publicKeyUncompressed.toString("hex"));
    console.log("Public Key (compressed):  ", publicKeyCompressed.toString("hex"));

    return publicKeyCompressed; // Bitcoin uses compressed keys now
}

// Step 3: SHA-256 hash
function sha256(buffer) {
    return crypto.createHash("sha256").update(buffer).digest();
}

// Step 4: RIPEMD-160 hash
function ripemd160(buffer) {
    return crypto.createHash("ripemd160").update(buffer).digest();
}

// Step 5: Create public key hash (hash160)
function hash160(buffer) {
    // SHA-256 followed by RIPEMD-160
    const sha = sha256(buffer);
    const ripe = ripemd160(sha);
    console.log("Hash160 (RIPEMD160(SHA256(pubKey))):", ripe.toString("hex"));
    return ripe;
}

// Step 6: Add version byte and checksum
function addVersionAndChecksum(hash160, version = 0x00) {
    // Version byte: 0x00 for mainnet, 0x6f for testnet
    const versionedHash = Buffer.concat([Buffer.from([version]), hash160]);
    console.log("Versioned hash:", versionedHash.toString("hex"));

    // Checksum: first 4 bytes of double SHA-256
    const checksum = sha256(sha256(versionedHash)).slice(0, 4);
    console.log("Checksum:", checksum.toString("hex"));

    // Combine: version + hash160 + checksum
    const addressBytes = Buffer.concat([versionedHash, checksum]);
    console.log("Address bytes:", addressBytes.toString("hex"));

    return addressBytes;
}

// Step 7: Base58 encoding
function base58Encode(buffer) {
    const ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

    // Convert bytes to big integer
    let num = BigInt("0x" + buffer.toString("hex"));
    let encoded = "";

    // Convert to base58
    while (num > 0n) {
        const remainder = num % 58n;
        num = num / 58n;
        encoded = ALPHABET[Number(remainder)] + encoded;
    }

    // Add "1" for each leading zero byte
    for (let i = 0; i < buffer.length && buffer[i] === 0; i++) {
        encoded = "1" + encoded;
    }

    return encoded;
}

// Complete Bitcoin address generation
function generateBitcoinAddress() {
    console.log("Bitcoin Address Generation\n" + "=".repeat(50) + "\n");

    // Step 1: Private key
    const privateKey = generatePrivateKey();
    console.log("Step 1 - Private Key:");
    console.log("  Hex:", privateKey.toString("hex"));
    console.log("  Decimal:", BigInt("0x" + privateKey.toString("hex")).toString());
    console.log();

    // Step 2: Public key (secp256k1)
    console.log("Step 2 - Public Key (ECDSA secp256k1):");
    const publicKey = getPublicKey(privateKey);
    console.log();

    // Step 3 & 4: Hash the public key (SHA-256 then RIPEMD-160)
    console.log("Step 3 & 4 - Hash Public Key:");
    const pkHash = hash160(publicKey);
    console.log();

    // Step 5: Add version and checksum
    console.log("Step 5 - Add Version (0x00) and Checksum:");
    const addressBytes = addVersionAndChecksum(pkHash, 0x00);
    console.log();

    // Step 6: Base58 encode
    console.log("Step 6 - Base58 Encoding:");
    const address = base58Encode(addressBytes);
    console.log("  Bitcoin Address:", address);
    console.log();

    return address;
}

// Run the example
console.log("Example: Generating Bitcoin Address from Private Key\n");
const address = generateBitcoinAddress();

console.log("\n" + "=".repeat(50));
console.log("FINAL RESULT");
console.log("=".repeat(50));
console.log("Bitcoin Address:", address);
console.log("\nThis address can now receive Bitcoin!");
console.log("The private key is needed to spend from this address.");

// Demonstrate with different private key
console.log("\n\n" + "=".repeat(50));
console.log("BONUS: Generate Random Address");
console.log("=".repeat(50));
console.log("\nGenerating address from random private key...\n");

// Generate random private key (UNCOMMENT FOR REAL RANDOM ADDRESS)
// const randomPrivateKey = crypto.randomBytes(32);
// const randomPublicKey = getPublicKey(randomPrivateKey);
// const randomAddress = base58Encode(addVersionAndChecksum(hash160(randomPublicKey)));
// console.log("Random Bitcoin Address:", randomAddress);

console.log("\nNote: The address generated depends on the private key used.");
console.log("Different private keys generate different addresses.");
console.log("\nEach step is deterministic:");
console.log("  Same private key → Same public key → Same address");
console.log("\n⚠️  SECURITY WARNING:");
console.log("  - Never use predictable private keys");
console.log("  - This code is for educational purposes only");
console.log("  - Real Bitcoin addresses require secure key generation");
