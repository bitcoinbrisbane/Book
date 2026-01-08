const crypto = require("crypto");

// SHA-256 round constants (first 32 bits of fractional parts of cube roots of first 64 primes)
const K = [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
];

// SHA-256 internal functions for demonstration
function Ch(x, y, z) {
    // Choose: if x then y else z
    return (x & y) ^ (~x & z);
}

function Maj(x, y, z) {
    // Majority: true if at least 2 of x, y, z are true
    return (x & y) ^ (x & z) ^ (y & z);
}

function Sigma0(x) {
    // Right rotate by 2, 13, 22 and XOR
    return ((x >>> 2) | (x << 30)) ^ ((x >>> 13) | (x << 19)) ^ ((x >>> 22) | (x << 10));
}

function Sigma1(x) {
    // Right rotate by 6, 11, 25 and XOR
    return ((x >>> 6) | (x << 26)) ^ ((x >>> 11) | (x << 21)) ^ ((x >>> 25) | (x << 7));
}

function sigma0(x) {
    // Right rotate by 7, 18 and shift right by 3
    return ((x >>> 7) | (x << 25)) ^ ((x >>> 18) | (x << 14)) ^ (x >>> 3);
}

function sigma1(x) {
    // Right rotate by 17, 19 and shift right by 10
    return ((x >>> 17) | (x << 15)) ^ ((x >>> 19) | (x << 13)) ^ (x >>> 10);
}

// Example usage showing SHA-256 properties
console.log("SHA-256 Hash Function Demonstration");
console.log("===================================\n");

console.log("1. Deterministic - Same input always produces same output:");
const input1 = "Hello, World!";
const hash1a = crypto.createHash("sha256").update(input1).digest("hex");
const hash1b = crypto.createHash("sha256").update(input1).digest("hex");
console.log("   Input:", input1);
console.log("   Hash1:", hash1a);
console.log("   Hash2:", hash1b);
console.log("   Match:", hash1a === hash1b);
console.log();

console.log("2. Avalanche Effect - Small change drastically changes output:");
const input2a = "Hello, World!";
const input2b = "Hello, World?";  // Only last character changed
const hash2a = crypto.createHash("sha256").update(input2a).digest("hex");
const hash2b = crypto.createHash("sha256").update(input2b).digest("hex");
console.log("   Input A:", input2a);
console.log("   Hash A: ", hash2a);
console.log("   Input B:", input2b);
console.log("   Hash B: ", hash2b);

// Count different bits
let differentBits = 0;
for (let i = 0; i < hash2a.length; i += 2) {
    const byteA = parseInt(hash2a.substr(i, 2), 16);
    const byteB = parseInt(hash2b.substr(i, 2), 16);
    const xor = byteA ^ byteB;
    for (let bit = 0; bit < 8; bit++) {
        if ((xor >> bit) & 1) differentBits++;
    }
}
console.log("   Different bits:", differentBits, "out of 256 (" + (differentBits / 256 * 100).toFixed(1) + "%)");
console.log();

console.log("3. One-way - Cannot reverse hash to get original:");
const input3 = "Secret message";
const hash3 = crypto.createHash("sha256").update(input3).digest("hex");
console.log("   Input:", input3);
console.log("   Hash: ", hash3);
console.log("   (There is no function to reverse this!)");
console.log();

console.log("4. Fixed length - Output is always 256 bits (64 hex chars):");
const inputs = ["a", "Hello, World!", "A".repeat(1000)];
inputs.forEach(input => {
    const hash = crypto.createHash("sha256").update(input).digest("hex");
    console.log("   Input length:", input.length.toString().padStart(4), "→ Hash length:", hash.length);
});
console.log();

console.log("5. SHA-256 Internal Functions (for demonstration):");
const a = 0x6a09e667;  // Example value
const b = 0xbb67ae85;
const c = 0x3c6ef372;
console.log("   Ch(a, b, c)  = 0x" + (Ch(a, b, c) >>> 0).toString(16).padStart(8, "0"));
console.log("   Maj(a, b, c) = 0x" + (Maj(a, b, c) >>> 0).toString(16).padStart(8, "0"));
console.log("   Σ0(a)        = 0x" + (Sigma0(a) >>> 0).toString(16).padStart(8, "0"));
console.log("   Σ1(a)        = 0x" + (Sigma1(a) >>> 0).toString(16).padStart(8, "0"));
console.log("   σ0(a)        = 0x" + (sigma0(a) >>> 0).toString(16).padStart(8, "0"));
console.log("   σ1(a)        = 0x" + (sigma1(a) >>> 0).toString(16).padStart(8, "0"));
console.log();

console.log("6. Bitcoin Example - Double SHA-256:");
const blockHeader = "Bitcoin Block Header";
const hash1 = crypto.createHash("sha256").update(blockHeader).digest();
const hash2 = crypto.createHash("sha256").update(hash1).digest("hex");
console.log("   Input:       ", blockHeader);
console.log("   SHA-256:     ", hash1.toString("hex"));
console.log("   SHA-256(^2): ", hash2);
console.log("   (Bitcoin uses double SHA-256 for extra security)");
