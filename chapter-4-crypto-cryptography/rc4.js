// RC4 implementation in Node.js

function rc4(key, plaintext) {
    // Initialize state array S
    let S = [];
    for (let i = 0; i < 256; i++) {
        S[i] = i;
    }

    // Key scheduling algorithm (KSA)
    let j = 0;
    for (let i = 0; i < 256; i++) {
        j = (j + S[i] + key.charCodeAt(i % key.length)) % 256;
        [S[i], S[j]] = [S[j], S[i]]; // Swap S[i] and S[j]
    }

    // Pseudo-random generation algorithm (PRGA)
    let i = 0;
    j = 0;
    let keystream = [];
    for (let k = 0; k < plaintext.length; k++) {
        i = (i + 1) % 256;
        j = (j + S[i]) % 256;
        [S[i], S[j]] = [S[j], S[i]]; // Swap S[i] and S[j]
        let t = (S[i] + S[j]) % 256;
        keystream.push(S[t]);
    }

    // XOR plaintext with keystream to get ciphertext
    let ciphertext = "";
    for (let k = 0; k < plaintext.length; k++) {
        ciphertext += String.fromCharCode(plaintext.charCodeAt(k) ^ keystream[k]);
    }

    return ciphertext;
}

// Example usage
const key = "mysecretkey";
const plaintext = "Hello, World!";
const ciphertext = rc4(key, plaintext);
console.log("Ciphertext (in base64):", Buffer.from(ciphertext).toString('base64'));

// To decrypt, use the same key and the ciphertext as input
const decryptedText = rc4(key, ciphertext);
console.log("Decrypted text:", decryptedText);
