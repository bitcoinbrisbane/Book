const crypto = require("crypto");

// Define the key and IV (Initialization Vector)
const key = Buffer.from("0123456789abcdef01234567", "utf8"); // 24 bytes key for 3DES
const iv = Buffer.from("12345678", "utf8"); // 8 bytes IV for DES

// Function to encrypt plaintext using 3DES
function encrypt(text) {
  const cipher = crypto.createCipheriv("des-ede3-cbc", key, iv);
  let encrypted = cipher.update(text, "utf8", "base64");
  encrypted += cipher.final("base64");
  return encrypted;
}

// Function to decrypt ciphertext using 3DES
function decrypt(encryptedText) {
  const decipher = crypto.createDecipheriv("des-ede3-cbc", key, iv);
  let decrypted = decipher.update(encryptedText, "base64", "utf8");
  decrypted += decipher.final("utf8");
  return decrypted;
}

// Add a timer
console.time("3DES");

// Test the encryption and decryption
const plaintext = "Hello, this is a plaintext message!";
console.log("Plaintext:", plaintext);

const encryptedText = encrypt(plaintext);
console.log("Encrypted:", encryptedText);

const decryptedText = decrypt(encryptedText);
console.log("Decrypted:", decryptedText);

console.timeEnd("3DES");