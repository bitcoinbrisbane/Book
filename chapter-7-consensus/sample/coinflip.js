const crypto = require("crypto");

// Function to generate a hash as a commitment
function commit(value, nonce) {
  return crypto
    .createHash("sha256")
    .update(value + nonce)
    .digest("hex");
}

// Function to simulate Alice's part
function aliceCommit() {
  const a = Math.floor(Math.random() * 2); // Alice's random bit (0 or 1)
  const nonce = crypto.randomBytes(16).toString("hex"); // Alice's random nonce for commitment
  const commitment = commit(a.toString(), nonce); // Alice's commitment (hash of a + nonce)
  console.log(`Alice's Commitment (sent to Bob): ${commitment}`);
  return { a, nonce, commitment };
}

// Function to simulate Bob's part
function bobChoice() {
  const b = Math.floor(Math.random() * 2); // Bob's random bit (0 or 1)
  console.log(`Bob's Choice (sent to Alice): ${b}`);
  return b;
}

// Function to reveal Alice's bit and verify the commitment
function verifyAlice(a, nonce, commitment) {
  const recomputedCommitment = commit(a.toString(), nonce);
  return recomputedCommitment === commitment;
}

// Simulate the coin flip protocol
function coinFlip() {
  // Step 1: Alice commits to her bit
  const { a, nonce, commitment } = aliceCommit();

  // Step 2: Bob picks his bit
  const b = bobChoice();

  // Step 3: Alice reveals her bit and nonce
  console.log(`Alice Reveals: a = ${a}, nonce = ${nonce}`);

  // Step 4: Bob verifies Alice's commitment
  if (verifyAlice(a, nonce, commitment)) {
    console.log("Commitment is valid, no cheating detected.");

    // Step 5: Determine the result of the coin flip (a XOR b)
    const result = a ^ b; // XOR operation
    console.log(`Coin Flip Result: ${result === 0 ? "Heads" : "Tails"}`);
  } else {
    console.log("Commitment verification failed. Cheating detected!");
  }
}

// Run the simulation
coinFlip();
