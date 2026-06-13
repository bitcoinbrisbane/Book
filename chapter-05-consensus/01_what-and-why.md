# What is Consensus and Why Does it Matter?

Defining a consensus mechanism could be a book within itself. There could be infinite ways in which a network of people or machines agree on the state of the network. But first of all, we need to get a better understanding of what a consensus mechanism is and why it's important with respect to distributed systems such as blockchains.

Consensus is the task of getting a group of computers (a.k.a. nodes) to come to an agreement on a decision. In blockchain, this means that all the participants in a network have to agree on the changes made to the shared ledger. This agreement is reached through a specific process, a consensus protocol, that ensures that everyone sees the same information and that the information is accurate and trustworthy.

When I worked at ConsenSys, my colleague Tom described a consensus mechanism this way. I'll adapt to the theme of poker:

Let's say Alice and Bob are playing poker. Each player witnesses the deal, the flop, the turn and the river. Once we reach showdown and the hole cards are revealed, consensus on who won is reached. Obviously, sometimes arguments can occur, but for this thought experiment, let's assume not.

Now, can we do this over the phone? The coin flip over the phone problem involves two parties (let's call them Alice and Bob) who want to simulate a fair coin flip, but they can't meet in person. Instead, they communicate over an untrusted communication channel (e.g., the phone or internet), where one party could cheat. The goal is to design an algorithm where neither party can manipulate the outcome, ensuring a fair result.

The most common solution to this problem is based on commitment schemes, specifically Blum's protocol, which ensures fairness by using cryptography to prevent either party from cheating.

## Blum's Protocol

### 1. Alice commits to a bit (0 or 1):

- Alice picks a random bit `a` (either 0 or 1), which represents her initial choice for the coin flip.
- Alice generates a cryptographic commitment to `a` using a commitment function `C(a, r)`, where `r` is a random number (called a nonce) used to secure the commitment.
- Alice sends the commitment `C(a, r)` to Bob. The commitment function hides `a` while ensuring that Alice can't change her choice later (she is "committed" to `a`).

### 2. Bob chooses a bit:

- Bob picks a random bit `b` (either 0 or 1), which represents his choice for the coin flip.
- Bob sends his choice `b` to Alice.

### 3. Alice reveals her bit:

- After receiving Bob's choice, Alice reveals both her bit `a` and the random value `r` she used in the commitment.
- Alice sends `a` and `r` to Bob.

### 4. Bob verifies the commitment:

- Bob checks if the commitment `C(a, r)` matches the value Alice originally sent. If it does, this confirms that Alice did not change her bit after learning Bob's bit.

### 5. Determine the result:

- The result of the coin flip is computed as `a ⊕ b` (XOR of Alice's bit and Bob's bit). This gives a fair result, as each outcome (0 or 1) has an equal probability of occurring.

## Example

Now let's see this as an example with some JavaScript code:

**Commitment Phase:**
- Alice picks `a = 0` (for heads) and generates a cryptographic commitment: `C(a, r)`.
- Alice sends the commitment to Bob (something like a cryptographic hash).

**Bob's Choice:**
- Bob picks `b = 1` (for tails) and sends it to Alice.

**Reveal Phase:**
- Alice reveals her choice `a = 0` and the random nonce `r` used to create the commitment.
- Bob verifies that the commitment matches Alice's original message.

**Outcome:**
- The result of the coin flip is `0 ⊕ 1 = 1` (tails). The coin flip result is fair.

```javascript
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
```

Although these are cryptography examples of message protocols, they're also protocols in which the players, or nodes on the network, can agree on the state of the network. Or reach consensus.
