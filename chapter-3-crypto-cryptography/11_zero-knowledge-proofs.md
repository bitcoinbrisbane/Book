# Zero Knowledge Proofs

Now let's discuss zero knowledge proofs. This is the definition from Wikipedia: "In cryptography, a zero-knowledge proof (also known as a ZK proof or ZKP) is a protocol in which one party (the prover) can convince another party (the verifier) that some given statement is true, without conveying to the verifier any information beyond the mere fact of that statement's truth."

I like the word "convince" here, rather than "prove."

## The Colorblind Ball Analogy

There are a few analogies that I like when explaining what Zero Knowledge proofs are.

Imagine you have two balls that appear identical to your colorblind friend - but you can see one is red and one is green. You want to prove to them that the balls are actually different, without revealing anything about how they're different (since you can't explain color to someone who's never seen it).

Here's the proof: Your friend holds both balls behind their back, randomly switches them around (or doesn't), then brings them forward. You tell them whether they switched or not. You repeat this 20 times, and you're correct every single time.

After enough rounds, your friend becomes convinced the balls must be different (the odds of you guessing right 20 times by chance are 1 in a million).

### Probability Calculation

```python
def probability_of_random_success(n_trials):
    """
    Calculate the probability of randomly guessing correctly n times in a row
    when each guess has a 50/50 chance (switched or not switched).
    """
    # Probability of one correct guess is 1/2
    # Probability of n correct guesses is (1/2)^n
    probability = (1/2) ** n_trials

    # Express as "1 in X"
    one_in_x = 1 / probability

    return {
        'probability': probability,
        'percentage': probability * 100,
        'one_in': one_in_x,
        'formatted': f"1 in {one_in_x:,.0f}"
    }

# Test with 20 trials
result = probability_of_random_success(20)
print(f"Probability: {result['probability']}")
print(f"Percentage: {result['percentage']:.6f}%")
print(f"Odds: {result['formatted']}")
```

The outputs are:
```
Probability: 9.5367431640625e-07
Percentage: 0.000095%
Odds: 1 in 1,048,576
```

Yet they still have zero knowledge about the actual difference - they never learned what "red" and "green" are, or anything about color. They just know the balls are distinguishable to you.

## Age Verification Problem

For a less abstract use case, consider the age verification "problem."

Let's say you want to enter your favorite bar, and the doorman (bouncer) needs to check your ID for your age. Your age is actually a function of your birthday. However, by revealing your birthday you're actually giving away more metadata than you may be comfortable with.

The question is "are you above this age?", not "what is your birthday?"

## Application to Card Games

Now we have a basic understanding of what they are, perhaps you can already guess how they apply to a card game? Having a shuffled deck that we can prove is shuffled, but not know what the deck state is, is an interesting part of the system and something we can solve with ZK.

## Terminology

- **Prover**: The party that wants to prove a statement is true
- **Verifier**: The party that needs to be convinced
- **Gadget**: A reusable circuit component
- **Circuit**: The mathematical representation of the computation being proven

## Writing a Zero Knowledge Circuit

Let's try write that proof as a circuit. There's a language/framework called Circom that we use.

### Fisher-Yates Shuffle in ZK

As our book is heavily card-focused, we will use the Fisher-Yates algorithm for shuffling the deck. While naive approaches like repeated random swaps or sorting by random keys can introduce bias or be computationally expensive, the **Fisher-Yates (Knuth) shuffle** stands out as the gold standard for creating uniformly random permutations.

Invented by Ronald Fisher and Frank Yates in 1938 and later popularized by Donald Knuth, the algorithm works by iterating through the array from the first element to the second-to-last, and at each position i, selecting a random index j from i to n-1 and swapping the elements at positions i and j.

This approach guarantees that every possible permutation of n elements has an equal probability of 1/n! of being generated, making it perfectly unbiased. With O(n) time complexity and O(1) space complexity (when shuffling in-place), the algorithm is **deterministic** - given the same sequence of random values, it produces the same shuffle - making it ideal for cryptographic verification.

### Circom Circuit Example

```circom
pragma circom 2.0.0;

include "circomlib/circuits/comparators.circom";
include "circomlib/circuits/gates.circom";

// Fisher-Yates Shuffle Verification Circuit
// Proves that outputDeck is a valid permutation of inputDeck
// using Fisher-Yates algorithm with given random values
template FisherYatesShuffle(n) {
    // n = number of cards (52 for standard deck)

    signal input inputDeck[n];     // Original ordered deck
    signal input outputDeck[n];    // Shuffled deck
    signal input randomValues[n];  // Random values used for shuffling (private)

    // Intermediate deck states during shuffling
    signal deck[n][n];

    // Initialize first row with input deck
    for (var i = 0; i < n; i++) {
        deck[0][i] <== inputDeck[i];
    }

    // ... (shuffle logic)

    // Verify final output matches the shuffled deck
    for (var i = 0; i < n; i++) {
        outputDeck[i] === deck[n-1][i];
    }
}

// Main component for standard 52-card deck
component main {public [inputDeck, outputDeck]} = FisherYatesShuffle(52);
```

### Compiling the Circuit

```bash
circom deck_shuffle.circom --r1cs --wasm --sym -o build
```

## What is WASM?

WebAssembly (WASM) is a type of code that can be run in modern web browsers. It is a low-level assembly-like language with a compact binary format that runs with near-native performance. It provides languages such as C/C++, C#, and Rust with a compilation target so that they can run on the web. It is also designed to run alongside JavaScript, allowing both to work together.

## Project Structure

```
zk-poker-shuffle/
├── circuits/
│   ├── deck_shuffle.circom          # Basic circuit
│   └── optimized_deck_shuffle.circom # Optimized version
│
├── public/
│   ├── circuits/
│   │   ├── deck_shuffle_js/
│   │   │   └── deck_shuffle.wasm    # Used in browser
│   │   ├── deck_shuffle.zkey        # Proving key
│   │   └── verification_key.json    # Verification key
│   └── demo.html                    # Browser demo
│
├── src/
│   ├── zkProof/
│   │   ├── browserProofGenerator.ts # Browser proof generation
│   │   ├── zkDeckShuffler.ts        # Shuffle implementation
│   │   └── zkShuffleHelpers.ts      # Advanced features
│   └── components/
│       └── PokerGame.tsx            # React component
│
├── scripts/
│   └── test-proof.js                # Test script
│
├── contracts/
│   └── DeckShuffleVerifier.sol      # Solidity verifier
│
└── package.json
```

## Browser Usage

Here is a simple HTML example:

```html
<script type="module">
import * as snarkjs from 'https://cdn.jsdelivr.net/npm/snarkjs@latest/build/snarkjs.min.js';

const { proof, publicSignals } = await snarkjs.groth16.fullProve(
    input,
    '/circuits/deck_shuffle.wasm',
    '/circuits/deck_shuffle.zkey'
);
</script>
```

The full React/Vite version and pure HTML version are available in the GitHub repository.
