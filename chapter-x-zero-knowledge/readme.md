# 🃏 Zero-Knowledge Deck Shuffle for Poker

A complete implementation of provably fair deck shuffling using Fisher-Yates algorithm with zero-knowledge proofs in the browser via WebAssembly.

## 🎯 What This Does

This system allows you to:
- **Shuffle a deck** using the cryptographically secure Fisher-Yates algorithm
- **Generate a ZK proof** that the shuffle was performed correctly
- **Verify the proof** without revealing the random values used
- **Run entirely in the browser** using WebAssembly (WASM)
- **Verify on-chain** using Solidity smart contracts

## 📦 Quick Start

### Installation

```bash
# Clone your project
git clone <your-repo>
cd zk-poker-shuffle

# Install dependencies
yarn install

# Install circom globally (if not already installed)
yarn install -g circom snarkjs
```

### Build Everything (First Time Setup)

```bash
# This will:
# 1. Compile the circuits
# 2. Generate the trusted setup
# 3. Create proving/verification keys
# 4. Export Solidity verifier
yarn run build:all
```

This takes about 5-10 minutes on first run.

### Quick Test

```bash
# Test proof generation in Node.js
yarn run test:proof

# Start development server
yarn run dev

# Open browser demo
yarn run serve:demo
# Then visit: http://localhost:8080/demo.html
```

## 📁 Project Structure

```
zk-poker-shuffle/
├── circuits/
│   ├── deck_shuffle.circom           # Basic circuit
│   └── optimized_deck_shuffle.circom # Optimized version
│
├── public/
│   ├── circuits/
│   │   ├── deck_shuffle_js/
│   │   │   └── deck_shuffle.wasm    # ← Used in browser
│   │   ├── deck_shuffle.zkey         # Proving key
│   │   └── verification_key.json     # Verification key
│   └── demo.html                      # Browser demo
│
├── src/
│   ├── zkProof/
│   │   ├── browserProofGenerator.ts  # Browser proof generation
│   │   ├── zkDeckShuffler.ts         # Shuffle implementation
│   │   └── zkShuffleHelpers.ts       # Advanced features
│   └── components/
│       └── PokerGame.tsx              # React component
│
├── scripts/
│   └── test-proof.js                  # Test script
│
├── contracts/
│   └── DeckShuffleVerifier.sol       # Solidity verifier
│
└── package.json
```

## 🚀 Usage

### 1. Browser Usage (Simplest)

Open `public/demo.html` in your browser:

```bash
yarn run serve:demo
# Visit: http://localhost:8080/demo.html
```

Click through:
1. **Shuffle Deck** - Performs Fisher-Yates shuffle
2. **Generate ZK Proof** - Creates proof in browser (2-10 seconds)
3. **Verify Proof** - Verifies the proof (50-200ms)

### 2. React/TypeScript Usage

```typescript
import { BrowserZKProofGenerator } from './zkProof/browserProofGenerator';

const generator = new BrowserZKProofGenerator();
await generator.preload(); // Load circuit files

// Shuffle and prove
const { proof, publicSignals } = await generator.generateShuffleProof(
  inputDeck,    // [0, 1, 2, ..., 51]
  outputDeck,   // Shuffled version
  randomValues  // Random values used
);

// Verify
const isValid = await generator.verifyProof(proof, publicSignals);
console.log('Proof valid:', isValid);
```

### 3. React Hook

```typescript
import { useZKProofGenerator } from './zkProof/browserProofGenerator';

function MyPokerGame() {
  const { generateProof, verifyProof, isLoading, error } = useZKProofGenerator();

  const handleShuffle = async () => {
    const result = await generateProof(inputDeck, outputDeck, randomValues);
    console.log('Proof generated in', result.proofTime, 'ms');
  };

  return (
    <button onClick={handleShuffle} disabled={isLoading}>
      {isLoading ? 'Generating proof...' : 'Shuffle & Prove'}
    </button>
  );
}
```

### 4. Integration with TexasHoldemGame

```typescript
import { ZKTexasHoldemIntegration } from './zkProof/zkDeckShuffler';

const integration = new ZKTexasHoldemIntegration();

// Generate shuffled deck for new hand
const { deckString, proofData } = await integration.prepareNewHand(currentDeck);

// Use in your game
game.reInit(deckString);

// Verify the shuffle was fair
const isValid = await integration.verifyCurrentShuffle();
```

## 🔧 Build Commands

### Circuit Compilation

```bash
# Compile all circuits
yarn run compile:circuit

# Compile specific circuit
yarn run compile:circuit:basic
yarn run compile:circuit:optimized
```

### Trusted Setup

```bash
# Full setup (Powers of Tau + circuit-specific)
yarn run setup:ptau
yarn run setup:zkey

# Export verification keys
yarn run export:vkey

# Export Solidity verifier
yarn run export:solidity
```

### Testing

```bash
# Test proof generation (Node.js)
yarn run test:proof

# Run development server
yarn run dev

# Serve static demo
yarn run serve:demo
```

## 📊 Performance Benchmarks

| Operation | Time | Size |
|-----------|------|------|
| Circuit Compilation | 30-60s | - |
| WASM Load (first time) | 100-500ms | 2-5 MB |
| WASM Load (cached) | <50ms | - |
| Proof Generation | 2-10s | - |
| Proof Verification | 50-200ms | - |
| Proving Key (.zkey) | - | 10-50 MB |

## 🌐 Browser Compatibility

| Browser | Support | Notes |
|---------|---------|-------|
| Chrome/Edge | ✅ Full | Best performance |
| Firefox | ✅ Full | Slightly slower |
| Safari | ✅ Full | Requires CORS headers |
| Mobile Safari | ⚠️ Limited | Slower on older devices |
| Mobile Chrome | ✅ Full | Good performance |

## 🔐 Security Considerations

### Trusted Setup

The current setup uses a single contribution. For production:

1. **Multi-party ceremony**: Get multiple parties to contribute
2. **Use existing ceremony**: Use Powers of Tau from Zcash/Hermez
3. **Verify contributions**: Check all contributions are valid

```bash
# Multi-party contribution example
snarkjs powersoftau contribute pot.ptau pot_1.ptau --name="Party 1"
snarkjs powersoftau contribute pot_1.ptau pot_2.ptau --name="Party 2"
# ... continue for each party
```

### Randomness Source

Currently uses `crypto.getRandomValues()` (browser) or `crypto.randomBytes()` (Node.js).

For enhanced security:
- Use **hardware RNG** if available
- Implement **multi-party randomness** (see `zkShuffleHelpers.ts`)
- Consider **VRF** (Verifiable Random Function) for blockchain

### Privacy

The ZK proof reveals:
- ✅ **Public**: Input deck, output deck
- 🔒 **Private**: Random values used for shuffle

To hide the shuffled deck:
- Use the **commitment-based circuit** (see `optimized_deck_shuffle.circom`)
- Only reveal commitment hash publicly
- Reveal actual cards when needed

## 🧪 Testing

### Unit Tests

```bash
# Test shuffle algorithm
yarn test

# Test proof generation
yarn run test:proof

# Test browser integration
yarn run test:browser
```

### Manual Testing

1. Open `demo.html`
2. Click "Shuffle Deck"
3. Verify first 13 cards are randomized
4. Click "Generate ZK Proof"
5. Wait 2-10 seconds
6. Click "Verify Proof"
7. Should see "✓ Proof VALID"

## 📤 On-Chain Verification

### Deploy Verifier Contract

```solidity
// DeckShuffleVerifier.sol was generated by:
// yarn run export:solidity

// Deploy to your chain
forge create DeckShuffleVerifier \
  --rpc-url $RPC_URL \
  --private-key $PRIVATE_KEY
```

### Verify Proof On-Chain

```typescript
// Get Solidity calldata
const calldata = await generator.exportSolidityCallData(proof, publicSignals);

// Call verifier contract
const tx = await verifierContract.verifyProof(...calldata);
const isValid = await tx.wait();
```

## 🚧 Troubleshooting

### WASM Files Not Loading

**Problem**: 404 errors for `.wasm` or `.zkey` files

**Solution**:
1. Ensure files are in `public/circuits/`
2. Check MIME types in server config
3. Enable CORS headers

```javascript
// Express example
app.use('/circuits', express.static('public/circuits', {
  setHeaders: (res, path) => {
    if (path.endsWith('.wasm')) {
      res.set('Content-Type', 'application/wasm');
    }
  }
}));
```

### Out of Memory

**Problem**: "JavaScript heap out of memory"

**Solution**:
```bash
export NODE_OPTIONS="--max-old-space-size=4096"
yarn run build:all
```

### Slow Proof Generation

**Problem**: Proof takes >30 seconds

**Solutions**:
1. Use Web Worker (see `browserProofGenerator.ts`)
2. Preload circuit files
3. Consider server-side generation for mobile
4. Use optimized circuit

### CORS Errors

**Problem**: Cross-origin errors in browser

**Solution**:
```bash
# Development
yarn run serve:demo -- --cors

# Production: Configure server
Access-Control-Allow-Origin: *
```

## 📚 Additional Resources

- [Circom Documentation](https://docs.circom.io/)
- [SnarkJS Documentation](https://github.com/iden3/snarkjs)
- [ZK-SNARK Explainer](https://z.cash/technology/zksnarks/)
- [Fisher-Yates Algorithm](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

MIT License - see LICENSE file for details

## 🎉 What's Next?

- [ ] Multi-party shuffle protocol
- [ ] Time-lock encryption for delayed reveals
- [ ] Integration with smart contracts
- [ ] Mobile optimization
- [ ] Desktop app with Tauri
- [ ] Batch verification for multiple proofs

---

**Made with ♠️♥️♦️♣️ for provably fair poker**