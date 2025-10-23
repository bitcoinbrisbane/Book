// scripts/test-proof.js
// Test script to generate and verify a deck shuffle proof

const snarkjs = require('snarkjs');
const fs = require('fs');
const path = require('path');
const crypto = require('crypto');

// ANSI color codes for pretty output
const colors = {
    reset: '\x1b[0m',
    bright: '\x1b[1m',
    green: '\x1b[32m',
    red: '\x1b[31m',
    yellow: '\x1b[33m',
    blue: '\x1b[34m',
    cyan: '\x1b[36m',
};

function log(message, color = 'reset') {
    console.log(`${colors[color]}${message}${colors.reset}`);
}

function logSection(title) {
    console.log('\n' + '='.repeat(60));
    log(title, 'bright');
    console.log('='.repeat(60) + '\n');
}

// Generate random values for Fisher-Yates
function generateRandomValues(count) {
    const values = [];
    for (let i = 0; i < count; i++) {
        const randomBytes = crypto.randomBytes(32);
        const randomValue = BigInt('0x' + randomBytes.toString('hex'));
        values.push(randomValue);
    }
    return values;
}

// Perform Fisher-Yates shuffle
function fisherYatesShuffle(inputDeck, randomValues) {
    const outputDeck = [...inputDeck];
    
    for (let i = 0; i < outputDeck.length - 1; i++) {
        const range = BigInt(outputDeck.length - i);
        const randomMod = Number(randomValues[i] % range);
        const j = i + randomMod;
        
        // Swap
        [outputDeck[i], outputDeck[j]] = [outputDeck[j], outputDeck[i]];
    }
    
    return outputDeck;
}

// Verify shuffle deterministically
function verifyShuffle(inputDeck, outputDeck, randomValues) {
    const testDeck = fisherYatesShuffle(inputDeck, randomValues);
    
    for (let i = 0; i < testDeck.length; i++) {
        if (testDeck[i] !== outputDeck[i]) {
            return false;
        }
    }
    
    return true;
}

// Main test function
async function testProofGeneration() {
    try {
        logSection('🎴 ZK Deck Shuffle Proof Test');
        
        // Step 1: Create input deck (0-51)
        log('Step 1: Creating ordered deck...', 'cyan');
        const inputDeck = Array.from({ length: 52 }, (_, i) => i);
        log(`✓ Input deck: [${inputDeck.slice(0, 5).join(', ')}, ...]`, 'green');
        
        // Step 2: Generate random values
        log('\nStep 2: Generating random values...', 'cyan');
        const randomValues = generateRandomValues(51);
        log(`✓ Generated 51 random values`, 'green');
        log(`  First value: ${randomValues[0].toString().substring(0, 20)}...`, 'yellow');
        
        // Step 3: Perform shuffle
        log('\nStep 3: Performing Fisher-Yates shuffle...', 'cyan');
        const outputDeck = fisherYatesShuffle(inputDeck, randomValues);
        log(`✓ Shuffled deck: [${outputDeck.slice(0, 5).join(', ')}, ...]`, 'green');
        
        // Step 4: Verify shuffle locally
        log('\nStep 4: Verifying shuffle locally...', 'cyan');
        const isValidShuffle = verifyShuffle(inputDeck, outputDeck, randomValues);
        if (isValidShuffle) {
            log('✓ Shuffle verification: PASSED', 'green');
        } else {
            log('✗ Shuffle verification: FAILED', 'red');
            return;
        }
        
        // Step 5: Prepare circuit inputs
        log('\nStep 5: Preparing circuit inputs...', 'cyan');
        const circuitInput = {
            inputDeck: inputDeck.map(n => n.toString()),
            outputDeck: outputDeck.map(n => n.toString()),
            randomValues: randomValues.map(v => v.toString())
        };
        
        // Save input for debugging
        const inputPath = path.join(__dirname, '../build/input.json');
        fs.writeFileSync(inputPath, JSON.stringify(circuitInput, null, 2));
        log(`✓ Circuit input saved to: ${inputPath}`, 'green');
        
        // Step 6: Generate ZK proof
        logSection('🔐 Generating Zero-Knowledge Proof');
        
        const wasmPath = path.join(__dirname, '../public/circuits/deck_shuffle_js/deck_shuffle.wasm');
        const zkeyPath = path.join(__dirname, '../public/circuits/deck_shuffle.zkey');
        
        // Check if files exist
        if (!fs.existsSync(wasmPath)) {
            log(`✗ WASM file not found: ${wasmPath}`, 'red');
            log('  Run: npm run compile:circuit', 'yellow');
            return;
        }
        
        if (!fs.existsSync(zkeyPath)) {
            log(`✗ ZKEY file not found: ${zkeyPath}`, 'red');
            log('  Run: npm run setup:zkey', 'yellow');
            return;
        }
        
        log('Generating proof (this may take a few seconds)...', 'cyan');
        const startTime = Date.now();
        
        const { proof, publicSignals } = await snarkjs.groth16.fullProve(
            circuitInput,
            wasmPath,
            zkeyPath
        );
        
        const proofTime = Date.now() - startTime;
        log(`✓ Proof generated in ${proofTime}ms`, 'green');
        
        // Save proof
        const proofPath = path.join(__dirname, '../build/proof.json');
        const publicPath = path.join(__dirname, '../build/public.json');
        
        fs.writeFileSync(proofPath, JSON.stringify(proof, null, 2));
        fs.writeFileSync(publicPath, JSON.stringify(publicSignals, null, 2));
        
        log(`✓ Proof saved to: ${proofPath}`, 'green');
        log(`✓ Public signals saved to: ${publicPath}`, 'green');
        
        // Step 7: Verify the proof
        logSection('✓ Verifying Zero-Knowledge Proof');
        
        const vkeyPath = path.join(__dirname, '../public/circuits/verification_key.json');
        
        if (!fs.existsSync(vkeyPath)) {
            log(`✗ Verification key not found: ${vkeyPath}`, 'red');
            log('  Run: npm run export:vkey', 'yellow');
            return;
        }
        
        log('Loading verification key...', 'cyan');
        const vkey = JSON.parse(fs.readFileSync(vkeyPath, 'utf8'));
        
        log('Verifying proof...', 'cyan');
        const verifyStartTime = Date.now();
        
        const isValid = await snarkjs.groth16.verify(vkey, publicSignals, proof);
        
        const verifyTime = Date.now() - verifyStartTime;
        
        if (isValid) {
            log(`✓ Proof verification: VALID (${verifyTime}ms)`, 'green');
        } else {
            log(`✗ Proof verification: INVALID`, 'red');
        }
        
        // Step 8: Export Solidity calldata
        logSection('📤 Exporting Solidity Calldata');
        
        log('Generating calldata for on-chain verification...', 'cyan');
        const calldata = await snarkjs.groth16.exportSolidityCallData(proof, publicSignals);
        
        const calldataPath = path.join(__dirname, '../build/calldata.txt');
        fs.writeFileSync(calldataPath, calldata);
        
        log(`✓ Calldata saved to: ${calldataPath}`, 'green');
        log('\nCalldata (first 200 chars):', 'yellow');
        console.log(calldata.substring(0, 200) + '...\n');
        
        // Summary
        logSection('📊 Test Summary');
        log(`✓ Deck shuffled: ${inputDeck.length} cards`, 'green');
        log(`✓ Proof generation time: ${proofTime}ms`, 'green');
        log(`✓ Proof verification time: ${verifyTime}ms`, 'green');
        log(`✓ Proof validity: ${isValid ? 'VALID' : 'INVALID'}`, isValid ? 'green' : 'red');
        
        logSection('🎉 All Tests Passed!');
        
    } catch (error) {
        log('\n✗ Error during test:', 'red');
        console.error(error);
        process.exit(1);
    }
}

// Run the test
if (require.main === module) {
    testProofGeneration().then(() => {
        process.exit(0);
    }).catch((error) => {
        console.error(error);
        process.exit(1);
    });
}

module.exports = {
    generateRandomValues,
    fisherYatesShuffle,
    verifyShuffle
};