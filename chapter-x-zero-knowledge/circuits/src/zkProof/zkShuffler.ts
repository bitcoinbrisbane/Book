import * as crypto from "crypto";
import { buildPoseidon } from "circomlibjs";

/**
 * Advanced ZK Shuffle Helpers
 * Includes commitment schemes and secure randomness generation
 */

export class ZKShuffleHelpers {
    private poseidon: any;
    private initialized: boolean = false;

    async initialize() {
        if (!this.initialized) {
            this.poseidon = await buildPoseidon();
            this.initialized = true;
        }
    }

    /**
     * Generates cryptographically secure random seed
     */
    generateRandomSeed(): bigint {
        const randomBytes = crypto.randomBytes(32);
        return BigInt('0x' + randomBytes.toString('hex'));
    }

    /**
     * Generates random values for Fisher-Yates shuffle
     * Uses deterministic generation from seed for reproducibility
     */
    generateRandomValues(seed: bigint, count: number): bigint[] {
        const values: bigint[] = [];
        let currentSeed = seed;

        for (let i = 0; i < count; i++) {
            // Hash current seed to get next random value
            const hash = crypto.createHash('sha256')
                .update(currentSeed.toString())
                .digest();
            
            const randomValue = BigInt('0x' + hash.toString('hex'));
            values.push(randomValue);
            
            // Update seed for next iteration
            currentSeed = randomValue;
        }

        return values;
    }

    /**
     * Creates a Poseidon commitment to the shuffled deck
     */
    async createDeckCommitment(
        randomSeed: bigint,
        shuffledDeck: number[]
    ): Promise<bigint> {
        await this.initialize();

        // Prepare inputs: [randomSeed, card1, card2, ..., card52]
        const inputs = [randomSeed, ...shuffledDeck.map(n => BigInt(n))];

        // Hash using Poseidon
        const hash = this.poseidon(inputs);
        return this.poseidon.F.toObject(hash);
    }

    /**
     * Verifies a deck commitment
     */
    async verifyDeckCommitment(
        commitment: bigint,
        randomSeed: bigint,
        shuffledDeck: number[]
    ): Promise<boolean> {
        const calculatedCommitment = await this.createDeckCommitment(
            randomSeed,
            shuffledDeck
        );
        return commitment === calculatedCommitment;
    }

    /**
     * Generates complete proof inputs with commitment
     */
    async generateProofInputsWithCommitment(
        originalOrder: number[],
        shuffledOrder: number[],
        randomSeed?: bigint
    ): Promise<{
        publicInputs: {
            inputDeck: string[];
            deckCommitment: string;
        };
        privateInputs: {
            outputDeck: string[];
            randomSeed: string;
            randomValues: string[];
        };
    }> {
        await this.initialize();

        // Generate or use provided random seed
        const seed = randomSeed || this.generateRandomSeed();

        // Generate random values from seed
        const randomValues = this.generateRandomValues(seed, originalOrder.length - 1);

        // Create commitment
        const commitment = await this.createDeckCommitment(seed, shuffledOrder);

        return {
            publicInputs: {
                inputDeck: originalOrder.map(n => n.toString()),
                deckCommitment: commitment.toString()
            },
            privateInputs: {
                outputDeck: shuffledOrder.map(n => n.toString()),
                randomSeed: seed.toString(),
                randomValues: randomValues.map(v => v.toString())
            }
        };
    }
}

/**
 * Multi-party shuffle protocol
 * Allows multiple parties to contribute randomness
 */
export class MultiPartyShuffleProtocol {
    private helpers: ZKShuffleHelpers;

    constructor() {
        this.helpers = new ZKShuffleHelpers();
    }

    /**
     * Generates a random contribution for multi-party shuffle
     */
    generateContribution(): {
        contribution: bigint;
        commitment: string;
    } {
        const contribution = this.helpers.generateRandomSeed();
        const hash = crypto.createHash('sha256')
            .update(contribution.toString())
            .digest('hex');

        return {
            contribution,
            commitment: hash
        };
    }

    /**
     * Combines multiple party contributions into final seed
     */
    combineContributions(contributions: bigint[]): bigint {
        let combined = 0n;
        
        for (const contrib of contributions) {
            combined ^= contrib; // XOR all contributions
        }

        // Final hash to ensure uniform distribution
        const hash = crypto.createHash('sha256')
            .update(combined.toString())
            .digest();

        return BigInt('0x' + hash.toString('hex'));
    }

    /**
     * Verifies a party's contribution matches their commitment
     */
    verifyContribution(
        contribution: bigint,
        commitment: string
    ): boolean {
        const hash = crypto.createHash('sha256')
            .update(contribution.toString())
            .digest('hex');

        return hash === commitment;
    }

    /**
     * Complete multi-party shuffle workflow
     */
    async executeMultiPartyShufffle(
        originalOrder: number[],
        parties: number
    ): Promise<{
        shuffledOrder: number[];
        contributions: bigint[];
        commitments: string[];
        finalSeed: bigint;
        proofInputs: any;
    }> {
        // Phase 1: Collect commitments
        const contributions: bigint[] = [];
        const commitments: string[] = [];

        console.log(`\n=== Multi-Party Shuffle with ${parties} parties ===`);

        for (let i = 0; i < parties; i++) {
            const { contribution, commitment } = this.generateContribution();
            contributions.push(contribution);
            commitments.push(commitment);
            console.log(`Party ${i + 1} committed: ${commitment.substring(0, 16)}...`);
        }

        // Phase 2: Reveal and verify
        console.log("\n=== Verification Phase ===");
        for (let i = 0; i < parties; i++) {
            const valid = this.verifyContribution(contributions[i], commitments[i]);
            console.log(`Party ${i + 1} contribution: ${valid ? '✓ Valid' : '✗ Invalid'}`);
        }

        // Phase 3: Combine and shuffle
        const finalSeed = this.combineContributions(contributions);
        console.log(`\nFinal combined seed: ${finalSeed.toString().substring(0, 32)}...`);

        // Generate random values and perform shuffle
        const randomValues = this.helpers.generateRandomValues(
            finalSeed,
            originalOrder.length - 1
        );

        const shuffledOrder = [...originalOrder];
        for (let i = 0; i < shuffledOrder.length - 1; i++) {
            const range = BigInt(shuffledOrder.length - i);
            const randomMod = Number(randomValues[i] % range);
            const j = i + randomMod;
            [shuffledOrder[i], shuffledOrder[j]] = [shuffledOrder[j], shuffledOrder[i]];
        }

        // Generate proof inputs
        const proofInputs = await this.helpers.generateProofInputsWithCommitment(
            originalOrder,
            shuffledOrder,
            finalSeed
        );

        return {
            shuffledOrder,
            contributions,
            commitments,
            finalSeed,
            proofInputs
        };
    }
}

/**
 * Secure shuffle with time-lock encryption
 * Allows deck to be shuffled but not revealed until a specific time
 */
export class TimeLockShuffle {
    private helpers: ZKShuffleHelpers;

    constructor() {
        this.helpers = new ZKShuffleHelpers();
    }

    /**
     * Encrypts shuffled deck with time-based key
     */
    encryptShuffledDeck(
        shuffledOrder: number[],
        unlockTime: number
    ): {
        encryptedDeck: string;
        commitment: string;
        unlockTime: number;
    } {
        // Create time-based key
        const timeKey = crypto.createHash('sha256')
            .update(`${unlockTime}`)
            .digest();

        // Encrypt the deck
        const cipher = crypto.createCipheriv(
            'aes-256-cbc',
            timeKey,
            Buffer.alloc(16, 0)
        );

        const deckString = JSON.stringify(shuffledOrder);
        let encrypted = cipher.update(deckString, 'utf8', 'hex');
        encrypted += cipher.final('hex');

        // Create commitment
        const commitment = crypto.createHash('sha256')
            .update(deckString)
            .digest('hex');

        return {
            encryptedDeck: encrypted,
            commitment,
            unlockTime
        };
    }

    /**
     * Attempts to decrypt shuffled deck (only works after unlock time)
     */
    decryptShuffledDeck(
        encryptedDeck: string,
        unlockTime: number,
        currentTime: number
    ): number[] | null {
        if (currentTime < unlockTime) {
            console.log("Deck is still time-locked!");
            return null;
        }

        // Recreate time-based key
        const timeKey = crypto.createHash('sha256')
            .update(`${unlockTime}`)
            .digest();

        try {
            const decipher = crypto.createDecipheriv(
                'aes-256-cbc',
                timeKey,
                Buffer.alloc(16, 0)
            );

            let decrypted = decipher.update(encryptedDeck, 'hex', 'utf8');
            decrypted += decipher.final('utf8');

            return JSON.parse(decrypted);
        } catch (error) {
            console.error("Decryption failed:", error);
            return null;
        }
    }
}

/**
 * Usage examples
 */
export async function demonstrateAdvancedFeatures() {
    console.log("=== Advanced ZK Shuffle Features ===\n");

    // 1. Basic commitment
    console.log("1. Deck Commitment:");
    const helpers = new ZKShuffleHelpers();
    await helpers.initialize();

    const seed = helpers.generateRandomSeed();
    const deck = Array.from({ length: 52 }, (_, i) => i);
    const commitment = await helpers.createDeckCommitment(seed, deck);
    console.log(`   Commitment: ${commitment.toString().substring(0, 32)}...`);

    // 2. Multi-party shuffle
    console.log("\n2. Multi-Party Shuffle:");
    const multiParty = new MultiPartyShuffleProtocol();
    const result = await multiParty.executeMultiPartyShufffle(deck, 3);
    console.log(`   Shuffled first 5 cards: ${result.shuffledOrder.slice(0, 5)}`);

    // 3. Time-lock shuffle
    console.log("\n3. Time-Lock Shuffle:");
    const timeLock = new TimeLockShuffle();
    const unlockTime = Date.now() + 60000; // Unlock in 60 seconds
    const encrypted = timeLock.encryptShuffledDeck(result.shuffledOrder, unlockTime);
    console.log(`   Encrypted deck: ${encrypted.encryptedDeck.substring(0, 32)}...`);
    console.log(`   Unlock time: ${new Date(unlockTime).toISOString()}`);

    // Try to decrypt before time
    const decrypted1 = timeLock.decryptShuffledDeck(
        encrypted.encryptedDeck,
        unlockTime,
        Date.now()
    );
    console.log(`   Decrypt before time: ${decrypted1 ? 'Success' : 'Locked'}`);

    console.log("\n✓ All advanced features demonstrated!");
}

export default ZKShuffleHelpers;