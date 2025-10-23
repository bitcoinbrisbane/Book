/**
 * Integration between off-chain poker game and on-chain slashing contract
 */

import { ethers } from "ethers";
import * as snarkjs from "snarkjs";
import TexasHoldemGame from "./texasHoldemGame";
import { ZKDeckShuffler } from "./zkDeckShuffler";

// ==================== TYPES ====================

interface ShuffleData {
    inputDeck: number[];
    outputDeck: number[];
    randomValues: bigint[];
    deckHash: string;
    signature: string;
}

interface DepositInfo {
    amount: bigint;
    lockedUntil: bigint;
    exists: boolean;
}

// ==================== SLASHING CONTRACT INTEGRATION ====================

export class SlashingContractIntegration {
    private slashingContract: ethers.Contract;
    private signer: ethers.Signer;
    private shuffler: ZKDeckShuffler;

    constructor(
        contractAddress: string,
        signer: ethers.Signer,
        contractABI: any[]
    ) {
        this.slashingContract = new ethers.Contract(
            contractAddress,
            contractABI,
            signer
        );
        this.signer = signer;
        this.shuffler = new ZKDeckShuffler();
    }

    // ==================== DEPOSIT MANAGEMENT ====================

    /**
     * Deposit USDT to become eligible for shuffling
     */
    async deposit(usdtContract: ethers.Contract): Promise<void> {
        console.log("💰 Depositing USDT...");

        const depositAmount = await this.slashingContract.DEPOSIT_AMOUNT();
        const contractAddress = await this.slashingContract.getAddress();

        // 1. Approve USDT spending
        console.log("   Approving USDT...");
        const approveTx = await usdtContract.approve(contractAddress, depositAmount);
        await approveTx.wait();
        console.log("   ✓ USDT approved");

        // 2. Make deposit
        console.log("   Making deposit...");
        const depositTx = await this.slashingContract.deposit();
        await depositTx.wait();
        console.log("   ✓ Deposit complete:", ethers.formatUnits(depositAmount, 6), "USDT");
    }

    /**
     * Withdraw deposit (only if unlocked)
     */
    async withdraw(): Promise<void> {
        console.log("💸 Withdrawing deposit...");

        const signerAddress = await this.signer.getAddress();
        const deposit = await this.getDepositInfo(signerAddress);

        if (!deposit.exists) {
            throw new Error("No deposit found");
        }

        if (deposit.lockedUntil > BigInt(Date.now() / 1000)) {
            throw new Error(`Deposit locked until ${new Date(Number(deposit.lockedUntil) * 1000)}`);
        }

        const tx = await this.slashingContract.withdraw();
        await tx.wait();
        
        console.log("✓ Withdrawal complete:", ethers.formatUnits(deposit.amount, 6), "USDT");
    }

    /**
     * Check deposit status
     */
    async getDepositInfo(address: string): Promise<DepositInfo> {
        const deposit = await this.slashingContract.getDeposit(address);
        return {
            amount: deposit.amount,
            lockedUntil: deposit.lockedUntil,
            exists: deposit.exists
        };
    }

    // ==================== SHUFFLE & COMMITMENT ====================

    /**
     * Generate shuffled deck and create commitment
     */
    async generateAndCommitShuffle(
        gameId: string
    ): Promise<ShuffleData> {
        console.log("🎴 Generating shuffle for game:", gameId);

        // 1. Generate shuffled deck
        const orderedDeck = Array.from({ length: 52 }, (_, i) => i);
        const {
            shuffledOrder,
            originalOrder,
            randomValues
        } = this.shuffler.shuffleDeck({ toString: () => "" } as any);

        // 2. Create deck hash
        const deckHash = ethers.keccak256(
            ethers.solidityPacked(
                ["uint256[]"],
                [shuffledOrder]
            )
        );

        // 3. Sign the deck hash
        const signature = await this.signDeckHash(deckHash);

        console.log("✓ Shuffle generated");
        console.log("  Deck hash:", deckHash);
        console.log("  Signature:", signature.substring(0, 20) + "...");

        // 4. Commit to contract
        await this.commitToContract(gameId, deckHash, signature);

        return {
            inputDeck: originalOrder,
            outputDeck: shuffledOrder,
            randomValues,
            deckHash,
            signature
        };
    }

    /**
     * Sign a deck hash with Ethereum private key
     */
    private async signDeckHash(deckHash: string): Promise<string> {
        const messageHash = ethers.getBytes(deckHash);
        const signature = await this.signer.signMessage(messageHash);
        return signature;
    }

    /**
     * Commit shuffle to smart contract
     */
    private async commitToContract(
        gameId: string,
        deckHash: string,
        signature: string
    ): Promise<void> {
        console.log("📝 Committing shuffle to contract...");

        const gameIdBytes32 = ethers.id(gameId); // Convert to bytes32

        const tx = await this.slashingContract.commitShuffle(
            gameIdBytes32,
            deckHash,
            signature
        );

        await tx.wait();
        console.log("✓ Shuffle committed on-chain");
    }

    // ==================== VERIFICATION ====================

    /**
     * Generate and submit ZK proof for shuffle verification
     */
    async verifyShuffleOnChain(
        gameId: string,
        shuffleData: ShuffleData
    ): Promise<void> {
        console.log("🔐 Generating ZK proof...");

        // 1. Generate ZK proof
        const { proof, publicSignals } = await this.generateProof(
            shuffleData.inputDeck,
            shuffleData.outputDeck,
            shuffleData.randomValues
        );

        console.log("✓ Proof generated");

        // 2. Format for Solidity
        const solidityProof = this.formatProofForSolidity(proof, publicSignals);

        // 3. Submit to contract
        console.log("📤 Submitting proof to contract...");
        const gameIdBytes32 = ethers.id(gameId);

        const tx = await this.slashingContract.verifyShuffle(
            gameIdBytes32,
            solidityProof.a,
            solidityProof.b,
            solidityProof.c,
            solidityProof.input
        );

        await tx.wait();
        console.log("✓ Shuffle verified on-chain!");
    }

    /**
     * Generate ZK proof using snarkjs
     */
    private async generateProof(
        inputDeck: number[],
        outputDeck: number[],
        randomValues: bigint[]
    ): Promise<{ proof: any; publicSignals: string[] }> {
        const input = {
            inputDeck: inputDeck.map(n => n.toString()),
            outputDeck: outputDeck.map(n => n.toString()),
            randomValues: randomValues.map(v => v.toString())
        };

        const { proof, publicSignals } = await snarkjs.groth16.fullProve(
            input,
            "./circuits/deck_shuffle_js/deck_shuffle.wasm",
            "./circuits/deck_shuffle.zkey"
        );

        return { proof, publicSignals };
    }

    /**
     * Format proof for Solidity
     */
    private formatProofForSolidity(proof: any, publicSignals: string[]): {
        a: [string, string];
        b: [[string, string], [string, string]];
        c: [string, string];
        input: string[];
    } {
        return {
            a: [proof.pi_a[0], proof.pi_a[1]],
            b: [
                [proof.pi_b[0][1], proof.pi_b[0][0]],
                [proof.pi_b[1][1], proof.pi_b[1][0]]
            ],
            c: [proof.pi_c[0], proof.pi_c[1]],
            input: publicSignals.slice(0, 104)
        };
    }

    // ==================== CHALLENGE ====================

    /**
     * Challenge an invalid shuffle
     */
    async challengeShuffle(
        gameId: string,
        inputDeck: number[],
        outputDeck: number[],
        randomValues: bigint[]
    ): Promise<void> {
        console.log("⚠️  Challenging shuffle for game:", gameId);

        // Generate proof that should fail
        const { proof, publicSignals } = await this.generateProof(
            inputDeck,
            outputDeck,
            randomValues
        );

        const solidityProof = this.formatProofForSolidity(proof, publicSignals);
        const gameIdBytes32 = ethers.id(gameId);

        console.log("📤 Submitting challenge...");

        const tx = await this.slashingContract.challengeShuffle(
            gameIdBytes32,
            solidityProof.a,
            solidityProof.b,
            solidityProof.c,
            solidityProof.input
        );

        await tx.wait();
        console.log("✓ Challenge submitted! Shuffler will be slashed if proof is invalid.");
    }

    /**
     * Challenge using algorithm verification (alternative method)
     */
    async challengeShuffleAlgorithm(
        gameId: string,
        inputDeck: number[],
        outputDeck: number[],
        randomValues: bigint[]
    ): Promise<void> {
        console.log("⚠️  Challenging shuffle algorithm for game:", gameId);

        const gameIdBytes32 = ethers.id(gameId);

        // Convert to fixed-size arrays for Solidity
        const inputDeckArray = inputDeck as any;
        const outputDeckArray = outputDeck as any;
        const randomValuesArray = randomValues.slice(0, 51) as any;

        const tx = await this.slashingContract.challengeShuffleAlgorithm(
            gameIdBytes32,
            inputDeckArray,
            outputDeckArray,
            randomValuesArray
        );

        await tx.wait();
        console.log("✓ Algorithm challenge submitted!");
    }

    // ==================== QUERY FUNCTIONS ====================

    /**
     * Check if shuffle can be challenged
     */
    async canChallenge(gameId: string): Promise<boolean> {
        const gameIdBytes32 = ethers.id(gameId);
        return await this.slashingContract.canChallenge(gameIdBytes32);
    }

    /**
     * Get shuffle commitment details
     */
    async getShuffleCommitment(gameId: string): Promise<any> {
        const gameIdBytes32 = ethers.id(gameId);
        return await this.slashingContract.getShuffleCommitment(gameIdBytes32);
    }
}

// ==================== TEXAS HOLDEM GAME INTEGRATION ====================

export class PokerGameWithSlashing {
    private game: TexasHoldemGame;
    private slashing: SlashingContractIntegration;
    private currentShuffleData?: ShuffleData;

    constructor(
        game: TexasHoldemGame,
        slashing: SlashingContractIntegration
    ) {
        this.game = game;
        this.slashing = slashing;
    }

    /**
     * Initialize a new hand with verified shuffle
     */
    async startNewHand(gameId: string): Promise<void> {
        console.log("\n🎮 Starting new hand...");
        console.log("=".repeat(60));

        // 1. Generate and commit shuffle
        this.currentShuffleData = await this.slashing.generateAndCommitShuffle(gameId);

        // 2. Initialize game with shuffled deck
        const deckString = this.deckArrayToString(this.currentShuffleData.outputDeck);
        this.game.reInit(deckString);

        console.log("✓ Game initialized with shuffled deck");

        // 3. Verify shuffle on-chain (optional but recommended)
        await this.slashing.verifyShuffleOnChain(gameId, this.currentShuffleData);

        console.log("=".repeat(60));
        console.log("✅ Hand ready to play!\n");
    }

    /**
     * Let other players verify the shuffle
     */
    async verifyCurrentShuffle(gameId: string): Promise<boolean> {
        if (!this.currentShuffleData) {
            throw new Error("No shuffle data available");
        }

        try {
            await this.slashing.verifyShuffleOnChain(gameId, this.currentShuffleData);
            return true;
        } catch (error) {
            console.error("Verification failed:", error);
            return false;
        }
    }

    /**
     * Convert deck array to string for game
     */
    private deckArrayToString(deck: number[]): string {
        const suits = ['h', 'd', 'c', 's'];
        const ranks = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'j', 'q', 'k', 'a'];

        let deckString = '';
        for (const cardNum of deck) {
            const suit = suits[Math.floor(cardNum / 13)];
            const rank = ranks[cardNum % 13];
            deckString += rank + suit;
        }

        return deckString;
    }
}

// ==================== USAGE EXAMPLES ====================

async function exampleWorkflow() {
    console.log("📚 Example: Complete Poker Game Workflow");
    console.log("=".repeat(60));

    // Setup
    const provider = new ethers.JsonRpcProvider("http://localhost:8545");
    const signer = await provider.getSigner();

    // Contract addresses
    const slashingAddress = "0x...";
    const usdtAddress = "0x...";

    // Initialize contracts
    const slashingABI = [...]; // Your contract ABI
    const slashing = new SlashingContractIntegration(
        slashingAddress,
        signer,
        slashingABI
    );

    const usdtContract = new ethers.Contract(
        usdtAddress,
        ["function approve(address,uint256) returns (bool)"],
        signer
    );

    // 1. Deposit USDT
    console.log("\n1. Making deposit...");
    await slashing.deposit(usdtContract);

    // 2. Create game
    console.log("\n2. Creating game...");
    const gameId = "game-" + Date.now();
    const game = new TexasHoldemGame(
        await signer.getAddress(),
        {
            minBuyIn: 1000n,
            maxBuyIn: 10000n,
            maxPlayers: 9,
            minPlayers: 2,
            smallBlind: 10n,
            bigBlind: 20n,
            timeout: 30
        },
        { dealer: 0, smallBlind: 1, bigBlind: 2 },
        0,
        []
    );

    // 3. Integrate game with slashing
    const pokerGame = new PokerGameWithSlashing(game, slashing);

    // 4. Start hand with verified shuffle
    await pokerGame.startNewHand(gameId);

    // 5. Play the game (off-chain)
    console.log("\n5. Playing hand...");
    game.deal();
    // ... game actions ...

    // 6. After game, withdraw if desired
    console.log("\n6. Withdrawing deposit...");
    await slashing.withdraw();

    console.log("\n✅ Complete workflow finished!");
}

// ==================== CHALLENGE EXAMPLE ====================

async function exampleChallenge() {
    console.log("📚 Example: Challenge Invalid Shuffle");
    console.log("=".repeat(60));

    const provider = new ethers.JsonRpcProvider("http://localhost:8545");
    const signer = await provider.getSigner();

    const slashing = new SlashingContractIntegration(
        "0x...",
        signer,
        [...]
    );

    const gameId = "suspicious-game-123";

    // Check if we can challenge
    const canChallenge = await slashing.canChallenge(gameId);
    if (!canChallenge) {
        console.log("Cannot challenge this shuffle");
        return;
    }

    // Get the committed shuffle data
    const commitment = await slashing.getShuffleCommitment(gameId);
    console.log("Shuffler:", commitment.shuffler);
    console.log("Deck hash:", commitment.deckHash);

    // If you have evidence the shuffle is invalid, challenge it!
    const fakeInputDeck = Array.from({ length: 52 }, (_, i) => i);
    const fakeOutputDeck = [...fakeInputDeck]; // Not shuffled!
    const fakeRandomValues = Array(51).fill(0n);

    await slashing.challengeShuffle(
        gameId,
        fakeInputDeck,
        fakeOutputDeck,
        fakeRandomValues
    );

    console.log("✅ Challenge submitted! If invalid, shuffler will be slashed.");
}

export {
    exampleWorkflow,
    exampleChallenge
};