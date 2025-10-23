/**
 * Browser WASM Integration for ZK Deck Shuffling
 * 
 * This shows how to use the compiled Circom circuit WASM in a browser
 * to generate zero-knowledge proofs client-side
 */

// @ts-ignore
import * as snarkjs from "snarkjs";

export class BrowserZKProofGenerator {
    private wasmPath: string;
    private zkeyPath: string;
    private vkeyPath: string;
    private loaded: boolean = false;

    constructor(
        wasmPath: string = "/circuits/deck_shuffle.wasm",
        zkeyPath: string = "/circuits/deck_shuffle.zkey",
        vkeyPath: string = "/circuits/verification_key.json"
    ) {
        this.wasmPath = wasmPath;
        this.zkeyPath = zkeyPath;
        this.vkeyPath = vkeyPath;
    }

    /**
     * Preload circuit files for faster proof generation
     */
    async preload(): Promise<void> {
        console.log("Preloading ZK circuit files...");
        
        try {
            // Fetch and cache the files
            await Promise.all([
                fetch(this.wasmPath),
                fetch(this.zkeyPath),
                fetch(this.vkeyPath)
            ]);
            
            this.loaded = true;
            console.log("✓ Circuit files preloaded");
        } catch (error) {
            console.error("Failed to preload circuit files:", error);
            throw error;
        }
    }

    /**
     * Generates a ZK proof for a deck shuffle
     * This runs entirely in the browser using WASM
     */
    async generateShuffleProof(
        inputDeck: number[],
        outputDeck: number[],
        randomValues: bigint[]
    ): Promise<{
        proof: any;
        publicSignals: string[];
        proofTime: number;
    }> {
        console.log("Generating ZK proof in browser...");
        const startTime = performance.now();

        try {
            // Prepare circuit inputs
            const input = {
                inputDeck: inputDeck.map(n => n.toString()),
                outputDeck: outputDeck.map(n => n.toString()),
                randomValues: randomValues.map(v => v.toString())
            };

            console.log("Circuit inputs prepared");

            // Generate witness using WASM
            console.log("Computing witness...");
            const { proof, publicSignals } = await snarkjs.groth16.fullProve(
                input,
                this.wasmPath,
                this.zkeyPath
            );

            const proofTime = performance.now() - startTime;
            console.log(`✓ Proof generated in ${proofTime.toFixed(2)}ms`);

            return {
                proof,
                publicSignals,
                proofTime
            };
        } catch (error) {
            console.error("Proof generation failed:", error);
            throw error;
        }
    }

    /**
     * Verifies a ZK proof in the browser
     */
    async verifyProof(
        proof: any,
        publicSignals: string[]
    ): Promise<boolean> {
        console.log("Verifying proof...");

        try {
            // Fetch verification key
            const vkeyResponse = await fetch(this.vkeyPath);
            const vkey = await vkeyResponse.json();

            // Verify the proof
            const isValid = await snarkjs.groth16.verify(
                vkey,
                publicSignals,
                proof
            );

            console.log(`Proof verification: ${isValid ? "✓ VALID" : "✗ INVALID"}`);
            return isValid;
        } catch (error) {
            console.error("Proof verification failed:", error);
            return false;
        }
    }

    /**
     * Exports proof for on-chain verification
     */
    async exportSolidityCallData(
        proof: any,
        publicSignals: string[]
    ): Promise<string> {
        const calldata = await snarkjs.groth16.exportSolidityCallData(
            proof,
            publicSignals
        );
        return calldata;
    }
}

/**
 * React Hook for ZK Proof Generation
 */
export function useZKProofGenerator() {
    const [generator, setGenerator] = useState<BrowserZKProofGenerator | null>(null);
    const [isLoading, setIsLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const init = async () => {
            try {
                setIsLoading(true);
                const gen = new BrowserZKProofGenerator();
                await gen.preload();
                setGenerator(gen);
            } catch (err) {
                setError(err instanceof Error ? err.message : "Failed to initialize");
            } finally {
                setIsLoading(false);
            }
        };

        init();
    }, []);

    const generateProof = async (
        inputDeck: number[],
        outputDeck: number[],
        randomValues: bigint[]
    ) => {
        if (!generator) {
            throw new Error("Generator not initialized");
        }

        setIsLoading(true);
        setError(null);

        try {
            const result = await generator.generateShuffleProof(
                inputDeck,
                outputDeck,
                randomValues
            );
            return result;
        } catch (err) {
            const errorMsg = err instanceof Error ? err.message : "Proof generation failed";
            setError(errorMsg);
            throw err;
        } finally {
            setIsLoading(false);
        }
    };

    const verifyProof = async (proof: any, publicSignals: string[]) => {
        if (!generator) {
            throw new Error("Generator not initialized");
        }

        return await generator.verifyProof(proof, publicSignals);
    };

    return {
        generateProof,
        verifyProof,
        isLoading,
        error
    };
}

/**
 * Example: Integration with Poker Game UI
 */
export class PokerGameWithZKProofs {
    private proofGenerator: BrowserZKProofGenerator;
    private currentProof: any = null;

    constructor() {
        this.proofGenerator = new BrowserZKProofGenerator();
    }

    async initialize() {
        await this.proofGenerator.preload();
    }

    /**
     * Shuffles deck and generates proof
     */
    async shuffleWithProof(
        inputDeck: number[]
    ): Promise<{
        shuffledDeck: number[];
        proof: any;
        publicSignals: string[];
    }> {
        // Generate random values
        const randomValues: bigint[] = [];
        for (let i = 0; i < 51; i++) {
            const randomBytes = new Uint8Array(32);
            crypto.getRandomValues(randomBytes);
            const randomValue = BigInt('0x' + Array.from(randomBytes)
                .map(b => b.toString(16).padStart(2, '0'))
                .join(''));
            randomValues.push(randomValue);
        }

        // Perform Fisher-Yates shuffle
        const shuffledDeck = [...inputDeck];
        for (let i = 0; i < shuffledDeck.length - 1; i++) {
            const range = BigInt(shuffledDeck.length - i);
            const randomMod = Number(randomValues[i] % range);
            const j = i + randomMod;
            [shuffledDeck[i], shuffledDeck[j]] = [shuffledDeck[j], shuffledDeck[i]];
        }

        // Generate ZK proof
        const { proof, publicSignals } = await this.proofGenerator.generateShuffleProof(
            inputDeck,
            shuffledDeck,
            randomValues
        );

        this.currentProof = { proof, publicSignals };

        return {
            shuffledDeck,
            proof,
            publicSignals
        };
    }

    /**
     * Verifies the current shuffle proof
     */
    async verifyCurrentShuffle(): Promise<boolean> {
        if (!this.currentProof) {
            throw new Error("No proof to verify");
        }

        return await this.proofGenerator.verifyProof(
            this.currentProof.proof,
            this.currentProof.publicSignals
        );
    }

    /**
     * Prepares proof for on-chain verification
     */
    async getOnChainCalldata(): Promise<string> {
        if (!this.currentProof) {
            throw new Error("No proof available");
        }

        return await this.proofGenerator.exportSolidityCallData(
            this.currentProof.proof,
            this.currentProof.publicSignals
        );
    }
}

// React Component Example
import React, { useState, useEffect } from 'react';

export const ZKPokerGame: React.FC = () => {
    const { generateProof, verifyProof, isLoading, error } = useZKProofGenerator();
    const [shuffledDeck, setShuffledDeck] = useState<number[]>([]);
    const [proofData, setProofData] = useState<any>(null);
    const [verificationResult, setVerificationResult] = useState<boolean | null>(null);

    const handleShuffle = async () => {
        const inputDeck = Array.from({ length: 52 }, (_, i) => i);
        
        // Generate random values
        const randomValues: bigint[] = [];
        for (let i = 0; i < 51; i++) {
            const randomBytes = new Uint8Array(32);
            crypto.getRandomValues(randomBytes);
            const randomValue = BigInt('0x' + Array.from(randomBytes)
                .map(b => b.toString(16).padStart(2, '0'))
                .join(''));
            randomValues.push(randomValue);
        }

        // Perform shuffle
        const outputDeck = [...inputDeck];
        for (let i = 0; i < outputDeck.length - 1; i++) {
            const range = BigInt(outputDeck.length - i);
            const randomMod = Number(randomValues[i] % range);
            const j = i + randomMod;
            [outputDeck[i], outputDeck[j]] = [outputDeck[j], outputDeck[i]];
        }

        // Generate proof
        const result = await generateProof(inputDeck, outputDeck, randomValues);
        
        setShuffledDeck(outputDeck);
        setProofData(result);
    };

    const handleVerify = async () => {
        if (!proofData) return;
        
        const isValid = await verifyProof(
            proofData.proof,
            proofData.publicSignals
        );
        
        setVerificationResult(isValid);
    };

    return (
        <div className="zk-poker-game">
            <h2>ZK Poker Game</h2>
            
            <div className="controls">
                <button 
                    onClick={handleShuffle} 
                    disabled={isLoading}
                >
                    {isLoading ? "Shuffling..." : "Shuffle Deck"}
                </button>
                
                {proofData && (
                    <button onClick={handleVerify}>
                        Verify Proof
                    </button>
                )}
            </div>

            {error && (
                <div className="error">
                    Error: {error}
                </div>
            )}

            {shuffledDeck.length > 0 && (
                <div className="deck-info">
                    <h3>Shuffled Deck</h3>
                    <p>First 10 cards: {shuffledDeck.slice(0, 10).join(', ')}</p>
                </div>
            )}

            {proofData && (
                <div className="proof-info">
                    <h3>Proof Generated</h3>
                    <p>Time: {proofData.proofTime.toFixed(2)}ms</p>
                    <p>Public Signals: {proofData.publicSignals.length}</p>
                </div>
            )}

            {verificationResult !== null && (
                <div className={`verification ${verificationResult ? 'valid' : 'invalid'}`}>
                    {verificationResult ? '✓ Proof Valid' : '✗ Proof Invalid'}
                </div>
            )}
        </div>
    );
};