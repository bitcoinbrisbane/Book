//import { Card } from "@bitcoinbrisbane/block52";
import { Deck } from "../models/deck";
import * as crypto from "crypto";

/**
 * Fisher-Yates shuffle implementation with ZK proof generation
 */
export class ZKDeckShuffler {
    private readonly deckSize: number = 52;
    
    /**
     * Generates cryptographically secure random values for shuffle
     */
    private generateRandomValues(count: number): bigint[] {
        const randomValues: bigint[] = [];
        
        for (let i = 0; i < count; i++) {
            // Generate random bytes
            const randomBytes = crypto.randomBytes(32);
            // Convert to bigint
            const randomValue = BigInt('0x' + randomBytes.toString('hex'));
            randomValues.push(randomValue);
        }
        
        return randomValues;
    }
    
    /**
     * Performs Fisher-Yates shuffle on a deck
     * Returns the shuffled deck and the random values used (for ZK proof)
     */
    shuffleDeck(deck: Deck): {
        shuffledDeck: Deck;
        originalOrder: number[];
        shuffledOrder: number[];
        randomValues: bigint[];
    } {
        // Get the original card order as numeric values
        const originalOrder = this.deckToNumbers(deck);
        
        // Create a copy for shuffling
        const shuffledOrder = [...originalOrder];
        
        // Generate random values for Fisher-Yates
        const randomValues = this.generateRandomValues(this.deckSize - 1);
        
        // Perform Fisher-Yates shuffle
        for (let i = 0; i < shuffledOrder.length - 1; i++) {
            // Calculate swap index: j = i + (randomValues[i] % (n - i))
            const range = BigInt(shuffledOrder.length - i);
            const randomMod = Number(randomValues[i] % range);
            const j = i + randomMod;
            
            // Swap elements at positions i and j
            [shuffledOrder[i], shuffledOrder[j]] = [shuffledOrder[j], shuffledOrder[i]];
        }
        
        // Convert shuffled order back to Deck
        const shuffledDeck = this.numbersTodeck(shuffledOrder);
        
        return {
            shuffledDeck,
            originalOrder,
            shuffledOrder,
            randomValues
        };
    }
    
    /**
     * Converts a Deck to numeric representation
     * Each card is represented as a unique number 0-51
     */
    private deckToNumbers(deck: Deck): number[] {
        const cards: number[] = [];
        
        // Map each card to a unique number
        // Suits: 0=Hearts, 1=Diamonds, 2=Clubs, 3=Spades
        // Ranks: 0=2, 1=3, ..., 8=10, 9=J, 10=Q, 11=K, 12=A
        
        for (let i = 0; i < 52; i++) {
            cards.push(i);
        }
        
        return cards;
    }
    
    /**
     * Converts numeric representation back to Deck
     */
    private numbersTodeck(numbers: number[]): Deck {
        // Create string representation of shuffled deck
        const suits = ['h', 'd', 'c', 's'];
        const ranks = ['2', '3', '4', '5', '6', '7', '8', '9', '10', 'j', 'q', 'k', 'a'];
        
        let deckString = '';
        for (const num of numbers) {
            const suit = suits[Math.floor(num / 13)];
            const rank = ranks[num % 13];
            deckString += rank + suit;
        }
        
        return new Deck(deckString);
    }
    
    /**
     * Generates inputs for the Circom circuit
     */
    generateCircuitInputs(
        originalOrder: number[],
        shuffledOrder: number[],
        randomValues: bigint[]
    ): {
        inputDeck: string[];
        outputDeck: string[];
        randomValues: string[];
    } {
        return {
            inputDeck: originalOrder.map(n => n.toString()),
            outputDeck: shuffledOrder.map(n => n.toString()),
            randomValues: randomValues.map(v => v.toString())
        };
    }
    
    /**
     * Complete workflow: shuffle deck and generate ZK proof inputs
     */
    shuffleAndGenerateProofInputs(deck: Deck): {
        shuffledDeck: Deck;
        circuitInputs: {
            inputDeck: string[];
            outputDeck: string[];
            randomValues: string[];
        };
    } {
        const {
            shuffledDeck,
            originalOrder,
            shuffledOrder,
            randomValues
        } = this.shuffleDeck(deck);
        
        const circuitInputs = this.generateCircuitInputs(
            originalOrder,
            shuffledOrder,
            randomValues
        );
        
        return {
            shuffledDeck,
            circuitInputs
        };
    }
    
    /**
     * Verifies that a shuffle is valid (deterministic check)
     */
    verifyShuffle(
        originalOrder: number[],
        shuffledOrder: number[],
        randomValues: bigint[]
    ): boolean {
        // Create a copy to simulate the shuffle
        const testOrder = [...originalOrder];
        
        // Apply Fisher-Yates with the given random values
        for (let i = 0; i < testOrder.length - 1; i++) {
            const range = BigInt(testOrder.length - i);
            const randomMod = Number(randomValues[i] % range);
            const j = i + randomMod;
            
            [testOrder[i], testOrder[j]] = [testOrder[j], testOrder[i]];
        }
        
        // Check if result matches the claimed shuffle
        return testOrder.every((val, idx) => val === shuffledOrder[idx]);
    }
}