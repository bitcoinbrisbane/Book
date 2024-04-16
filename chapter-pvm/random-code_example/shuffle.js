const crypto = require("crypto");

const shuffleCards = (cards) => {
    let currentIndex = cards.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (currentIndex !== 0) {
        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = cards[currentIndex];
        cards[currentIndex] = cards[randomIndex];
        cards[randomIndex] = temporaryValue;
    }

    return cards;
}

const concatenateCards = (cards) => {
    // Using the join method with an empty string as the separator to concatenate all elements without any space.
    return cards.join("");
}

// Example usage with a 52-card deck:
const deck = [
    'AC', '2C', '3C', '4C', '5C', '6C', '7C', '8C', '9C', '10C', 'JC', 'QC', 'KC',
    'AD', '2D', '3D', '4D', '5D', '6D', '7D', '8D', '9D', '10D', 'JD', 'QD', 'KD',
    'AH', '2H', '3H', '4H', '5H', '6H', '7H', '8H', '9H', '10H', 'JH', 'QH', 'KH',
    'AS', '2S', '3S', '4S', '5S', '6S', '7S', '8S', '9S', '10S', 'JS', 'QS', 'KS'
];

// Shuffle the deck
shuffleCards(deck);
console.log(deck);

// Concatenate the deck
const concatenatedDeck = concatenateCards(deck);
console.log(concatenatedDeck);

// SHA-256 hash the concatenated deck
const hash = crypto.createHash("sha256");
hash.update(concatenatedDeck);
const hashedDeck = hash.digest("hex");
console.log(hashedDeck);
