# Poker Cryptography

In this section, we will discuss how cryptography can be used in our Poker application.

## The Coinflip Problem

Let's say you'd like to play a game of coin flip or scissors paper rock over the phone, or probably via text message these days. The game is pretty simple: each player chooses an outcome prior to performing the action, and the winner is determined once the coin lands or the hands are revealed.

This works in a live environment because both parties witness the event in real time, and should agree or form consensus on the winner. If one of the parties cannot witness the event in real time, it would be fairly easy to cheat. So how can we play this game fairly using cryptography?

## Commitment Schemes

We can do this by using a one-way function, such as the SHA256 function we discussed in previous sections. Alice picks either heads or tails, called x. We add an arbitrary random number to the value.

```
f(x + r) = y
```

Alice can then send the result as a message to Bob. Bob has no way to know the value Alice chose but has a copy of the hash.

After Bob makes his choice, Alice reveals both her original choice (x) and the random number (r). Bob can then verify by computing the hash himself and comparing it to what Alice originally sent.

## Mental Poker

Mental poker refers to a cryptographic problem that allows players to play a fair game of poker without requiring a trusted third party to deal the cards. The challenge is:

1. **No trusted dealer**: Players must be able to shuffle and deal cards without any single party knowing the full deck state
2. **Hidden information**: Each player should only see their own cards
3. **Verifiability**: At the end of the game, all actions should be verifiable as fair

This is a foundational concept that we'll build upon throughout this book as we implement our poker application.

## Cryptographic Card Shuffling

The key insight is that we can use encryption to "lock" cards, and commutative encryption schemes allow multiple players to each add their own "lock" without revealing the underlying card values.

[TODO: Add detailed implementation of mental poker protocols]
