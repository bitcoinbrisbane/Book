# Replay Attacks

To bridge between the Ethereum mainnet contract and our layer 2, we will use signed messages.

## What is a Replay Attack?

In our deposit contract later on we will use a nonce to prevent a replay attack, but first let's discuss what that is. As the name suggests, an attacker can try to send the same request or payload and create subsequent requests.

### Example Attack

1. Send a request to Bob to transfer $100
2. Send the same signed request to Alice for the same $100

Essentially, this is an example of a double spend. An attacker has been able to perform the same action twice.

## Defense: Using a Nonce

We can solve this by using a nonce - a "number used once."

A nonce is a unique identifier included in each transaction that ensures:

1. Each transaction can only be processed once
2. Transactions must be processed in order
3. Replaying an old transaction will be rejected

### Implementation Considerations

When implementing nonce-based replay protection:

- **Incrementing nonces**: Each new transaction must have a nonce greater than the previous
- **Tracking state**: The contract/system must track the last used nonce for each user
- **Timestamp-based**: Alternatively, include timestamps with a validity window

[TODO: Add JavaScript and Solidity code examples for nonce-based replay protection]
