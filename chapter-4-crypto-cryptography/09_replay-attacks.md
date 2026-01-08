# Replay Attacks

To bridge between the Ethereum mainnet contract and our layer 2, we will use signed messages.

## What is a Replay Attack?

In our deposit contract later on we will use a nonce to prevent a replay attack, but first let's discuss what that is. As the name suggests, an attacker can try to send the same request or payload and create subsequent requests.

### Example Attack

1. Send a request to Bob to transfer $100
2. Send the same signed request to Alice for the same $100

Essentially, this is an example of a double spend. An attacker has been able to perform the same action twice.  So long as the the reciepents are not know to each other, and even if they are, they the attack is performed.

## Defense: Using a Nonce

We can solve this by using a nonce - a "number used once." as part of the message sent to the server.  A nonce is a unique identifier included in each transaction that ensures:

1. Each transaction can only be processed once
2. Transactions must be processed in order
3. Replaying an old transaction will be rejected

When implementing nonce-based replay protection, consider each new transaction must have a nonce greater than the previous.  The contract/system must track the last used nonce for each user or alternatively, include timestamps with a validity window.  If ordering is not a consideration, then a GUID might work too.

### Code Examples

#### JavaScript Example

```javascript
// Simple nonce-based replay protection in JavaScript
class NonceTracker {

  private readonly nonces;

  constructor() {
    this.nonces = new Map(); // Track nonce for each user address
  }

  validateAndIncrement(userAddress, providedNonce) {
    const currentNonce = this.nonces.get(userAddress) || 0;

    // Check if the provided nonce matches the expected nonce
    if (providedNonce !== currentNonce) {
      throw new Error(`Invalid nonce. Expected ${currentNonce}, got ${providedNonce}`);
    }

    // Increment the nonce after successful validation
    this.nonces.set(userAddress, currentNonce + 1);
    return true;
  }

  getCurrentNonce(userAddress) {
    return this.nonces.get(userAddress) || 0;
  }
}

// Usage example
const tracker = new NonceTracker();
const userAddress = "0x742d35Cc6634C0532925a3b844Bc454e4438f44e";

// First transaction - nonce should be 0
tracker.validateAndIncrement(userAddress, 0); // Success

// Second transaction - nonce should be 1
tracker.validateAndIncrement(userAddress, 1); // Success

// Replay attack attempt - trying to use nonce 0 again
try {
  tracker.validateAndIncrement(userAddress, 0); // Fails!
} catch (error) {
  console.log("Replay attack prevented:", error.message);
}
```

#### Solidity Example

Lets demonstrate via a EVM Smart Contract too.

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract ReplayProtection {
    // Mapping from user address to their current nonce
    mapping(address => uint256) public nonces;

    event TransactionExecuted(address indexed user, uint256 nonce, uint256 amount);

    /**
     * @dev Execute a transaction with replay protection
     * @param amount The amount to transfer
     * @param nonce The nonce for this transaction
     * @param signature The signature proving authorization
     */
    function executeTransaction(
        uint256 amount,
        uint256 nonce,
        bytes memory signature
    ) public {
        // Get the current nonce for the user
        uint256 currentNonce = nonces[msg.sender];

        // Verify the nonce matches what we expect
        require(nonce == currentNonce, "Invalid nonce: possible replay attack");

        // Verify the signature (simplified - in production use proper signature verification)
        bytes32 messageHash = getMessageHash(msg.sender, amount, nonce);
        require(verifySignature(messageHash, signature, msg.sender), "Invalid signature");

        // Increment the nonce to prevent replay
        nonces[msg.sender]++;

        // Execute the transaction logic here
        // ... transfer logic ...

        emit TransactionExecuted(msg.sender, nonce, amount);
    }

    /**
     * @dev Get the hash of the message to be signed
     */
    function getMessageHash(
        address user,
        uint256 amount,
        uint256 nonce
    ) public pure returns (bytes32) {
        return keccak256(abi.encodePacked(user, amount, nonce));
    }

    /**
     * @dev Verify the signature (simplified example)
     */
    function verifySignature(
        bytes32 messageHash,
        bytes memory signature,
        address expectedSigner
    ) internal pure returns (bool) {
        // In production, use ECDSA.recover from OpenZeppelin
        // This is a placeholder for demonstration
        bytes32 ethSignedMessageHash = keccak256(
            abi.encodePacked("\x19Ethereum Signed Message:\n32", messageHash)
        );

        // Actual signature verification would happen here
        // For this example, we're showing the structure
        return true; // Simplified
    }

    /**
     * @dev Get the current nonce for a user
     */
    function getNonce(address user) public view returns (uint256) {
        return nonces[user];
    }
}
```

Each user address has their own nonce stored in the `nonces` mapping.  The contract checks that the provided nonce matches the expected value, and after validation, the nonce is immediately incremented.  The nonce is included in the message hash that gets signed and the signature proves the user authorized this specific nonce value.

This pattern is used throughout Ethereum - every transaction you send has a nonce that must match your account's current nonce, preventing replay attacks at the protocol level.
