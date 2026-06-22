## Time-bound NFTs

If ERC721 contracts (NFTs) are to be used for other applications like **ticketing** or **memberships**, the NFT must expire or at least have a start and end date. A perpetual-ownership primitive doesn't fit a one-night event or a 12-month subscription.

[**EIP-5643**](https://eips.ethereum.org/EIPS/eip-5643) — "Subscription NFTs" — attempts to solve this by adding an `expiresAt` per token, plus events that let off-chain systems detect renewal and cancellation. It's a small, focused extension to ERC721 and is the closest thing to a "subscription standard" the ecosystem has.

It has a limitation, though: **it doesn't implement a "start date"**. There's `expiresAt`, but no `validFrom`. While you could argue that a *membership* is purchased and then valid from that moment on — and EIP-5643 handles that case cleanly — a *ticket to an event at a specific time* is a different shape. A ticket has both a start (the event begins at 7:30 PM) and an end (the event ends at 10 PM, or maybe at 7:30 PM the next morning if the venue is generous). EIP-5643 can model "expires at 10 PM" but it can't model "becomes valid at 7:30 PM".

In practice, teams that need both bounds either:

1. Extend EIP-5643 with their own `validFrom` field (and lose strict standards compliance).
2. Store the start time in tokenURI metadata and check it in their dApp (and lose on-chain enforcement).
3. Implement a *fully custom* expiry scheme that doesn't pretend to be a standard at all.

None of those is wonderful. It's a good reminder of the point from the previous page: *standards are interfaces, not abstractions*. EIP-5643 standardised the half of the problem the authors needed solved — the half they didn't standardise is still your job.
