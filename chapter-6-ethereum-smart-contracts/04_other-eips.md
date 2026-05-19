## Other notable EIPs

It would be remiss of me not to give a shout out to some other EIPs that have been widely adopted, or that I think are worth a look.

Before we dive in, one observation worth filing away: **EIPs are normally an *interface*, not an *abstraction***. They tell you the function signatures and event shapes you need to honour — they don't tell you how your business logic should work. I've had clients say "oh, you can just use this EIP" as if reaching for a standard makes the design decisions for them. It doesn't. The standard makes your contract *interoperable* with the rest of the ecosystem; the business logic is still on you.

With that caveat in hand, the next few pages cover the three EIPs you're most likely to meet in production:

- **ERC777** — an extension of ERC20 that adds operators and hooks.
- **ERC721** — the NFT standard. The second most-used contract interface on Ethereum.
- **Time-bound NFT extensions** (EIP-5643 and friends) — what you'd reach for if you wanted an NFT to *expire*.
