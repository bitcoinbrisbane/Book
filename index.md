# Introduction

Enough of the hype, the never ending stream of conferences. The POCs, the MVPs, the workshops, the consultations, the enormous amounts of blockchains to build on. The infinite amount of tech choices to wade through. This book will teach you blockchain from the trenches, with a clear project to build along the way. Each chapter I will teach you a way to build an entire blockchain application. From protocol to application. It's about doing it one way out of literally millions of ways to build a project.

After you have built the stack you will then have an understanding of the fundamentals of blockchains, cryptography, distributed systems, open source, modern frameworks, security concerns, economic motivations and challenges, and probably most importantly: Why! What is the philosophy behind this movement?

I will steer clear of any legal arguments, because honestly, I just don't care. I'm not going to wait for "regulatory frameworks" to catch up. When email took over the world as a method of communication, it didn't ask for permission nor should it. It just did because it was a better method of communication. Uber didn't wait for permission, in fact, it forced lawmakers to change what taxis are.

Blockchains are a technology to do things better. This will explain the above by doing, not talking.

It's time to build (buidl).

> **Buidl aside:** In the wonderful world of crypto, there are many "in house" jokes, such as hodl and buidl.

## Topics Covered

- About me and my motivations to write this book
- A brief history of technologies that make blockchain possible, and early blockchains themselves
- What is a protocol
- The internet itself
- Public key & private key
- One way functions
- Peer to peer protocols
- Distributed hash tables
- An overview of the project we are building
- Why build a poker app?
- Why it needs to be on chain, both technically and philosophically
- Censorship resistant money
- What is decentralized? Goal vs requirement
- Importance of decentralization
- Challenges of decentralization
- Game theory attacks
- Regulation
- A consensus protocol. I've chosen one so you don't have to.
- Modifying the core protocol and scripting language to suit our custom project
- Implementing the core protocol library / transportation layer in Go
- What is Go
- What is a "low level" language
- Creating our own domain specific programming language
- Building a compiler for our own domain language
- Formal verification
- Creating an SDK to "dog food" our protocol. Python and NodeJS
- What an SDK is
- Creating a client library
- It's time to mine. Now we have all the pieces we need, it's time to mine our own chain including mining software in C and discuss the challenges of mining
- What is mining
- Why it's important
- The application. A simple hosted web application to consume our chain via our SDKs using React.
- Going more decentralized. Taking our hosted web application to a cross OS desktop application using Electrum.
- Open source the entire thing. Yes, give away all our hard work.
- Working collaboratively in a decentralized organization through GitHub, pull requests, bug bounties and tokens.
