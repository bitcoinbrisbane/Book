## Layers

Building a project from scratch can be fun and also daunting. We have to consider every possible architectural option and patterns. These can be for now and the future. There are fundamental computer science ideas that need to be researched, to up and coming language choices and patterns.

Each has its advantages and disadvantages. For example, a popular Ethereum node Geth is written in Go (GoLang). This language is known to be performant because of its … but it might not be commonly used or taught. Does getting your project off the ground and tested outweigh some of the other benefits?

My approach is always to ship it dirty and validate. Speed to validate and speed to market trumps all, then revisit, refactor, rethink. It might be better to port an existing code base to a new language once you've thought through the challenges of the build.

## What are layers?

Layers in software applications refer to an architectural pattern that organizes code into distinct horizontal levels, each with specific responsibilities. Each layer typically only communicates with the layers directly above or below it, creating a clear separation of concerns.

From the "ground" up.

### Persistence layer

Where our client application stores and retrieves data, in our case blocks and transactions.

### API layer

How our front end application interfaces with the core logic.

### RPC layer

The protocol by which other clients communicate to our application, and how nodes will sync.

### Notification layer

How clients subscribe to our application for changes of state.

### Presentation layer

The UI of the application.
