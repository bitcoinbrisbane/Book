# Related Work

Game description languages have been explored in several contexts. The Game Description Language (GDL) provides a logic-based formalism for general game playing, but lacks domain-specific primitives for card games. Poker-specific formalisms exist but do not generalize to other card games.

In the functional programming community, domain-specific languages embedded in Haskell have proven effective for capturing domain semantics while leveraging the host language's type system. Our approach draws inspiration from this tradition.

The cryptographic requirements for fair card games have been studied extensively in the context of mental poker, providing the theoretical foundation for our deterministic shuffling primitives.

## References

- Love, N., Hinrichs, T., Haley, D., Schkufza, E., & Genesereth, M. (2008). General Game Playing: Game Description Language Specification. *Stanford Logic Group Technical Report LG-2006-01*.

- Billings, D., Davidson, A., Schaeffer, J., & Szafron, D. (2002). The challenge of poker. *Artificial Intelligence*, 134(1-2), 201-240.

- Hudak, P. (1996). Building domain-specific embedded languages. *ACM Computing Surveys*, 28(4es), 196-es.

- Shamir, A., Rivest, R. L., & Adleman, L. M. (1981). Mental Poker. *The Mathematical Gardner*, 37-43.
