Let's think about our intended audience for our language, and the goals for the language

Our primary goal is to actually limit the amount of freedom developers can have to build out turn based card games on our layer two.  The idea behind this is to constrict developers to think more about the game and the mechanics, rather than the implementation of the game.

In the words of the Columbia University language Joker, "The goal of our language is to allow programmers to succinctly describe the rules of a card game and to create a runtime card game engine."

While there isn't too much information on this white paper, save from a few PDFs around 2003, it gives us a base line to begin our language.

This is a fine line between OP codes and a high level language.


Primative Types

	DECK
	CARD
	PLAYER

Operators

	==
	!=

The game must start with the game type, and a name or identifier.  We will use inheritance later.
