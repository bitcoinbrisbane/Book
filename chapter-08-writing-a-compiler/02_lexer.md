## The lexer

The first stage of any compiler is the **lexer** (also called the scanner or tokeniser). Its job is narrow and mechanical: read the raw source text, one character at a time, and group those characters into **tokens**. A token is the smallest meaningful unit of the language, a word, a number, a piece of punctuation. The lexer knows nothing about grammar or meaning. It just chops.

Given our `Greeter.sol`, the lexer turns this:

```solidity
uint256 greeting;
```

into this stream of tokens:

```
IDENT("uint256")  IDENT("greeting")  SEMI(";")
```

Notice what's already gone: the whitespace and the comments. They mattered to a human reading the file, but they carry no meaning for the compiler, so the lexer throws them away. Everything downstream gets to work with a clean, flat list of tokens instead of a soup of characters.

### How ours works

Our lexer is built on a single idea: every token has a *shape*, and shapes can be described with regular expressions. We list each token kind alongside the pattern that matches it, in priority order:

```python
TOKEN_SPEC = [
    ("WS",            r"[ \t\r\n]+"),     # whitespace (skipped)
    ("LINE_COMMENT",  r"//[^\n]*"),       # // ...      (skipped)
    ("BLOCK_COMMENT", r"/\*.*?\*/"),      # /* ... */   (skipped)
    ("NUMBER",        r"\d+"),
    ("IDENT",         r"[A-Za-z_]\w*"),
    ("LBRACE",        r"\{"),
    ("RBRACE",        r"\}"),
    ("LPAREN",        r"\("),
    ("RPAREN",        r"\)"),
    ("SEMI",          r";"),
    ("ASSIGN",        r"="),
    # ...
]
```

We compile all of these into one big alternation and walk the source with it, emitting a `Token` for every match. Tokens tagged as whitespace or comments are matched and then dropped, so they never reach the parser. The order matters: more specific patterns must come before more general ones, which is why the comment rules sit above the punctuation rules.

```python
@dataclass
class Token:
    kind: str
    value: str
    pos: int     # byte offset, kept for error messages
```

That `pos` field is worth keeping even in a toy. When something later goes wrong, the difference between "syntax error" and "syntax error at column 14" is the difference between a usable compiler and an infuriating one.

### Where lexers get hard

Ours is easy because Solidity's tokens are easy. Real lexers get subtle fast:

- **Maximal munch.** Does `==` lex as two `=` tokens or one `==`? The rule is "take the longest match," and getting it wrong silently breaks the language.
- **Keywords vs identifiers.** `return` looks exactly like a variable name to a regex. Most lexers match it as an identifier first, then check a keyword table. We sidestep this by letting the *parser* recognise words like `contract` and `return` by their value.
- **Numbers.** Hex, scientific notation, underscores in literals, decimals: each is another pattern, and each is a place to get the boundary wrong.

But the shape never changes: characters in, tokens out, meaning deferred. With the token stream in hand, we can move on to the stage that actually imposes structure: the parser.
