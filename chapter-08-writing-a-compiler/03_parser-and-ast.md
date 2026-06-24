## The parser and the AST

The lexer handed us a flat list of tokens. That list has no structure: it doesn't know that a `{` opens a block that a later `}` closes, or that `function greet()` begins a function. Imposing that structure is the job of the **parser**, and the structure it produces is the **abstract syntax tree**, or AST.

### What the AST is

An AST is a tree of objects that captures the *meaning* of the program while discarding the *syntax* used to write it. The semicolons, the braces, the exact spelling of keywords: all of that was scaffolding to help the parser find the structure, and once found, the scaffolding is dropped. What remains is the essence.

For our `Greeter.sol`, the whole contract collapses to a handful of nested objects:

```
Contract(name="Greeter")
├── state_vars: ["greeting"]
├── constructor: [ Assign(target="greeting", value=42) ]
└── functions:
    ├── Function(name="greet", params=[], mutates=False)
    │     └── body: [ Return(name="greeting") ]
    └── Function(name="setGreeting", params=["g"], mutates=True)
          └── body: [ Assign(target="greeting", value="g") ]
```

That tree *is* the program, as far as the rest of the compiler is concerned. Code generation never looks at the source text again; it walks this tree.

### The node types

We define one small dataclass per kind of node. The grammar of our subset is tiny, so the AST is tiny too:

```python
@dataclass
class Assign:
    target: str          # variable name on the left
    value: int | str     # an int literal, or a parameter name

@dataclass
class Return:
    name: str            # the variable being returned

@dataclass
class Function:
    name: str
    params: list[str]
    body: list           # Assign / Return nodes
    mutates: bool        # True unless the function is `view`

@dataclass
class Contract:
    name: str
    state_vars: list[str]
    constructor: list[Assign]
    functions: list[Function]
```

A real Solidity AST has dozens of node types: expressions, loops, mappings, modifiers, inheritance. Ours has four. But the principle is identical at any scale: each node type is a shape the language can take, and a program is those shapes nested inside one another.

### How the parser builds it

Our parser is a **recursive descent** parser, the most common hand-written kind. The idea is one method per grammar rule, and the methods call each other the way the grammar nests. `parse()` expects a contract, which expects members, where each member is a state variable, a constructor, or a function, and each of those expects its own pieces in turn.

The parser walks the token stream with three little helpers that do all the work:

```python
def expect(self, kind):       # consume a token, error if it's the wrong kind
def accept(self, kind):       # consume a token only if it matches; else None
def peek(self):               # look at the next token without consuming it
```

With those, a grammar rule reads almost like prose. Here is the rule for a function:

```python
def _parse_function(self):
    self.expect("IDENT")                 # the word "function"
    name = self.expect("IDENT").value
    params = self._parse_params()

    mutates = True
    while self.peek().kind != "LBRACE":  # skim modifiers until the body
        tok = self.next()
        if tok.value == "view":
            mutates = False

    body = self._parse_block(params)
    return Function(name, params, body, mutates)
```

Each `expect` both advances the parser and asserts that the token is what the grammar demands. If it isn't, we raise a syntax error pointing at the offending position. This is the parser doing its second job: not just building the tree for valid input, but **rejecting invalid input** instead of silently miscompiling it.

### Why structure beats a flat list

It's tempting to skip the AST and generate code straight from the token stream. For a calculator you might get away with it. But the moment the language nests, a tree is the only sane representation: a function contains statements, a statement contains expressions, an expression contains more expressions. The tree makes that nesting explicit and walkable.

It also cleanly separates concerns. The parser worries about *grammar* ("is this a valid function?"). The next stage worries about *meaning* ("what bytecode does this function become?"). The AST is the contract between them, and keeping that boundary sharp is what stops a compiler from turning into an unmaintainable tangle. With the tree in hand, we're ready to generate code.
