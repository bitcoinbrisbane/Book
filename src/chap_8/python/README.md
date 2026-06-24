# A tiny Solidity → EVM compiler

The runnable code for Chapter 8. `greeter_compiler.py` compiles a small subset
of Solidity (`Greeter.sol`) all the way to deployable EVM bytecode, through the
same stages a real compiler uses:

```
Greeter.sol  ──lex──>  tokens  ──parse──>  AST  ──codegen──>  EVM bytecode
```

No third-party packages are required: keccak256 (for function selectors) has a
pure-Python fallback, used automatically if `pycryptodome` isn't installed.

## Compile

```sh
python3 greeter_compiler.py Greeter.sol
```

Prints the function selectors and both the runtime and deploy bytecode as hex.

## Unit tests

```sh
python3 test_greeter_compiler.py
```

Covers the lexer, parser, selectors, the pure-Python keccak, and a couple of
bytecode invariants. No external tools needed.

## End-to-end: run it on a real EVM

The proof that the bytecode is correct is that an actual EVM runs it. With
[Foundry](https://book.getfoundry.sh/) installed (`anvil` + `cast`):

```sh
anvil --silent &              # local EVM on :8545
RPC=http://127.0.0.1:8545
PK=0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80   # anvil acct 0

DEPLOY=$(python3 -c "from greeter_compiler import compile_source; \
  print('0x'+compile_source(open('Greeter.sol').read())[2].hex())")

ADDR=$(cast send --rpc-url $RPC --private-key $PK --create $DEPLOY --json \
  | python3 -c "import sys,json; print(json.load(sys.stdin)['contractAddress'])")

cast call --rpc-url $RPC $ADDR 'greet()(uint256)'         # -> 42
cast send --rpc-url $RPC --private-key $PK $ADDR 'setGreeting(uint256)' 99
cast call --rpc-url $RPC $ADDR 'greet()(uint256)'         # -> 99
```

The constructor seeds `greeting = 42`; `greet()` returns it; `setGreeting`
writes storage and `greet()` reflects the new value.

## The supported subset

Deliberately tiny, so the codegen stays readable:

- one contract,
- a single `uint256` state variable (storage slot 0),
- a constructor whose body is `name = <number>;` assignments,
- functions whose bodies are `return <var>;` or `<var> = <param>;`,
- at most one `uint256` parameter per function.

Anything outside this subset is rejected by the parser rather than miscompiled.
Returning a dynamic `string` (the "real" Greeter) is left as an extension: it
needs ABI encoding of a length-prefixed, word-padded dynamic type, which is a
lesson in the ABI rather than in compilation.
