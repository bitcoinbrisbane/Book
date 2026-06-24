#!/usr/bin/env python3
"""Tests for the Greeter compiler.

The unit tests check each stage in isolation (lexing, parsing, selectors, and a
few bytecode invariants) with no external dependencies. The end-to-end EVM test
that actually deploys the bytecode and calls greet()/setGreeting lives in the
README as a runnable anvil+cast snippet, since it needs Foundry installed.

Run:  python3 -m pytest test_greeter_compiler.py   (or just: python3 test_greeter_compiler.py)
"""

from greeter_compiler import (
    lex,
    Parser,
    CodeGen,
    compile_source,
    function_selector,
    _keccak256_pure,
)

SOURCE = open(__file__.rsplit("/", 1)[0] + "/Greeter.sol").read()


def test_lexer_skips_comments_and_whitespace():
    tokens = lex("// hi\nuint256 x;")
    kinds = [t.kind for t in tokens]
    assert kinds == ["IDENT", "IDENT", "SEMI", "EOF"]


def test_parser_builds_contract():
    contract = Parser(lex(SOURCE)).parse()
    assert contract.name == "Greeter"
    assert contract.state_vars == ["greeting"]
    assert [a.target for a in contract.constructor] == ["greeting"]
    assert [a.value for a in contract.constructor] == [42]
    names = sorted(f.name for f in contract.functions)
    assert names == ["greet", "setGreeting"]
    greet = next(f for f in contract.functions if f.name == "greet")
    assert greet.mutates is False  # it's `view`
    setter = next(f for f in contract.functions if f.name == "setGreeting")
    assert setter.params == ["g"]
    assert setter.mutates is True


def test_selectors_match_known_values():
    # These are the canonical 4-byte selectors (verifiable with `cast sig`).
    assert function_selector("greet()") == 0xCFAE3217
    assert function_selector("setGreeting(uint256)") == 0xB2010978


def test_pure_keccak_matches_known_digest():
    # keccak256("") well-known empty-string digest.
    empty = _keccak256_pure(b"")
    assert empty.hex() == (
        "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
    )


def test_runtime_and_deploy_are_nonempty_bytes():
    _, runtime, deploy = compile_source(SOURCE)
    assert isinstance(runtime, bytes) and len(runtime) > 0
    assert isinstance(deploy, bytes) and len(deploy) > len(runtime)
    # Deploy code ends by returning the runtime, so the runtime must appear
    # verbatim as the tail of the deploy bytecode.
    assert deploy.endswith(runtime)


def test_dispatcher_size_prediction_is_exact():
    # CodeGen asserts this internally; calling runtime() exercises it.
    contract = Parser(lex(SOURCE)).parse()
    gen = CodeGen(contract)
    gen.runtime()  # raises AssertionError if the prediction is wrong


if __name__ == "__main__":
    fns = [v for k, v in sorted(globals().items()) if k.startswith("test_")]
    for fn in fns:
        fn()
        print(f"ok  {fn.__name__}")
    print(f"\n{len(fns)} passed")
