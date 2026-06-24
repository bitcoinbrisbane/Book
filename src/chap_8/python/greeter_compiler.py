#!/usr/bin/env python3
"""A tiny Solidity-to-EVM-bytecode compiler for the Greeter contract.

This is TEACHING CODE for Chapter 8. It compiles a deliberately small subset of
Solidity, just enough to take Greeter.sol and emit deployable EVM bytecode that
actually runs. The pipeline mirrors a real compiler:

    source text  --lex-->  tokens  --parse-->  AST  --codegen-->  bytecode

The supported subset:
  * one contract,
  * a single `uint256` state variable in storage slot 0,
  * a constructor body of `name = <number>;` assignments,
  * functions whose bodies are `return <var>;` or `<var> = <param>;`.

That is enough for a getter and a setter. Everything else is intentionally out
of scope, and the parser will reject it rather than silently miscompile.
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass, field

# ---------------------------------------------------------------------------
# Stage 1: the lexer  (source text -> tokens)
# ---------------------------------------------------------------------------
#
# The lexer's only job is to chop the raw source into a flat list of tokens,
# throwing away whitespace and comments. It knows nothing about grammar; it just
# recognises the shapes of words, numbers, and punctuation.

TOKEN_SPEC = [
    ("WS",       r"[ \t\r\n]+"),                 # whitespace (skipped)
    ("LINE_COMMENT",  r"//[^\n]*"),              # // ...        (skipped)
    ("BLOCK_COMMENT", r"/\*.*?\*/"),             # /* ... */     (skipped)
    ("NUMBER",   r"\d+"),
    ("IDENT",    r"[A-Za-z_]\w*"),
    ("LBRACE",   r"\{"),
    ("RBRACE",   r"\}"),
    ("LPAREN",   r"\("),
    ("RPAREN",   r"\)"),
    ("SEMI",     r";"),
    ("ASSIGN",   r"="),
    ("CARET",    r"\^"),                          # appears in the pragma line
    ("DOT",      r"\."),
    ("MISC",     r"[<>/\-]"),                     # tolerated inside pragma etc.
]

MASTER_RE = re.compile(
    "|".join(f"(?P<{name}>{pattern})" for name, pattern in TOKEN_SPEC),
    re.DOTALL,
)

SKIP = {"WS", "LINE_COMMENT", "BLOCK_COMMENT"}


@dataclass
class Token:
    kind: str
    value: str
    pos: int


def lex(source: str) -> list[Token]:
    tokens: list[Token] = []
    for match in MASTER_RE.finditer(source):
        kind = match.lastgroup
        value = match.group()
        if kind in SKIP:
            continue
        tokens.append(Token(kind, value, match.start()))
    tokens.append(Token("EOF", "", len(source)))
    return tokens


# ---------------------------------------------------------------------------
# Stage 2: the parser  (tokens -> AST)
# ---------------------------------------------------------------------------
#
# The parser imposes grammar on the flat token stream, producing an abstract
# syntax tree: a structured object that says "this is a contract, with these
# state variables and these functions." A few things (the license comment and
# `pragma` line) are skipped wholesale; they don't affect codegen.


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
    body: list           # list of Assign / Return
    mutates: bool        # True if it writes state (no `view`)


@dataclass
class Contract:
    name: str
    state_vars: list[str]
    constructor: list[Assign]
    functions: list[Function]


class Parser:
    def __init__(self, tokens: list[Token]):
        self.tokens = tokens
        self.i = 0

    # --- token helpers ---
    def peek(self) -> Token:
        return self.tokens[self.i]

    def next(self) -> Token:
        tok = self.tokens[self.i]
        self.i += 1
        return tok

    def expect(self, kind: str) -> Token:
        tok = self.next()
        if tok.kind != kind:
            raise SyntaxError(f"expected {kind}, got {tok.kind} '{tok.value}' at {tok.pos}")
        return tok

    def accept(self, kind: str) -> Token | None:
        if self.peek().kind == kind:
            return self.next()
        return None

    # --- grammar ---
    def parse(self) -> Contract:
        self._skip_pragma()
        self.expect("IDENT")  # the word "contract"
        name = self.expect("IDENT").value
        self.expect("LBRACE")

        state_vars: list[str] = []
        constructor: list[Assign] = []
        functions: list[Function] = []

        while self.peek().kind != "RBRACE":
            word = self.peek().value
            if word == "uint256":
                state_vars.append(self._parse_state_var())
            elif word == "constructor":
                constructor = self._parse_constructor()
            elif word == "function":
                functions.append(self._parse_function())
            else:
                tok = self.peek()
                raise SyntaxError(f"unsupported member '{tok.value}' at {tok.pos}")

        self.expect("RBRACE")
        return Contract(name, state_vars, constructor, functions)

    def _skip_pragma(self) -> None:
        # Skip everything up to and including the first `;` that belongs to the
        # `pragma solidity ...;` line. The SPDX comment was already dropped by
        # the lexer. If there's no pragma, this is a no-op.
        if self.peek().value == "pragma":
            while self.next().kind != "SEMI":
                pass

    def _parse_state_var(self) -> str:
        self.expect("IDENT")            # uint256
        name = self.expect("IDENT").value
        self.expect("SEMI")
        return name

    def _parse_constructor(self) -> list[Assign]:
        self.expect("IDENT")            # constructor
        self.expect("LPAREN")
        self.expect("RPAREN")
        return self._parse_block(params=[])

    def _parse_function(self) -> Function:
        self.expect("IDENT")            # function
        name = self.expect("IDENT").value
        params = self._parse_params()

        # Consume modifiers/return declaration until the body's `{`. We only care
        # whether the function is `view` (read-only) for codegen purposes.
        mutates = True
        while self.peek().kind != "LBRACE":
            tok = self.next()
            if tok.value == "view":
                mutates = False
            if tok.kind == "EOF":
                raise SyntaxError("unexpected EOF in function header")

        body = self._parse_block(params)
        return Function(name, params, body, mutates)

    def _parse_params(self) -> list[str]:
        # Our Greeter subset has at most one parameter per function, so we don't
        # need to handle comma-separated lists. Each param is `<type> <name>`.
        self.expect("LPAREN")
        params: list[str] = []
        while self.peek().kind != "RPAREN":
            self.expect("IDENT")        # the type, e.g. uint256
            params.append(self.expect("IDENT").value)
        self.expect("RPAREN")
        return params

    def _parse_block(self, params: list[str]) -> list:
        self.expect("LBRACE")
        stmts: list = []
        while self.peek().kind != "RBRACE":
            stmts.append(self._parse_statement(params))
        self.expect("RBRACE")
        return stmts

    def _parse_statement(self, params: list[str]):
        if self.peek().value == "return":
            self.next()
            name = self.expect("IDENT").value
            self.expect("SEMI")
            return Return(name)

        # Otherwise an assignment: <ident> = <number | param> ;
        target = self.expect("IDENT").value
        self.expect("ASSIGN")
        rhs = self.next()
        if rhs.kind == "NUMBER":
            value: int | str = int(rhs.value)
        elif rhs.kind == "IDENT":
            value = rhs.value
        else:
            raise SyntaxError(f"bad right-hand side '{rhs.value}' at {rhs.pos}")
        self.expect("SEMI")
        return Assign(target, value)


# ---------------------------------------------------------------------------
# Stage 3: code generation  (AST -> EVM bytecode)
# ---------------------------------------------------------------------------
#
# This is where we turn the tree into EVM opcodes. The EVM is a stack machine:
# instructions push and pop 256-bit words. We build up a list of bytes, using a
# small opcode table and helper for the only multi-byte instruction we need
# (PUSH).

OPCODES = {
    "STOP": 0x00,
    "ADD": 0x01,
    "EQ": 0x14,
    "ISZERO": 0x15,
    "CALLDATALOAD": 0x35,
    "CALLDATASIZE": 0x36,
    "CALLDATACOPY": 0x37,
    "CODECOPY": 0x39,
    "POP": 0x50,
    "MLOAD": 0x51,
    "MSTORE": 0x52,
    "SLOAD": 0x54,
    "SSTORE": 0x55,
    "JUMP": 0x56,
    "JUMPI": 0x57,
    "JUMPDEST": 0x5B,
    "DUP1": 0x80,
    "SHR": 0x1C,
    "RETURN": 0xF3,
    "REVERT": 0xFD,
}


def push(value: int) -> bytes:
    """Emit a PUSHn instruction carrying `value` as its immediate operand."""
    if value == 0:
        body = b"\x00"
    else:
        length = (value.bit_length() + 7) // 8
        body = value.to_bytes(length, "big")
    push_opcode = 0x60 + (len(body) - 1)   # PUSH1 is 0x60, PUSH2 0x61, ...
    return bytes([push_opcode]) + body


def op(name: str) -> bytes:
    return bytes([OPCODES[name]])


def _keccak256(data: bytes) -> bytes:
    """keccak256 with a pure-Python fallback so the demo runs with no deps."""
    try:
        from Crypto.Hash import keccak
        k = keccak.new(digest_bits=256)
        k.update(data)
        return k.digest()
    except Exception:
        return _keccak256_pure(data)


def function_selector(signature: str) -> int:
    return int.from_bytes(_keccak256(signature.encode())[:4], "big")


class CodeGen:
    """Generate runtime bytecode for the contract, then wrap it in deploy code."""

    def __init__(self, contract: Contract):
        self.c = contract
        # Map each state variable to a storage slot, in declaration order.
        self.slots = {name: i for i, name in enumerate(contract.state_vars)}

    # --- runtime code: the dispatcher + each function body ---
    def runtime(self) -> bytes:
        # The runtime begins with a dispatcher: load the 4-byte selector from
        # calldata, compare it against each function's selector, and jump to the
        # matching body. We assemble in two passes so we can resolve jump
        # targets to real byte offsets.
        bodies: dict[str, bytes] = {}
        for fn in self.c.functions:
            bodies[fn.name] = self._function_body(fn)

        # First, lay out the bodies after the dispatcher to learn their offsets.
        # The dispatcher's size depends on the number of functions and is fixed
        # per function (a known, constant instruction sequence), so we can
        # compute it up front.
        dispatcher_size = self._dispatcher_size()

        offsets: dict[str, int] = {}
        cursor = dispatcher_size
        for fn in self.c.functions:
            offsets[fn.name] = cursor
            cursor += len(bodies[fn.name])

        dispatcher = self._dispatcher(offsets)
        assert len(dispatcher) == dispatcher_size, (
            f"dispatcher size mismatch: predicted {dispatcher_size}, got {len(dispatcher)}"
        )

        code = dispatcher
        for fn in self.c.functions:
            code += bodies[fn.name]
        return code

    def _signature(self, fn: Function) -> str:
        arg_types = ",".join("uint256" for _ in fn.params)
        return f"{fn.name}({arg_types})"

    # The dispatcher, per function, is this fixed sequence:
    #   DUP1 PUSH4 <selector> EQ PUSH2 <dest> JUMPI
    # plus a one-time prologue that loads the selector, and a trailing REVERT.
    _PROLOGUE = None

    def _prologue(self) -> bytes:
        # Load calldata[0:4] as the selector: read the first word, shift right
        # by 224 bits (256 - 32) to isolate the top 4 bytes.
        return (
            push(0)
            + op("CALLDATALOAD")
            + push(224)
            + op("SHR")
        )

    def _dispatch_entry(self, sig_selector: int, dest: int) -> bytes:
        # The selector is always 4 bytes, so PUSH4. The destination uses a fixed
        # PUSH2 so this entry has a constant width regardless of the actual
        # offset, which keeps _dispatcher_size exact.
        return (
            op("DUP1")
            + push4(sig_selector)
            + op("EQ")
            + push2(dest)
            + op("JUMPI")
        )

    def _dispatcher(self, offsets: dict[str, int]) -> bytes:
        out = self._prologue()
        for fn in self.c.functions:
            sel = function_selector(self._signature(fn))
            out += self._dispatch_entry(sel, offsets[fn.name])
        # No selector matched: revert with empty data.
        out += push(0) + push(0) + op("REVERT")
        return out

    def _dispatcher_size(self) -> int:
        size = len(self._prologue())
        for fn in self.c.functions:
            # Each entry is a fixed-length instruction sequence regardless of the
            # actual selector/dest values, because PUSH4 and PUSH2 have constant
            # widths. Build a dummy to measure it.
            size += len(self._dispatch_entry(0xFFFFFFFF, 0xFFFF))
        size += len(push(0) + push(0) + op("REVERT"))
        return size

    def _function_body(self, fn: Function) -> bytes:
        out = op("JUMPDEST")  # a valid landing pad for the dispatcher's JUMPI
        for stmt in fn.body:
            out += self._statement(fn, stmt)
        # A view getter returns; a setter just stops.
        if not any(isinstance(s, Return) for s in fn.body):
            out += op("STOP")
        return out

    def _statement(self, fn: Function, stmt) -> bytes:
        if isinstance(stmt, Return):
            slot = self.slots[stmt.name]
            # Load the storage value, store it in memory at 0x00, return 32 bytes.
            return (
                push(slot) + op("SLOAD")        # value
                + push(0) + op("MSTORE")        # mem[0] = value
                + push(32) + push(0) + op("RETURN")
            )
        if isinstance(stmt, Assign):
            slot = self.slots[stmt.target]
            if isinstance(stmt.value, int):
                value_code = push(stmt.value)
            else:
                # A parameter: it lives in calldata right after the 4-byte
                # selector, i.e. at calldata offset 4.
                arg_index = fn.params.index(stmt.value)
                value_code = push(4 + arg_index * 32) + op("CALLDATALOAD")
            return value_code + push(slot) + op("SSTORE")
        raise TypeError(f"unknown statement {stmt!r}")

    # --- constructor / deploy code ---
    def deploy(self) -> bytes:
        """Wrap the runtime in init code that runs the constructor and then
        returns the runtime bytecode to be stored as the contract."""
        runtime = self.runtime()

        # Constructor body: run each assignment as an SSTORE.
        ctor = b""
        for stmt in self.c.constructor:
            slot = self.slots[stmt.target]
            assert isinstance(stmt.value, int), "constructor only assigns literals"
            ctor += push(stmt.value) + push(slot) + op("SSTORE")

        # The classic deploy trailer: CODECOPY the runtime to memory then RETURN
        # it. We need the runtime's offset within the full init bytecode, which
        # depends on the size of the copier itself, so we compute it with the
        # PUSH widths pinned to 2 bytes for stability.
        #
        #   PUSH2 <len> PUSH2 <offset> PUSH1 0 CODECOPY
        #   PUSH2 <len> PUSH1 0 RETURN
        rlen = len(runtime)

        def copier(offset: int) -> bytes:
            return (
                push2(rlen) + push2(offset) + push(0) + op("CODECOPY")
                + push2(rlen) + push(0) + op("RETURN")
            )

        # offset = len(ctor) + len(copier). copier length is constant once we
        # force PUSH2, so solve directly.
        copier_len = len(copier(0))
        runtime_offset = len(ctor) + copier_len
        return ctor + copier(runtime_offset) + runtime


def push2(value: int) -> bytes:
    """A PUSH2 with a fixed 2-byte operand, for stable offset arithmetic."""
    return bytes([0x61]) + value.to_bytes(2, "big")


def push4(value: int) -> bytes:
    """A PUSH4 with a fixed 4-byte operand, used for function selectors."""
    return bytes([0x63]) + value.to_bytes(4, "big")


# ---------------------------------------------------------------------------
# Pure-Python keccak256 fallback (so the demo has zero dependencies)
# ---------------------------------------------------------------------------

def _keccak256_pure(message: bytes) -> bytes:
    # Minimal Keccak-256 (the hash Ethereum uses; note it predates the NIST
    # SHA-3 padding change, so this is "original" Keccak, not SHA3-256).
    RC = [
        0x0000000000000001, 0x0000000000008082, 0x800000000000808A,
        0x8000000080008000, 0x000000000000808B, 0x0000000080000001,
        0x8000000080008081, 0x8000000000008009, 0x000000000000008A,
        0x0000000000000088, 0x0000000080008009, 0x000000008000000A,
        0x000000008000808B, 0x800000000000008B, 0x8000000000008089,
        0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
        0x000000000000800A, 0x800000008000000A, 0x8000000080008081,
        0x8000000000008080, 0x0000000080000001, 0x8000000080008008,
    ]
    ROT = [
        [0, 36, 3, 41, 18],
        [1, 44, 10, 45, 2],
        [62, 6, 43, 15, 61],
        [28, 55, 25, 21, 56],
        [27, 20, 39, 8, 14],
    ]
    MASK = (1 << 64) - 1

    def rol(x, n):
        return ((x << n) | (x >> (64 - n))) & MASK

    def keccak_f(state):
        for rnd in range(24):
            # theta
            c = [state[x][0] ^ state[x][1] ^ state[x][2] ^ state[x][3] ^ state[x][4] for x in range(5)]
            d = [c[(x - 1) % 5] ^ rol(c[(x + 1) % 5], 1) for x in range(5)]
            for x in range(5):
                for y in range(5):
                    state[x][y] ^= d[x]
            # rho + pi
            b = [[0] * 5 for _ in range(5)]
            for x in range(5):
                for y in range(5):
                    b[y][(2 * x + 3 * y) % 5] = rol(state[x][y], ROT[x][y])
            # chi
            for x in range(5):
                for y in range(5):
                    state[x][y] = b[x][y] ^ ((~b[(x + 1) % 5][y]) & b[(x + 2) % 5][y]) & MASK
            # iota
            state[0][0] ^= RC[rnd]
        return state

    rate = 136  # bytes, for 256-bit output
    # padding (Keccak pad10*1 with 0x01 domain byte)
    msg = bytearray(message)
    msg.append(0x01)
    while len(msg) % rate != 0:
        msg.append(0x00)
    msg[-1] ^= 0x80

    state = [[0] * 5 for _ in range(5)]
    for block_start in range(0, len(msg), rate):
        block = msg[block_start:block_start + rate]
        for i in range(rate // 8):
            lane = int.from_bytes(block[i * 8:i * 8 + 8], "little")
            state[i % 5][i // 5] ^= lane
        keccak_f(state)

    out = bytearray()
    for i in range(4):  # 4 lanes * 8 bytes = 32-byte digest
        out += state[i % 5][i // 5].to_bytes(8, "little")
    return bytes(out[:32])


# ---------------------------------------------------------------------------
# Driver
# ---------------------------------------------------------------------------

def compile_source(source: str) -> tuple[Contract, bytes, bytes]:
    tokens = lex(source)
    contract = Parser(tokens).parse()
    gen = CodeGen(contract)
    return contract, gen.runtime(), gen.deploy()


def main() -> None:
    path = sys.argv[1] if len(sys.argv) > 1 else "Greeter.sol"
    with open(path) as f:
        source = f.read()

    contract, runtime, deploy = compile_source(source)

    print(f"contract: {contract.name}")
    print(f"  state slots: {[ (v, i) for i, v in enumerate(contract.state_vars) ]}")
    for fn in contract.functions:
        sig = f"{fn.name}({','.join('uint256' for _ in fn.params)})"
        sel = function_selector(sig)
        print(f"  {sig:24s} selector 0x{sel:08x}  {'view' if not fn.mutates else 'mutating'}")
    print()
    print(f"runtime bytecode ({len(runtime)} bytes):")
    print("  0x" + runtime.hex())
    print(f"deploy bytecode  ({len(deploy)} bytes):")
    print("  0x" + deploy.hex())


if __name__ == "__main__":
    main()
