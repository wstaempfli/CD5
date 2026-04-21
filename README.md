# Oat V2 Compiler

A full compiler from **Oat V2** — a statically typed, object-oriented subset of a C-like language — to **x86-64 assembly**, written in OCaml. Final project for ETH Zürich's *Compiler Design* course.

## Pipeline

```
source  →  tokens  →  AST  →  typed AST  →  LLVM IR  →  x86-64  →  a.out
```

| Stage | File | LOC |
|---|---|---:|
| Lexing | `lexer.mll` (ocamllex) | 227 |
| Parsing | `parser.mly` (menhir) | 239 |
| AST | `ast.ml` + `astlib.ml` | 586 |
| Type checking | `typechecker.ml` + `tctxt.ml` | 553 |
| IR generation | `frontend.ml` | 646 |
| x86-64 codegen | `backend.ml` | 498 |
| Runtime | `runtime.c` | — |
| Driver | `driver.ml` + `main.ml` | 195 |

~3,000 LOC of OCaml across the core pipeline.

## Language support

Primitive types, typed arrays, structs with subtyping, first-class function references, string literals via a C runtime, nested control flow, separate compilation through clang or a bundled reference backend.

## Building and running

```bash
make                                             # builds main.native
./main.native --execute-x86 oatprograms/fact.oat # compile and run
./main.native --test                             # run test harness
```

Full flag reference in [`README`](./README).

## Tests

- `gradedtests.ml` — course-provided test suite
- `studenttests.ml` — additional cases
