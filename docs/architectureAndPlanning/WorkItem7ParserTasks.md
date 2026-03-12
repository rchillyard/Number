# Work Item 7 — Parser Tasks for Euler / Complex Support

*Number Library — Architecture & Planning — March 2026*
*Milestone: Version 1.8*

---

## Overview

The target parser is `LaTeXParser.scala` (package `com.phasmidsoftware.number.parse`).
It produces `BiFunction`/`UniFunction` expression trees; the simplification pipeline
then converts those trees to `Euler` form. Mathematical knowledge stays out of the
parser — the parser stays dumb.

---

## Task 1 — Add `i` as a math symbol

`i` (the imaginary unit) is not currently recognised by `LaTeXParser.mathSymbol`.
It needs to map to the expression constant `I`.

**Change:** add to `mathSymbol`:
```scala
("i" ^^^ I) | ("𝑖" ^^^ I)
```

**Result:** `i` in any expression position (e.g. `a + bi`, `e^(iθ)`) will parse
correctly as the `I` named constant.

---

## Task 2 — Add `sinh`, `cosh`, `tanh` to the function parser

`sinh`, `cosh`, `tanh` are not in `fnName` or `tupleToFunctionExpression`.

**Change 1:** extend `fnName`:
```scala
val fnName: Parser[String] =
  "sinh" | "cosh" | "tanh" | "sin" | "cos" | "tan" | "ln" | "exp" | "rec" | "neg"
```
Note: longer names (`sinh`, `cosh`, `tanh`) must come before their prefixes
(`sin`, `cos`, `tan`) to avoid prefix matching.

**Change 2:** extend `tupleToFunctionExpression`:
```scala
case "sinh" ~ arg => UniFunction(arg, Sinh)
case "cosh" ~ arg => UniFunction(arg, Cosh)
case "tanh" ~ arg =>
  BiFunction(
    UniFunction(arg, Sinh),
    UniFunction(UniFunction(arg, Cosh), Reciprocal),
    Product
  )
```

---

## Task 3 — Cartesian complex form `a + bi`

This is **already handled** by the existing `expr` parser once Task 1 is done.
`1 + 2i` will parse as:
```
BiFunction(Literal(1), BiFunction(Literal(2), I, Product), Sum)
```
which is exactly the tree that `EulerSumCommutative` / the recognition rules expect.

No parser changes needed beyond Task 1.

---

## Task 4 — Polar form `r·e^(iθ)`

This is **already handled** by the existing `implicitMul` + `power` + `function`
chain once Task 1 is done. For example `2e^{i\pi}` parses as:
```
BiFunction(Literal(2), BiFunction(E, BiFunction(I, Pi, Product), Power), Product)
```
The simplification pipeline then converts this via:
- `(E, x) => UniFunction(x, Exp)` in `matchBiFunctionIdentitiesPower`
- `EulerProductCommutative` / `BiFunction(r, Euler(One, θ), Product)` rules

No parser changes needed beyond Task 1.

---

## Task 5 — Verify `e^{i\theta}` LaTeX syntax

Confirm that `\e^{i\theta}` and `e^{i\theta}` both parse correctly end-to-end
via the `puremath` string interpolator. Add parser tests covering:

| Input | Expected expression tree |
|---|---|
| `"i"` | `I` |
| `"1 + 2i"` | `BiFunction(1, BiFunction(2, I, Product), Sum)` |
| `"e^{i\pi}"` | simplifies to `MinusOne` |
| `"2e^{i\pi/2}"` | simplifies to `Literal(2.i)` |
| `"\sinh{x}"` | `UniFunction(x, Sinh)` |
| `"\cosh{x}"` | `UniFunction(x, Cosh)` |
| `"\tanh{x}"` | `BiFunction(UniFunction(x,Sinh), UniFunction(UniFunction(x,Cosh),Reciprocal), Product)` |

---

## Summary

| Task | Effort | Status |
|---|---|---|
| Task 1: `i` as math symbol | Trivial | ⬜ |
| Task 2: `sinh`/`cosh`/`tanh` | Small | ⬜ |
| Task 3: Cartesian `a + bi` | None (falls out of Task 1) | ⬜ |
| Task 4: Polar `r·e^(iθ)` | None (falls out of Task 1) | ⬜ |
| Task 5: Parser tests | Small | ⬜ |