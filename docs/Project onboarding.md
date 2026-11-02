# Number Library — Project Onboarding Document

This document is intended to bring a new Claude instance up to speed on the **Number** project so that it can act as an effective design partner, code reviewer, and document drafter.

---

## 1. Project Overview

**Number** is a Scala 3 mathematical library (`com.phasmidsoftware.number`) developed by Robin. It handles:

- Exact and fuzzy (uncertainty-bearing) arithmetic
- Symbolic expression simplification
- Uncertainty propagation through algebraic operations
- Dimensional analysis and physical constants
- Complex number support

The library is targeting a **2.0.0 milestone release** and a potential **Typelevel affiliate submission**.

Repository location: `IdeaProjects/Number`  
Published API docs: https://rchillyard.github.io/Number/api/

---

## 2. Build & Toolchain

| Tool | Details |
|------|---------|
| Build | sbt |
| IDE | IntelliJ IDEA (with sbt shell) |
| CI | CircleCI |
| Publishing | Maven Central via `sonatypeCentralUpload` |
| VCS UI | GitKraken |
| Scala version | 3.7.3 |

Key dependencies: **Cats**, **ScalaUnidocPlugin**, **ScalaTest**, **FastParse** (migrated to Scala Parser Combinators), **tasty-query 1.6.1**, **Flog** (Robin's own functional logging library).

---

## 3. Module Structure

The project uses sbt submodules:

- `core` — older, foundational numeric types
- `algebra` — newer algebraic operations
- `expression` — symbolic expression tree
- `parse` — parser combinators
- `dimensions` — dimensional analysis
- `top` — top-level integration
- `mermaid/` — automated class diagram generation (TASTy-based, forked as subprocess to avoid Scala 2.12/3 runtime mismatch)

---

## 4. Current State

**Test suite:** 3,866 passing, 10 amber/ignored.

- Amber tests are largely under **Issue #203** (`getQualifiedString` rewrite and mantissa rounding in `AbsoluteFuzz`). A truth table exists but implementation was shelved after cascading failures.
- **Issue #204** (fuzz propagation through `Functional`/`Transformed` algebra types) — **fixed and committed**.

---

## 5. Key Domain Concepts

### Fuzz (Uncertainty) Types

- `AbsoluteFuzz` — absolute uncertainty (e.g. ±0.5)
- `RelativeFuzz` — relative/percentage uncertainty (e.g. ±0.1%)
- Shapes: `Box`, `Gaussian`, `Trapezoid(a, b)` (Box⊗Box convolution)
- **Confidence semantics:** higher confidence = *narrower* wiggle = harder to satisfy `isProbablyZero`. Default confidence is `oneSigma = 0.317`.

### Fuzz Propagation Rules (Critical)

- `fuzzFunction` **must** be evaluated at the *input* value, not the output value.
- `AbsoluteFuzz` uses the **absolute derivative**.
- `RelativeFuzz` uses the **relative derivative**: `derivative * x / f(x)`.
- Monadic operations require `monadicCombine` (scales by derivative only) — **not** the dyadic `combine`, which incorrectly treats correlated operands as independent.

### Trapezoid Shape (`WI13` — complete)

`Trapezoid(a, b)` represents a Box⊗Box convolution with `a < b`:

- Flat-top wiggle (`confidence > a/b`): `b - a`
- Ramp wiggle: `(a+b) - 2√(ab·confidence)`
- Variance: `σ = √((a²+b²)/3)`

### Other Key Constants / Thresholds

- `negligibleRatio = 10`: smaller fuzz contributes less than 0.5% to quadrature combination.
- `doublePrecisionFloor ≈ 4.4E-16`: three-tier `applyRules` with Rule 2 (negligible ratio) applied before Rule 1 (floor scaling).
- `sanitize`: strips NaN/infinite fuzz magnitudes; forces tolerances positive via `math.abs`.

---

## 6. Expression Simplification Pipeline

Five-phase pipeline (order is significant):

1. `simplifyOperands`
2. `simplifyStructural`
3. `simplifyIdentities`
4. `simplifyExpand`
5. `simplifyByEvaluation`

Key guards:

- **`shouldStaySymbolic`**: requires ANY term to be symbolic (not ALL) to protect `Pi`/`E` from premature numeric evaluation.
- **`operandsMatcher` guards**: prevent eager evaluation of trig arguments containing `I` before structural rules fire.
- **Structural vs. identity simplifications**: structural rules depend on function type and transform to different types; identity rules handle specific values (0, 1, E, π, φ). These must remain in separate phases.
- **`normalize`** returns `Eager`, not the original subtype, because simplification may produce a fundamentally different type.

---

## 7. Architecture Notes

- **`Matchers` framework**: structurally identical to parser combinators with parametric input type; simplification rules are analogous to grammar productions.
- **`eitherOr` fix**: original `m1 & m2 | m1 | m2` evaluated `m1` up to three times; corrected so each matcher is evaluated at most once.
- **TASTy diagram generation**: tasty-query API requires `javap` inspection to determine correct method signatures; `parentClasses` requires JDK `java.base` on the classpath.
- **`normalization`** can change types — this is intentional and expected.

---

## 8. Deferred Work (Tracked Issues)

The following are known deferred items, in approximate priority order:

1. **Series Option B** — accumulate as `Double`
2. **`evaluateToTolerance` dedup** — merge duplicate code
3. **Trapezoid eccentricity refactor** — store only `a/b` as `Rational` for stateless consistency
4. **Issue #196** — Box/Gaussian fuzz combination too strict in subtraction
5. **Issue #197** — asymmetric `Complex + Real` addition
6. **Issue #199** — `fuzzyCompare` asymmetry
7. **`tan`/`tanh` as first-class functions** (currently represented as Sin/Cos division)
8. **Symbolic hyperbolic identity** (`cosh²−sinh²=1` for concrete numeric args) — architecturally blocked
9. **`LaTeXParserSpec` pending test**
10. **Typelevel submission prerequisites**
11. **`asComparedWith`/`probabilityOfZero` merger**
12. **Issue #203** — context-sensitive simplification / `getQualifiedString` rewrite

---

## 9. Coding Conventions & Preferences

Robin implements changes himself after design is agreed; Claude produces code for complex or document-heavy tasks.

| Convention | Detail |
|------------|--------|
| Spelling | British (e.g. `kluge` not `kludge`) |
| Power operator | `∧` |
| Equality | `===` (Cats `Eq`) over `==` |
| Implicits | `given`/`summon` over `implicit`/`implicitly` |
| Comments | `FIXME` / `TESTME` / `TODO` / `CONSIDER` / `NOTE` / `XXX` |
| Tests | ScalaTest `TableDrivenPropertyChecks` preferred |
| Symbolic equality | Always compare `.simplify` forms |
| Pending tests | Mark with issue reference rather than weakening assertions |
| Extractors | `unapply` preferred over private helpers |
| Operators | `ExpressionOps` operators where possible |
| Commits | Tight, single-concern |
| Design | Document before implementation |

**Debugger note:** `debug` method is the configured custom renderer to avoid Heisenbugs from `toString`/`show` triggering lazy evaluation.

**Decimal arithmetic note:** Decimal digit manipulation with floating point is hard. Robin prefers `BigDecimal` for exact decimal arithmetic and first-principles reasoning when debugging formatting logic.

---

## 10. Working Style

- Robin uses Claude primarily as a **design partner, code reviewer, and document drafter** — a role he has described as "Boswell."
- Iterative workflow: run full test suite between changes, share `println`/debugger output to trace values, upload source files when broader analysis is needed.
- Commits are made by Robin; Claude drafts commit messages when asked.
- Prefers one tight commit per logical change.
- Robin teaches university courses (Big Data with Scala; Program Structure and Algorithms) and enjoys bridge and following Fulham FC.

---

## 11. Where to Start

When beginning a new session:

1. Ask Robin what he'd like to work on, or refer to the **Deferred Work** list in §8.
2. If given a source file, read it carefully before suggesting changes — the codebase has many subtle invariants.
3. For any design question, write up a short analysis and ask Robin to confirm before producing code.
4. Run tests mentally against the **Fuzz Propagation Rules** in §5 before declaring a fix correct.