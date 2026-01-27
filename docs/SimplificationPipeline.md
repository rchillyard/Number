# Expression Simplification Pipeline

## Overview
[High-level description of the multi-phase approach]

## Pipeline Phases

### Phase 1: simplifyOperands

Purpose: Recursively simplify all operands/components of composite expressions
before applying structural or identity transformations.

Applies to: BiFunction, UniFunction, Aggregate (all CompositeExpression types)

Mechanism:
1. Extract operands from the composite expression
2. Call .simplify() recursively on each operand
3. Reconstruct expression with simplified operands
4. Optimization: Return Miss if no operands changed (avoid unnecessary reconstruction)

Key principle: Bottom-up simplification
- Inner/nested expressions are fully simplified first
- Outer expressions operate on already-simplified components
- Ensures consistency: nested expressions get same treatment as top-level

Example: (√3 + 1) * (√3 - 1)
- First simplifies (√3 + 1) and (√3 - 1) independently
- Then outer * operation can work with simplified operands

### Phase 2: simplifyStructural (via `structuralMatcher`)

**Purpose:** Apply algebraic laws that transform expressions between different function types

**Applies to:** BiFunction, UniFunction, Aggregate (all CompositeExpression types)

**Core Principle:**
Structural simplifications must satisfy two criteria:
1. **Depend on the function type** (Sum, Product, Power) - the transformation is based on algebraic laws specific to that operation
2. **Transform to a different function type** - the result uses a different operation than the input
3. **Value-independent** - work for ANY expression `a`, `b`, etc., not specific values like 0, 1, E, π

**Key Rule:** If a simplification depends on specific values (identity elements, special constants) or doesn't change function types, it belongs in `simplifyIdentities` instead.

**Examples of Structural Transformations:**

| Pattern | Input Function | Output Function | Algebraic Law |
|---------|---------------|-----------------|---------------|
| `a + a → 2*a` | Sum | Product | Combining like terms |
| `a * a → a²` | Product | Power | Power notation |
| `x^a * x^b → x^(a+b)` | Product | Power | Exponent addition law |
| `(ab)^p → a^p * b^p` | Power | Product | Distributive power |
| `(a^b)^p → a^(bp)` | Power | Power (different structure) | Power of power law |
| `a * (-a) → -(a²)` | Product | UniFunction(Power) | Negation factoring |

**Non-Structural (belong in identities):**
- `_^0 → 1` - depends on specific value (0)
- `a * 1 → a` - depends on identity element (1)
- `e^x → NatLog(x)` - depends on specific base (e)
- `φ^n → (φ+1)^(n-1)` - depends on specific value (φ) and its mathematical properties

**Implementation Details:**

Each CompositeExpression type implements `structuralMatcher` differently:

**BiFunction patterns:**
- Combining like terms in sums and products
- Power law transformations
- Distributive properties
- Special handling for angles with Radian factors
- Catch-all: converts to Aggregate for term collection

**Aggregate patterns:**
- Complementary term elimination
- Literal combination
- Aggregate flattening

**Mechanism:**
The matcher uses pattern matching to identify structural transformation opportunities. When a pattern matches:
1. Creates new expression with transformed function type
2. Returns `Match(newExpression)`
3. If no pattern matches, returns `Miss` (not an error - just means no structural simplification applies)

**Order in Pipeline:**
Structural simplification runs AFTER operands are simplified but BEFORE identity simplifications. This ensures:
- Nested expressions are already in canonical form
- Structural transformations can combine terms before identity rules apply
- Maximum opportunity for algebraic simplification

**Example Simplification Flow:**

Input: `π^2 * π^3`
1. Phase 1 (operands): Both operands already simplified
2. **Phase 2 (structural):** Matches `x^a * x^b` pattern → `π^5`
3. Phase 3 (identities): No identity patterns apply
4. Result: `π^5`

Input: `(√2)^3 * (√2)^2`
1. Phase 1 (operands): Both operands already simplified
2. **Phase 2 (structural):** Matches power combination → `(√2)^5`
3. Phase 3 (identities): No identity patterns apply
4. Result: `(√2)^5`

### Phase 3: simplifyIdentities (via `identitiesMatcher`)

**Purpose:** Apply mathematical identities, handle special values, and simplify expressions involving identity elements

**Applies to:** BiFunction, UniFunction, Aggregate (all CompositeExpression types)

**Core Principle:**
Identity simplifications handle cases that depend on **specific values** rather than general algebraic structure. Unlike structural simplifications, these patterns recognize special constants, identity elements, and mathematical properties unique to particular expressions.

**Key Characteristics:**
- **Value-dependent** - simplifications depend on specific values (0, 1, -1, E, π, ∞, φ, angles)
- **Identity elements** - handle additive identity (0), multiplicative identity (1)
- **Inverse operations** - handle negation, reciprocals, conjugates
- **Special functions** - exponential, logarithm, trigonometric identities
- **May or may not change function type** - often reduces to simpler/atomic forms

**Dispatch Architecture:**

`identitiesMatcher` first checks for identity elements, then dispatches to function-specific sub-methods:
```
identitiesMatcher
├── Check left identity element  → simplify to right operand
├── Check right identity element → simplify to left operand
└── Dispatch by function type:
    ├── Sum     → matchBiFunctionIdentitiesSum
    ├── Product → matchBiFunctionIdentitiesProduct
    ├── Power   → matchBiFunctionIdentitiesPower
    ├── Log     → matchBiFunctionIdentitiesLog
    └── Atan    → matchBiFunctionIdentitiesAtan
```

**Examples by Category:**

**Identity Elements:**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `a + 0 → a` | `π + 0` | `π` | Additive identity |
| `a * 1 → a` | `√2 * 1` | `√2` | Multiplicative identity |
| `a * 0 → 0` | `e * 0` | `0` | Absorbing element |

**Inverse Operations:**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `a + (-a) → 0` | `√3 + (-√3)` | `0` | Additive inverse |
| `a * (1/a) → 1` | `π * (1/π)` | `1` | Multiplicative inverse |
| `a * (-1) → -a` | `e * (-1)` | `-e` | Negation |

**Special Exponents:**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `a^0 → 1` | `π^0` | `1` | Any number to zero power |
| `a^1 → a` | `√2^1` | `√2` | Identity exponent |
| `a^(-1) → 1/a` | `e^(-1)` | `1/e` | Negative exponent |
| `a^∞ → ∞` | `2^∞` | `∞` | Infinite exponent |

**Exponential and Logarithm:**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `e^x → NatLog(x)` | `e^5` | `NatLog(5)` | Natural exponential |
| `log_b(b) → 1` | `log_e(e)` | `1` | Logarithm identity |
| `log_b(1) → 0` | `log_π(1)` | `0` | Log of 1 |
| `b^(log_b(x)) → x` | `e^(log_e(π))` | `π` | Inverse functions |

**Trigonometric (Atan):**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `atan(x>0, 0) → 0` | `atan(5, 0)` | `0` | Angle on positive x-axis |
| `atan(x<0, 0) → π` | `atan(-3, 0)` | `π` | Angle on negative x-axis |
| `atan(1, 1) → π/4` | `atan(1, 1)` | `π/4` | 45-degree angle |
| `atan(0, 1) → π/2` | `atan(0, 1)` | `π/2` | 90-degree angle |

**Quadratic Roots (Golden Ratio, etc.):**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `φ + φ̄ → 1` | Conjugate sum | `1` | Sum of conjugates = -b/a |
| `φ * φ̄ → -1` | Conjugate product | `-1` | Product of conjugates = c/a |
| `φ^n` | `φ^3` | `φ(φ+1)` | Golden ratio identity: φ² = φ+1 |
| `2^(1/2) → √2` | `2^0.5` | `√2` | Fractional power to root |

**Euler's Identity:**
| Pattern | Example | Result | Reason |
|---------|---------|--------|--------|
| `e^(iπ)` | Complex exponential | `-1` | Euler's formula |
| `e^(π*i) → -1` | Various forms | `-1` | Euler's identity |

**Implementation Strategy:**

Each function-specific matcher handles patterns relevant to that operation:

**matchBiFunctionIdentitiesSum:**
- Additive identity (0)
- Combining equal terms: `a + a → 2a` [NOTE: This is now in structural!]
- Additive inverses: `a + (-a) → 0`
- Quadratic root conjugates (sum)

**matchBiFunctionIdentitiesProduct:**
- Multiplicative identity (1)
- Absorbing element (0)
- Negation: `a * (-1) → -a`
- Combining equal terms: `a * a → a²` [NOTE: This is now in structural!]
- Quadratic root conjugates (product)
- Inexact value combination (fuzzy arithmetic)

**matchBiFunctionIdentitiesPower:**
- Special exponents (0, 1, -1, ∞)
- Exponential notation: `e^x → NatLog(x)`
- Inverse of logarithm: `b^(log_b(x)) → x`
- Golden ratio power reduction: `φ^n → ...`
- Root notation: `2^(1/2) → √2`
- Euler's identity patterns

**matchBiFunctionIdentitiesLog:**
- Log of base: `log_b(b) → 1`
- Log of 1: `log_b(1) → 0`
- Natural log: `log_e(x) → ln(x)`

**matchBiFunctionIdentitiesAtan:**
- Special angles (0°, 45°, 90°, 180°)
- Axis cases (positive/negative x-axis, y-axis)

**Order in Pipeline:**

Identity simplification runs AFTER structural simplification. This ordering ensures:
1. Algebraic structure is already optimized (terms combined, powers simplified)
2. Identity patterns work on already-structurally-simplified expressions
3. Special values and constants are handled after general transformations

**Example Simplification Flow:**

Input: `e^0`
1. Phase 1 (operands): `e` and `0` already atomic
2. Phase 2 (structural): No structural pattern applies (not changing function types)
3. **Phase 3 (identities):** Matches `_^0 → 1` in `matchBiFunctionIdentitiesPower`
4. Result: `1`

Input: `√3 + (-√3)`
1. Phase 1 (operands): Both operands already simplified
2. Phase 2 (structural): No structural pattern applies
3. **Phase 3 (identities):** Matches `a + (-a) → 0` in `matchBiFunctionIdentitiesSum`
4. Result: `0`

Input: `φ * φ̄` (conjugate golden ratios)
1. Phase 1 (operands): Both roots already atomic
2. Phase 2 (structural): No structural pattern applies
3. **Phase 3 (identities):** Matches conjugate product pattern in `matchBiFunctionIdentitiesProduct`
4. Result: `-1`

**Design Notes:**

- **Aggregate considerations:** Identity simplifications apply to individual BiFunction expressions before they're converted to Aggregates. Once in an Aggregate, term combination happens through `complementaryTermsEliminator` and `literalsCombiner`.

- **Match vs. Miss:** A `Miss` result doesn't indicate failure - it simply means no identity pattern applies. The expression continues through the pipeline.

- **Helper methods:** Delegate to specialized methods like `matchRoot`, `matchLiteral` for complex pattern matching.

- **TESTME patterns:** Some defensive patterns remain marked `TESTME` for edge cases that rarely occur but are kept for robustness.

### Phase 4: simplifyExact

**Purpose:** Evaluate expressions that can be computed exactly without introducing approximations

**Applies to:** All CompositeExpression types

**Core Principle:**
Attempt to evaluate the expression as an exact value. If successful and the result is confirmed exact (no fuzziness), replace the entire expression with a `ValueExpression` containing that exact value.

**Key Characteristics:**
- **Exactness-preserving** - only simplifies if result is provably exact
- **All-or-nothing** - evaluates the entire expression, not individual parts
- **Double-check mechanism** - verifies result is actually exact before accepting
- **Falls through** - returns `Miss` if evaluation fails or introduces approximation

**Implementation:**
```scala
lazy val simplifyExact: em.AutoMatcher[Expression] =
  em.Matcher("BiFunction: simplifyExact") {
    (expr: Expression) =>
      expr.evaluateAsIs match {
        case Some(value) =>
          em.Match(ValueExpression(value)).filter(_.isExact) // Verify exactness
        case None =>
          em.Miss[Expression, Expression]("BiFunction: simplifyExact: no simplifications", this)
      }
  }
```

**When It Applies:**

This phase catches expressions that:
- Have been fully simplified structurally and via identities
- Can be evaluated to an exact numeric result
- Involve only exact operations on exact values

**Examples:**

| Input | Can Evaluate Exactly? | Result | Reason |
|-------|----------------------|--------|--------|
| `2 + 3` | Yes | `5` | Integer arithmetic is exact |
| `1/2 + 1/3` | Yes | `5/6` | Rational arithmetic is exact |
| `√4` | Yes | `2` | Perfect square |
| `π + e` | No | `π + e` | Transcendentals can't be exactly combined |
| `√2 + √3` | No | `√2 + √3` | Irrational sum not expressible exactly |
| `2.5 * 4` | Yes | `10.0` | If values are exact decimals |

**Order in Pipeline:**

Phase 4 runs AFTER all symbolic simplifications (operands, structural, identities). This ensures:
1. Maximum symbolic simplification has occurred
2. The expression is in canonical form
3. Only truly irreducible exact expressions are evaluated

**Example Flow:**

Input: `√4 + 1`
1. Phase 1 (operands): `√4` stays symbolic, `1` atomic
2. Phase 2 (structural): No pattern applies
3. Phase 3 (identities): No pattern applies
4. **Phase 4 (exact):** `√4` evaluates to exact `2`, sum evaluates to exact `3`
5. Result: `3` (as `ValueExpression`)

Input: `√2 + √2`
1. Phase 1 (operands): Both stay symbolic
2. Phase 2 (structural): Matches `a + a → 2a`, becomes `2 * √2`
3. Phase 3 (identities): No pattern applies
4. **Phase 4 (exact):** Cannot evaluate exactly (irrational)
5. Continues to Phase 5...

---

### Phase 5: simplifyConstant

**Purpose:** Final fallback - attempt to evaluate any expression to a constant value, potentially introducing approximation

**Applies to:** All CompositeExpression types

**Core Principle:**
If all previous phases fail to simplify an expression, try to evaluate it as a constant. Unlike Phase 4, this phase accepts fuzzy/approximate results. If evaluation succeeds, recursively simplify the result to ensure it's in canonical form.

**Key Characteristics:**
- **Last resort** - only runs if all other simplifications fail
- **Accepts approximation** - will produce fuzzy values if needed
- **Recursive simplification** - calls `.simplify` on the result
- **Graceful failure** - returns `Miss` if even approximate evaluation fails

**Implementation:**
```scala
lazy val simplifyConstant: em.AutoMatcher[Expression] = 
  em.Matcher[Expression, Expression]("simplifyConstant") {
    expr =>
      expr.evaluateAsIs match {
        case Some(f) =>
          em.MatchCheck(Expression(f))(expr).map(_.simplify) // Recursive simplify!
        case _ =>
          em.Miss("matchSimpler: cannot be simplified", expr)
      }
  }
```

**When It Applies:**

This phase is the final attempt to reduce an expression:
- All symbolic simplifications exhausted
- Expression cannot be evaluated exactly (Phase 4 failed)
- But can be approximated to a numeric value

**Examples:**

| Input | Previous Phases | Phase 5 Result | Notes |
|-------|----------------|----------------|-------|
| `√2` | No simplification | `Real(1.414..., fuzz)` | Approximates to fuzzy value |
| `π + e` | No simplification | `Real(5.859..., fuzz)` | Sum of transcendentals |
| `sin(π/4)` | Identity might handle | `Real(0.707..., fuzz)` | Trigonometric evaluation |
| `2 + 3` | Phase 4 handled | N/A | Already exact, doesn't reach here |
| `ln(unknown)` | No evaluation possible | `ln(unknown)` | Can't evaluate, stays symbolic |

**Recursive Simplification:**

Note the `.map(_.simplify)` in the implementation. This ensures:
- Evaluated constants go through the full pipeline again
- Catches any patterns that apply to the numeric result
- Example: Evaluating to `1.0` might then match identity patterns

**Order in Pipeline:**

Phase 5 is the FINAL phase. At this point:
1. All symbolic transformations attempted (Phases 1-3)
2. Exact evaluation attempted (Phase 4)
3. Now accepting approximate evaluation as last resort
4. If this fails, expression remains in symbolic form

**Complete Example Flow:**

Input: `√2 * √2`
1. Phase 1 (operands): Both `√2` stay symbolic
2. **Phase 2 (structural):** Matches `a * a → a²`, becomes `(√2)²`
3. **Phase 3 (identities):** No pattern for `(√2)²`
4. **Phase 4 (exact):** Evaluates `(√2)² = 2` exactly ✓
5. Result: `2` (doesn't reach Phase 5)

Input: `√2 + π`
1. Phase 1 (operands): Both stay symbolic
2. Phase 2 (structural): No pattern applies
3. Phase 3 (identities): No pattern applies
4. Phase 4 (exact): Cannot evaluate exactly (sum of irrationals)
5. **Phase 5 (constant):** Evaluates approximately to `Real(4.556..., fuzz)`
6. Result: Fuzzy approximation

Input: `x + y` (symbolic variables)
1. Phases 1-4: No simplification possible
2. **Phase 5 (constant):** Cannot evaluate (no values for x, y)
3. Result: `x + y` (stays symbolic)

**Design Philosophy:**

The five-phase pipeline follows a "maximum symbolic, minimum numeric" philosophy:
1. Try structural algebra first (symbolic, exact, reversible)
2. Try identity simplifications (symbolic, exact, specific values)
3. Try exact evaluation (numeric, exact, preserves precision)
4. Only as last resort, approximate (numeric, fuzzy, information loss)

This ensures:
- Mathematical precision preserved where possible
- Symbolic forms preferred over numeric approximations
- Users can see "π" rather than "3.14159..." unless explicitly evaluated
- Exact arithmetic (rationals, integers) never degraded to floating-point

---

## Summary: Complete Simplification Pipeline
```
Expression.simplify
    ↓
Phase 1: simplifyOperands (via operandsMatcher)
    │   └─→ Recursively simplify all nested operands
    │       (Bottom-up: inner expressions first)
    ↓
Phase 2: simplifyStructural (via structuralMatcher)
    │   └─→ Apply algebraic laws (function type transformations)
    │       Examples: a+a→2a, x^a*x^b→x^(a+b)
    ↓
Phase 3: simplifyIdentities (via identitiesMatcher)
    │   └─→ Handle special values, identity elements
    │       Examples: a*0→0, e^x→NatLog(x), φ*φ̄→-1
    ↓
Phase 4: simplifyExact
    │   └─→ Exact evaluation (no approximation)
    │       Only if result is provably exact
    ↓
Phase 5: simplifyConstant
    │   └─→ Approximate evaluation (last resort)
    │       Accepts fuzzy values if necessary
    ↓
Simplified Expression (or original if no simplification possible)
```

**Key Principles:**

1. **Symbolic before numeric** - preserve mathematical form as long as possible
2. **Exact before approximate** - never lose precision unnecessarily
3. **General before specific** - structural laws before special cases
4. **Bottom-up** - simplify components before combinations
5. **Recursive** - results may trigger further simplification
6. **Graceful** - `Miss` is not failure, just "no pattern applies"
