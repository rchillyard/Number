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
### Phase 3: simplifyIdentities
[To be documented]

### Phase 4: simplifyExact
[To be documented]

### Phase 5: simplifyConstant
[To be documented]

## Design Principles
[When to call .simplify, pattern organization, etc.]