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

### Phase 2: simplifyStructural
[To be documented]

### Phase 3: simplifyIdentities
[To be documented]

### Phase 4: simplifyExact
[To be documented]

### Phase 5: simplifyConstant
[To be documented]

## Design Principles
[When to call .simplify, pattern organization, etc.]