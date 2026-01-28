# Testing Conventions - Number Library

This document describes the testing organization, conventions, and standards for the Number library test suite.

## Test Suite Organization

The Number library has **~3,500 tests** across multiple test files, organized by responsibility and scope.

### Expression Tests - File Structure

```
expression/expr/
├── AtomicExpressionSpec.scala       (773 lines)  - Atomic expression tests
├── ExpressionSpec.scala             (541 lines)  - Integration & behavior tests  
├── ExpressionMatchersSpec.scala     (1,615 lines) - Matcher & simplification tests
├── ExpressionEdgeCasesSpec.scala    (254 lines)  - Edge cases & TESTME coverage
└── CompositeExpressionSpec.scala    (776 lines)  - Composite expression tests
```

### File Responsibilities

#### AtomicExpressionSpec.scala
**Purpose:** Single source of truth for all atomic expression testing

**Tests:**
- Individual atomic constants (One, Two, E, Pi, MinusOne, etc.)
- Atomic properties (isAtomic, depth, exactness, signum)
- Atomic behaviors (evaluate, materialize, simplify, render)
- Special types: Noop, Literal, Root (QuadraticRoot, LinearRoot)

**Pattern per constant:**
```scala
behavior of "One"

it should "be atomic" in { ... }
it should "be exact" in { ... }
it should "have depth 1" in { ... }
it should "have signum 1" in { ... }
it should "evaluate to Eager.one" in { ... }
it should "materialize to Eager.one" in { ... }
it should "render as '1'" in { ... }
it should "support monadic functions" in { ... }
```

**When to use:**
- Adding a new atomic constant (e.g., new transcendental number)
- Testing atomic expression properties
- Testing basic atomic operations

#### ExpressionSpec.scala
**Purpose:** High-level integration and behavioral tests

**Tests:**
- Expression operations (addition, multiplication, etc.)
- Integration scenarios involving multiple expression types
- Parse → simplify → evaluate pipelines
- Expression equality and comparison
- Complex simplification scenarios

**Pattern:**
```scala
behavior of "simplify"

it should "simplify complex nested expressions" in {
  val expr = (Pi + E) * (Pi - E)
  val result = expr.simplify
  // Test high-level behavior
}
```

**When to use:**
- Testing how different expression types work together
- Testing end-to-end workflows
- Testing complex multi-step simplifications
- Integration scenarios

**What NOT to test here:**
- Individual atomic constants (→ AtomicExpressionSpec)
- Detailed matcher behavior (→ ExpressionMatchersSpec)
- Edge cases (→ ExpressionEdgeCasesSpec)

#### ExpressionMatchersSpec.scala
**Purpose:** Detailed matcher and pattern matching tests

**Tests:**
- Expression matchers (matchSimpler, matchValue, etc.)
- Simplification pattern matching
- Structural vs identity simplifications
- Matcher combinators
- BiFunction/UniFunction simplification patterns

**Pattern:**
```scala
behavior of "matchSimpler"

it should "match power combination: x^a * x^b → x^(a+b)" in {
  val expr = BiFunction(BiFunction(E, 2, Power), BiFunction(E, 3, Power), Product)
  val result = Expression.matchSimpler(expr)
  result should matchPattern { case em.Match(_) => }
}
```

**When to use:**
- Testing specific simplification patterns
- Testing matcher logic and combinators
- Testing pattern matching edge cases
- Verifying structural/identity simplification phases

#### ExpressionEdgeCasesSpec.scala
**Purpose:** Tests for previously untested code paths and edge cases

**Tests:**
- TESTME markers that were found in code coverage analysis
- Boundary conditions
- Defensive code patterns
- Unusual combinations

**Pattern:**
```scala
behavior of "BiFunction structural simplifications"

it should "simplify power of power: (a^b)^p → a^(bp)" in {
  // Line 561 in CompositeExpression.scala
  val inner = BiFunction(Pi, Expression(3), Power)
  val outer = BiFunction(inner, Expression(4), Power)
  val result = outer.simplify
  result shouldBe BiFunction(Pi, Expression(12), Power)
}
```

**When to use:**
- Code coverage identified untested patterns
- Found an edge case bug
- Testing defensive code
- Documenting unusual behavior

#### CompositeExpressionSpec.scala
**Purpose:** Tests for composite expressions (UniFunction, BiFunction, Aggregate)

**Tests:**
- UniFunction properties and operations
- BiFunction properties and operations
- Aggregate properties and operations
- Composite expression evaluation

**Status:** Historical file, partially overlaps with other specs. Future consolidation planned.

**When to use:** Currently, prefer adding tests to more specific files above.

## Where to Add New Tests - Quick Reference

| Test Type | File | Example |
|-----------|------|---------|
| New atomic constant | AtomicExpressionSpec | Testing golden ratio constant |
| Atomic property | AtomicExpressionSpec | Testing isAtomic, depth, exactness |
| Expression operation | ExpressionSpec | Testing (a + b) * (a - b) simplification |
| Integration scenario | ExpressionSpec | Testing parse → simplify → evaluate |
| Simplification pattern | ExpressionMatchersSpec | Testing x^a * x^b → x^(a+b) |
| Matcher behavior | ExpressionMatchersSpec | Testing complementaryTermsEliminator |
| Edge case | ExpressionEdgeCasesSpec | Testing previously untested code path |
| Boundary condition | ExpressionEdgeCasesSpec | Testing NaN, infinity, zero handling |

## Test Naming Conventions

### Behavior Blocks

Use descriptive `behavior of` blocks that clearly indicate what is being tested:

**Good:**
```scala
behavior of "UniFunction - Simplification"
behavior of "BiFunction - Power operations"
behavior of "Aggregate - Angle handling"
```

**Avoid:**
```scala
behavior of "tests"
behavior of "misc"
behavior of "various operations"  // Too vague
```

### Test Names

Test names should clearly state:
1. What is being tested
2. The expected behavior or result

**Good:**
```scala
it should "simplify sqrt(2) * sqrt(2) to 2" in { ... }
it should "evaluate cos(π) to -1" in { ... }
it should "preserve exactness through addition" in { ... }
it should "simplify power of power: (a^b)^c → a^(bc)" in { ... }
```

**Avoid:**
```scala
it should "work correctly" in { ... }  // Too vague
it should "test simplification" in { ... }  // Not specific
it should "be ok" in { ... }  // Meaningless
it should "pass" in { ... }  // Says nothing about behavior
```

### Special Cases

For edge cases or bug fixes, reference the issue or line number:

```scala
it should "handle phi power reduction for whole number exponents (Issue #123)" in { ... }
it should "simplify complementary terms with NaN sort keys (Line 503)" in { ... }
```

## Test Organization Patterns

### Group Related Tests

Group tests under clear behavior blocks:

```scala
behavior of "One"
// All One tests together

behavior of "Two"  
// All Two tests together

behavior of "depth"
// All depth tests together (for composite expressions only)
```

### Avoid Mixing Concerns

Don't mix atomic and composite tests in the same file:

**Wrong:**
```scala
// In ExpressionSpec.scala
it should "One be exact" in { ... }  // Atomic test
it should "simplify (a + b) * c" in { ... }  // Composite test
```

**Right:**
```scala
// In AtomicExpressionSpec.scala
it should "One be exact" in { ... }

// In ExpressionSpec.scala
it should "simplify (a + b) * c" in { ... }
```

### Use Consistent Patterns

For similar types, use consistent test patterns:

```scala
// Pattern for all atomic constants
behavior of "<Constant>"
it should "be atomic" in { ... }
it should "be exact" in { ... }
it should "have depth 1" in { ... }
// ... etc
```

## Table-Driven Tests (Optional)

For highly repetitive tests, consider table-driven approaches using ScalaTest's `TableDrivenPropertyChecks`:

### Example: Atomic Constants

**Current approach (explicit):**
```scala
behavior of "One"
it should "be atomic" in { One.isAtomic shouldBe true }
it should "be exact" in { One.isExact shouldBe true }
it should "render as '1'" in { One.render shouldBe "1" }

behavior of "Two"
it should "be atomic" in { Two.isAtomic shouldBe true }
it should "be exact" in { Two.isExact shouldBe true }
it should "render as '2'" in { Two.render shouldBe "2" }
// ... repeat for E, Pi, etc.
```

**Table-driven approach (compact):**
```scala
import org.scalatest.prop.TableDrivenPropertyChecks

val atomicConstants = Table(
  ("name", "constant", "expectedRender"),
  ("One", One, "1"),
  ("Two", Two, "2"),
  ("E", E, "e"),
  ("Pi", Pi, "π"),
)

forAll(atomicConstants) { (name, constant, expectedRender) =>
  it should s"$name be atomic" in { constant.isAtomic shouldBe true }
  it should s"$name be exact" in { constant.isExact shouldBe true }
  it should s"$name render as $expectedRender" in { constant.render shouldBe expectedRender }
}
```

**Trade-offs:**

**Pros:**
- Reduces ~200 lines to ~50 lines (75% reduction)
- Easy to add new constants
- Ensures consistent coverage
- Less boilerplate

**Cons:**
- Less readable for individual failures
- All constants share same test logic
- Some loss of test specificity
- Requires PropertyChecks import

**Recommendation:** Use explicit tests by default, consider table-driven for:
- Very repetitive patterns (10+ similar tests)
- When consistency is critical
- When adding new instances frequently

## Testing Anti-Patterns to Avoid

### 1. Debugging Code in Tests

**Never commit:**
```scala
it should "test something" in {
  val result = expr.simplify
  println(s"Result: $result")  // ❌ Remove println
  // Just observe what it becomes  // ❌ Remove exploration comments
}
```

**Instead:**
```scala
it should "simplify sqrt(2)^2 to 2" in {
  val result = BiFunction(Expression(2).sqrt, Expression(2), Power).simplify
  result shouldBe Expression(2)  // ✅ Assert specific behavior
}
```

### 2. Empty Test Stubs

**Don't commit:**
```scala
it should "test rendering" in { }  // ❌ Empty stub
it should "test simplification" in { pending }  // ❌ Without reason
```

**Instead:**
```scala
// Delete the stub, or use pending with explanation:
it should "test complex rendering with LaTeX" in {
  pending  // TODO Issue #123: Waiting for LaTeX renderer implementation
}
```

### 3. Vague Assertions

**Avoid:**
```scala
it should "work correctly" in {
  result should not be null  // ❌ Too vague
  noException should be thrownBy expr.simplify  // ❌ Only tests no crash
}
```

**Instead:**
```scala
it should "simplify double negation to original value" in {
  val expr = UniFunction(UniFunction(Two, Negate), Negate)
  expr.simplify shouldBe Two  // ✅ Specific assertion
}
```

### 4. Testing Multiple Things in One Test

**Avoid:**
```scala
it should "test everything about One" in {
  One.isAtomic shouldBe true  // Testing multiple concerns
  One.isExact shouldBe true
  One.depth shouldBe 1
  One.render shouldBe "1"
  // ... etc
}
```

**Instead:**
```scala
it should "be atomic" in { One.isAtomic shouldBe true }
it should "be exact" in { One.isExact shouldBe true }
it should "have depth 1" in { One.depth shouldBe 1 }
it should "render as '1'" in { One.render shouldBe "1" }
```

One assertion per test makes failures easier to diagnose.

## Test Quality Guidelines

### 1. Tests Should Be Deterministic

Every test run should produce the same result:

```scala
// ✅ Good - deterministic
it should "simplify Pi + Pi to 2*Pi" in {
  (Pi + Pi).simplify shouldBe Expression(2) * Pi
}

// ❌ Bad - time-dependent or random
it should "benchmark simplification" in {
  val start = System.nanoTime()
  expr.simplify
  val duration = System.nanoTime() - start
  duration should be < 1000000  // Flaky - depends on system load
}
```

### 2. Tests Should Be Independent

Tests should not depend on execution order or shared mutable state:

```scala
// ❌ Bad - shared mutable state
var counter = 0
it should "increment counter" in { counter += 1 }
it should "check counter is 1" in { counter shouldBe 1 }  // Order-dependent

// ✅ Good - independent tests
it should "increment from zero" in { 
  val counter = 0
  val result = counter + 1
  result shouldBe 1
}
```

### 3. Tests Should Test One Thing

Each test should focus on a single behavior:

```scala
// ✅ Good - focused tests
it should "simplify exp(ln(x)) to x" in { ... }
it should "simplify ln(exp(x)) to x" in { ... }

// ❌ Bad - testing multiple things
it should "test all logarithm properties" in {
  // exp(ln(x)) = x
  // ln(exp(x)) = x
  // ln(1) = 0
  // ... etc - split these up
}
```

### 4. Use Meaningful Test Data

Avoid trivial test data that might hide bugs:

```scala
// ⚠️ Weak - might miss edge cases
it should "simplify sqrt(1) * sqrt(1)" in {
  val result = (Expression(1).sqrt * Expression(1).sqrt).simplify
  result shouldBe Expression(1)  // 1 is too simple
}

// ✅ Better - non-trivial data
it should "simplify sqrt(n) * sqrt(n) to n" in {
  val result = (Expression(7).sqrt * Expression(7).sqrt).simplify
  result shouldBe Expression(7)
}

// ✅ Best - symbolic to avoid eager evaluation
it should "simplify sqrt(pi) * sqrt(pi) to pi" in {
  val result = (Pi.sqrt * Pi.sqrt).simplify  
  result shouldBe Pi
}
```

## Running Tests

### Run All Tests
```bash
sbt test
```

### Run Specific Test File
```bash
sbt "testOnly *ExpressionSpec"
sbt "testOnly *AtomicExpressionSpec"
```

### Run Specific Test
```bash
sbt "testOnly *ExpressionSpec -- -z \"simplify sqrt\""
```

### Run with Coverage
```bash
sbt clean coverage test coverageReport
```

Coverage reports are generated in `target/scala-3.7.3/scoverage-report/index.html`

## Contributing Guidelines

### Before Adding Tests

1. **Check existing tests** - Avoid duplicates
2. **Choose the right file** - Use the organization guide above
3. **Follow naming conventions** - Clear, descriptive names
4. **Review anti-patterns** - Avoid common mistakes

### Test Checklist

- [ ] Test is in the correct file (see Quick Reference table)
- [ ] Test name clearly describes what is being tested
- [ ] Test uses meaningful, non-trivial data
- [ ] Test has clear assertions (not just "should not crash")
- [ ] Test is independent (no shared mutable state)
- [ ] Test is deterministic (same result every run)
- [ ] No println or debugging code
- [ ] No empty stubs or pending without explanation

### Pull Request Tests

When submitting a PR:

1. **Run full test suite** locally
2. **Ensure new tests pass** consistently
3. **Add tests for new features** or bug fixes
4. **Document** any new test patterns or conventions
5. **Check coverage** for the code you're testing

## Simplification Pipeline Reference

Understanding the simplification pipeline helps write better tests. See `SimplificationPipeline.md` for full details.

**Five Phases:**

1. **Phase 1: Structural Simplifications** - Transform between function types
2. **Phase 2: Atomic Simplification** - Simplify atomic expressions
3. **Phase 3: Identity Simplifications** - Apply mathematical identities
4. **Phase 4: Evaluation** - Evaluate when possible
5. **Phase 5: Aggregation** - Convert to aggregates for further simplification

**Testing Strategy:**

- Test each phase independently when possible
- Test full pipeline for integration scenarios
- Use ExpressionMatchersSpec for phase-specific patterns
- Use ExpressionSpec for full pipeline tests

## Test Maintenance

### When Refactoring Code

1. **Run affected tests** after changes
2. **Update tests** if behavior intentionally changed
3. **Don't delete tests** without understanding why they fail
4. **Add tests** for bug fixes

### When Reorganizing Tests

1. **Move tests** to appropriate files using this guide
2. **Check for duplicates** before moving
3. **Update test names** if file organization changes context
4. **Keep git history** clear with focused commits

### Periodic Review

Periodically review test suite for:
- Duplicate tests across files
- Outdated or obsolete tests
- Missing coverage for new features
- Opportunities for table-driven tests

## Additional Resources

- **SimplificationPipeline.md** - Detailed simplification architecture
- **Coverage Reports** - `target/scala-3.7.3/scoverage-report/`
- **ScalaTest Documentation** - https://www.scalatest.org/
- **Cats Testing** - https://typelevel.org/cats/typeclasses/lawtesting.html

## Questions?

For questions about testing conventions:
1. Check this document first
2. Look at similar existing tests for patterns
3. Ask in pull request comments
4. Open an issue for clarification

---

**Last Updated:** January 27, 2026  
**Version:** 1.0