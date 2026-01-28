# Testing Quick Reference

Quick guide for adding tests to the Number library.

## Where Does My Test Go?

| I want to test... | Add to... |
|------------------|-----------|
| New atomic constant (e.g., golden ratio) | `AtomicExpressionSpec.scala` |
| Atomic property (isAtomic, depth, exactness) | `AtomicExpressionSpec.scala` |
| Expression operation ((a+b)*(a-b)) | `ExpressionSpec.scala` |
| Integration scenario (parse→simplify→eval) | `ExpressionSpec.scala` |
| Simplification pattern (x^a * x^b → x^(a+b)) | `ExpressionMatchersSpec.scala` |
| Matcher behavior (complementary terms) | `ExpressionMatchersSpec.scala` |
| Edge case or boundary condition | `ExpressionEdgeCasesSpec.scala` |
| Previously untested code path | `ExpressionEdgeCasesSpec.scala` |

## Good Test Names

✅ **Good:**
- `"simplify sqrt(2) * sqrt(2) to 2"`
- `"evaluate cos(π) to -1"`
- `"preserve exactness through addition"`

❌ **Bad:**
- `"work correctly"`
- `"test simplification"`
- `"be ok"`

## Test Checklist

Before committing:
- [ ] Test is in the correct file
- [ ] Test name clearly describes behavior
- [ ] No println statements
- [ ] No empty test stubs
- [ ] Clear assertions (not just "should not crash")
- [ ] Test runs locally: `sbt "testOnly *MySpec"`

## Anti-Patterns to Avoid

❌ **Don't commit:**
```scala
it should "test something" in {
  println(s"Result: $result")  // Remove println
}

it should "test rendering" in { }  // Empty stub

it should "work" in {  // Vague name
  result should not be null  // Vague assertion
}
```

✅ **Instead:**
```scala
it should "simplify double negation to original value" in {
  val expr = UniFunction(UniFunction(Two, Negate), Negate)
  expr.simplify shouldBe Two
}
```

## Running Tests

```bash
# All tests
sbt test

# Specific file
sbt "testOnly *ExpressionSpec"

# Specific test
sbt "testOnly *ExpressionSpec -- -z \"simplify sqrt\""

# With coverage
sbt clean coverage test coverageReport
```

## Need More Details?

See `TESTING_CONVENTIONS.md` for comprehensive guide.