# Number Library Rendering Refactor: `show` vs `render`

## Executive Summary

The current rendering system tries to serve multiple conflicting goals through a single `render` method, leading to fragile code and test brittleness. This document outlines a refactor to separate concerns into three distinct rendering paths: **canonical** (for round-trips), **display** (for humans), and **qualified** (for scientific precision).

**Status**: Band-aid applied, all tests green with 4 ignores. Ready for major refactor.

---

## Current Problems

### The Core Issue
A single `render` method attempts to:
1. Produce round-trippable output (parse → render → parse should preserve value)
2. Display human-friendly formatting (avoid excessive zeros, use readable notation)
3. Handle scientific notation appropriately
4. Embed uncertainty notation inline (`[5]`, `(11)`)
5. Work for both exact and fuzzy numbers

These goals conflict, resulting in:
- Fragile mask trimming logic that breaks with edge cases
- Scientific notation thresholds that work for some cases but break others
- Trailing zero handling that's inconsistent
- Tests that fail when format changes even if values are correct

### Specific Issues Encountered

1. **Decimal point positioning**: Values like `1836.15267343` rendering as `1.83615267343` due to aggressive scientific notation
2. **Trailing zeros**: `100.0(5)` vs `100.000(5)` - mask length doesn't match value precision
3. **Round-trip failures**: `"0.333..."` → `"0.333*"` is acceptable but requires different expectations
4. **Uncertainty digit errors**: `(11)` rendering as `(10)` (calculation bug, not format issue)

### What's Working (Don't Break!)

✅ Vulgar fraction parsing and rendering (`"⅓"` ↔ `Rational(1,3)`)
✅ Repeating decimal notation (`"0.<3>"` ↔ `Rational(1,3)`)
✅ Symbol semantics clearly defined (`*` = fuzzy, `...` = truncated, `<>` = repeating)
✅ Parser/renderer consistency for basic cases
✅ Scientific notation for very large/small numbers (|exp| ≥ 6)

---

## Proposed Solution: Three-Tier Rendering System

### Tier 1: `render` - Canonical Form
**Purpose**: Round-trippable, machine-readable, preserves all information  
**Location**: Core trait method used by all Number types  
**Guarantees**: `Number.parse(x.render) ≈ x` (value and fuzziness preserved)

**Rules**:
- Always show enough decimal places to indicate uncertainty position
- Use simple notation: `*` for default fuzz, `[n]`/`(n)` for explicit uncertainty
- Prefer decimal notation over scientific unless |exp| > 100
- Preserve mathematical precision over brevity
- Examples: `"0.10*"`, `"1836.15267343(11)"`, `"2.5[2]"`

**Implementation**:
```scala
trait Number {
   def render: String // Canonical form for round-trips
}
```

### Tier 2: `show` - Display Form
**Purpose**: Human-friendly, readable, optimized for console/UI display  
**Location**: Cats `Show` typeclass instance, also available as method  
**Guarantees**: Clear and readable, may lose some precision details

**Rules**:
- Can drop trailing zeros: `"0.1*"` instead of `"0.10*"`
- Can use friendly formats: `"100±1%"` instead of `"100.0*"`
- Can simplify: `"5"` instead of `"5.<0>"`
- Use scientific notation for very large/small (|exp| ≥ 6)
- Prefer common mathematical notation (fractions as `⅓` rather than `0.<3>`)
- Examples: `"0.1*"`, `"100±1%"`, `"⅓"`, `"1.23×10⁶"`

**Implementation**:
```scala
trait Number {
   def show: String // Friendly display
}

object Number {
   implicit val showNumber: Show[Number] = Show.show(_.show)
}
```

### Tier 3: `asAbsolute`/`asRelative`/`asPercentage` - Qualified Forms
**Purpose**: Scientific/technical precision, explicit uncertainty notation  
**Location**: Extension methods via `MaybeFuzzy` (algebra module)  
**Guarantees**: Unambiguous uncertainty representation

**Rules**:
- Always use scientific notation: `"1.00(1)E+02"`
- Always show absolute uncertainty explicitly
- For `asRelative`: convert to relative and show as percentage
- For `asPercentage`: show uncertainty as percentage
- Examples: `"1.00(1)E+02"`, `"100.0±1.0%"`, `"2.500±0.5%"`

**Implementation**:
```scala
// Already exists in MaybeFuzzy.scala
extension (m: MaybeFuzzy)
  def asAbsolute: String   // Scientific notation with absolute uncertainty
  def asRelative: String   // Decimal notation with relative uncertainty
  def asPercentage: String // Percentage notation
```

---

## Migration Plan

### Phase 1: Separate `render` from `show` (Core Module)

**Step 1.1**: Add `show` method to `Number` trait
```scala
trait Number {
  def render: String  // Existing - canonical form
  def show: String    // New - display form
}
```

**Step 1.2**: Implement `show` in concrete classes
- `ExactNumber.show`: Simplify where possible (drop `.0`, use vulgar fractions)
- `FuzzyNumber.show`: Friendly fuzz notation, drop trailing zeros
- Keep existing `render` implementations as canonical form

**Step 1.3**: Update test expectations
- Round-trip tests use `render`
- Display tests use `show`
- Clearly separate the two concerns in test suites

### Phase 2: Fix `render` for True Round-Trips (Core Module)

**Step 2.1**: Ensure `render` always shows enough decimal places
```scala
// In FuzzyNumber.render or similar
def render: String = {
  val precision = calculateRequiredPrecision(value, fuzziness)
  val formatted = formatWithPrecision(value, precision)
  val fuzzNotation = formatFuzziness(fuzziness, precision)
  s"$formatted$fuzzNotation"
}
```

**Step 2.2**: Fix mask length calculation
- Base mask length on value precision, not uncertainty magnitude
- Ensure trailing zeros are preserved when needed

**Step 2.3**: Comprehensive round-trip testing
```scala
property("parse-render round-trips preserve value") {
  forAll(numberGen) { n =>
    val rendered = n.render
    val reparsed = Number.parse(rendered).get
    reparsed.materialize shouldBe n.materialize
    reparsed.fuzz.map(_.wiggle(0.5)) shouldBe n.fuzz.map(_.wiggle(0.5)) +- epsilon
  }
}
```

### Phase 3: Enhance `show` for Better UX (Core Module)

**Step 3.1**: Smart simplification
```scala
def show: String = (value, fuzz) match {
  case (r: Rational, None) if isVulgarFraction(r) => toVulgarFraction(r)
  case (r: Rational, None) if r.isWhole => r.toBigInt.toString
  case (_, Some(f)) => friendlyFuzzyFormat(value, f)
  case _ => render  // Fall back to canonical
}
```

**Step 3.2**: Percentage display for relative fuzz
```scala
// For relative fuzz, automatically show as percentage
case Some(RelativeFuzz(tolerance, _)) => 
  s"${value}±${(tolerance * 100).round}%"
```

**Step 3.3**: Locale/context awareness (future)
- Scientific notation preferences
- Decimal separator (. vs ,)
- Thousands separator preferences

### Phase 4: Refine Qualified Forms (Algebra Module)

**Step 4.1**: Fix `asAbsolute` to always use scientific notation
```scala
def asAbsolute: String = {
  fuzz.normalize(value, relative = false) match {
    case Some(absFuzz) =>
      // Force scientific notation for consistency
      formatScientificWithUncertainty(value, absFuzz)
    case None => 
      formatScientific(value)
  }
}
```

**Step 4.2**: Ensure `asRelative` is truly relative
- Convert absolute fuzz to relative
- Show as percentage
- Handle edge cases (zero value, etc.)

**Step 4.3**: Add `asEngineering` for engineering notation
- Exponents in multiples of 3
- SI prefix support (k, M, G, etc.)

---

## Implementation Details

### Required Precision Calculation

The key to `render` working correctly:

```scala
def calculateRequiredPrecision(value: Double, fuzz: Option[Fuzziness[Double]]): Int = {
  fuzz match {
    case None => 
      // For exact values, show precision of the Double representation
      val bd = BigDecimal(value)
      bd.scale
      
    case Some(f) =>
      // For fuzzy values, show enough places for the uncertainty
      val uncertainty = f.wiggle(0.5)
      val uncertaintyExp = if (uncertainty > 0) {
        math.floor(math.log10(uncertainty)).toInt
      } else -15  // Default to high precision
      
      // Show at least to the position where uncertainty matters
      math.max(1, -uncertaintyExp + 1)
  }
}
```

### Mask Length Matching

The critical bug fix in `AbsoluteFuzz.getQualifiedString`:

```scala
// Current (WRONG): mask length based on qPrefix from scaled magnitude
val mask = new String(qPrefix) + "0" * (2 + adjust) + brackets.head + yq + brackets.tail.head

// Fixed: mask length should match value's precision
val valuePrecision = calculateValuePrecision(t)
val mask = "0" * valuePrecision + brackets.head + yq + brackets.tail.head
```

This ensures that for `1836.15267343(11)`:
- Value has 8 decimal places of precision
- Mask has 8 zeros before the `(11)`
- No extra trailing zeros

### Scientific Notation Thresholds

Keep the current settings:

```scala
// In HasValueDouble.render
if (absValue >= 10000.0 || absValue < 0.001) {
  f"$t%.20E"
}
```

This provides:
- Scientific for very large: `≥ 10,000`
- Scientific for very small: `< 0.001`
- Decimal for normal range: `[0.001, 10,000)`

### Examples

| Declared value      | Fuzziness              | `render` (canonical)  | `show` (display) | `asAbsolute` (scientific) |
|---------------------|------------------------|-----------------------|------------------|---------------------------|
| 2.5                 | ±0.05 (Box)            | `"2.50[5]"`           | `"2.5*"`         | `"2.50[5]"`               |
| "2.5"               | None                   | `"2.5"`               | `"2.5"`          | `"2.5"`                   |
| "9.81*"             | ±0.005 (Box)           | `"9.810[5]"`          | `"9.810[5]"`     | `"9.810[5]"`              |
| "9.81\[1]"          | ±0.01 (Box)            | `"9.81[1]"`           | `"9.81[1]"`      | `"9.81[1]"`               |
| 100.0   †           | ±0.5 (0.5% rel)        | `"100.*"`             | `"100±0.5%"`     | `"1.000[5]E+02"`          |
| "1836.15267343(11)" | ±0.00000011 (Gaussian) | `"1836.15267343(11)"` | `"1836.153(11)"` | `"1.83615267343(11)E+03"` |
| 1:/3                | None                   | `"0.<3>"`             | `"⅓"`            | `"3.333...E-01"`          |
| π                   | None                   | `"π"`                 | `"π"`            | `"3.14159265359E+00"`     |
| 0.0001              | ±0.00005               | `"0.0001*"`           | `"1×10⁻⁴*"`      | `"1.0[5]E-04"`            |

† 100.0 cannot be distinguished from 100 after the compiler has parsed this value.

---

## Testing Strategy

### Test Organization

```
NumberRenderSpec.scala      // Tests for render (canonical, round-trips)
NumberShowSpec.scala         // Tests for show (display, readability)
NumberQualifiedSpec.scala    // Tests for asAbsolute/asRelative/asPercentage
```

### Round-Trip Properties

```scala
class NumberRenderSpec {
  property("exact numbers round-trip perfectly") {
    forAll(exactNumberGen) { n =>
      val rendered = n.render
      val reparsed = Number.parse(rendered).get
      reparsed shouldBe n
    }
  }
  
  property("fuzzy numbers preserve value and fuzziness") {
    forAll(fuzzyNumberGen) { n =>
      val rendered = n.render
      val reparsed = Number.parse(rendered).get
      reparsed.materialize shouldBe n.materialize
      reparsed.fuzz.map(_.wiggle(0.5)) shouldBe 
        n.fuzz.map(_.wiggle(0.5)) +- 1e-10
    }
  }
}
```

### Display Quality Tests

```scala
class NumberShowSpec {
  it should "use vulgar fractions for common rationals" in {
    Number(Rational(1, 3)).show shouldBe "⅓"
    Number(Rational(1, 2)).show shouldBe "½"
  }
  
  it should "use percentage notation for relative fuzz" in {
    val n = Real(100, Some(RelativeFuzz(0.01, Box)))
    n.show shouldBe "100±1%"
  }
}
```

---

## Implementation Checklist

### Core Module (`com.phasmidsoftware.number.core`)

- [ ] Add `show: String` method to `Number` trait
- [ ] Implement `ExactNumber.show` with simplifications
- [ ] Implement `FuzzyNumber.show` with friendly formatting
- [ ] Keep `render` as canonical form
- [ ] Fix `FuzzyNumber.render` trailing zero issues
- [ ] Fix `AbsoluteFuzz.getQualifiedString` mask length calculation
- [ ] Update `FuzzyNumber` to use Cats `Show` typeclass
- [ ] Add comprehensive round-trip tests for `render`
- [ ] Add display quality tests for `show`

### Algebra Module (`com.phasmidsoftware.number.algebra`)

- [ ] Update `Real.render` to delegate to core `Number.render`
- [ ] Add `Real.show` for friendly display
- [ ] Fix `asAbsolute` to always use scientific notation
- [ ] Fix `asRelative` uncertainty calculations
- [ ] Add tests specifically for qualified forms
- [ ] Update existing tests to use appropriate methods (`show` vs `render` vs `asAbsolute`)

### Parse Module (`com.phasmidsoftware.number.expression.parse`)

- [ ] Ensure all notation forms parse correctly
- [ ] Verify round-trips work with `render` output
- [ ] Document which notations are input-only vs output-only
- [ ] Add parse error messages that suggest correct notation

### Documentation

- [ ] Update NUMBER_NOTATION.md with `show` vs `render` examples
- [ ] Add migration guide for users
- [ ] Document which method to use when
- [ ] Update README with rendering examples
- [ ] Add scaladoc with clear guidance

---

## Design Decisions to Make

### 1. Scientific Notation Threshold for `show`

**Question**: When should `show` use scientific notation?

**Options**:
- Conservative (|exp| ≥ 6): `"100000"` stays decimal, `"1000000"` → `"1×10⁶"`
- Moderate (|exp| ≥ 4): `"10000"` stays decimal, `"100000"` → `"1×10⁵"`
- Aggressive (|exp| ≥ 3): `"1000"` stays decimal, `"10000"` → `"1×10⁴"`

**Recommendation**: Conservative (≥ 6) for `show`, always scientific for `asAbsolute`

### 2. Trailing Zero Handling in `render`

**Question**: Should `render` always show trailing zeros to uncertainty position?

**Options**:
- A) Always show: `"2.50*"` (clear precision indication)
- B) Drop when unambiguous: `"2.5*"` (shorter, still parseable)

**Recommendation**: Option A - `render` should be unambiguous about precision

### 3. Vulgar Fraction Preference in `show`

**Question**: When should `show` prefer vulgar fractions over decimals?

**Options**:
- Always for common fractions: `⅓`, `½`, `¼`, `⅕`, `⅙`, `⅐`, `⅛`, `⅑`, `⅒`
- Only for halves/thirds/quarters: `½`, `⅓`, `¼`, `¾`
- Never (always use decimals)

**Recommendation**: Always for available vulgar fractions (more readable)

### 4. Repeating Decimal Rendering of Rationals in `render` vs `show`

**Question**: Should both use `<>` notation or should `show` prefer vulgar fractions?

**Decision Matrix**:

| Rational | `render`           | `show`           | Notes                                |
|----------|--------------------|------------------|--------------------------------------|
| 1/3      | `"0.<3>"`          | `"⅓"`            | Vulgar available                     |
| 1/7      | `"0.<142857>"`     | `"0.<142857>"`   | No vulgar, small prime, show pattern |
| 1/37     | `"0.<027>"`        | `"0.<027>"`      | No vulgar, show (short) pattern      |
| 1/137    | `"0.00<729927>"`   | `"0.00730..."`   | Long pattern, large prime, truncate? |

**Recommendation**: `render` uses `<>` when detected, `show` prefers vulgar fractions

### 5. Relative vs. Absolute Fuzz

**Key principle**: The parser always produces `AbsoluteFuzz`. The bracket/paren/asterisk notations
are inherently positional (tied to a specific decimal place), so they are absolute by definition.
There is no surface syntax for relative fuzz — it is an internal/computational representation.

`RelativeFuzz` arises from arithmetic operations: when multiplying two fuzzy numbers, both fuzz
values are converted to relative form so that tolerances can be combined by simple addition
(or root-sum-squares for Gaussian). This follows the standard error propagation rule:
for `z = x * y`, `δz/z = δx/x + δy/y`.

Consequently:
- `render` always produces absolute fuzz notation (`[n]`, `(n)`, `*`)
- `show` may display relative fuzz as a percentage: `"100±1%"`
- `asRelative` (Tier 3) explicitly converts to relative form for display

### 6. Zero Handling

**Question**: How should exact zero vs fuzzy zero render?

**Examples**:
- Exact: `"0"` in both `render` and `show`
- Fuzzy: `"0.0*"` in `render`, `"~0"` or `"0±ε"` in `show`?

**Recommendation**: Keep current behavior for now, revisit in Phase 3

---

## Code Locations

### Files to Modify

**Core Module**:
- `Number.scala` - Add `show` method signature
- `ExactNumber.scala` - Implement `show` with simplifications
- `FuzzyNumber.scala` - Implement `show` friendly, fix `render` precision
- `Fuzziness.scala` - Fix `AbsoluteFuzz.getQualifiedString` mask calculation
- `Value.scala` - Possibly add `valueToStringPrecise` for `render`

**Algebra Module**:
- `Real.scala` - Delegate to core `render`/`show`, update `Show` instance
- `MaybeFuzzy.scala` - Verify `asAbsolute`/`asRelative` work correctly
- Update test files to use appropriate methods

**Parse Module**:
- No changes needed (already working correctly)

### Key Methods to Refactor

1. **`FuzzyNumber.render`** (core/numerical/FuzzyNumber.scala ~line 187)
   - Current: Mixed goals (canonical + display)
   - After: Pure canonical form with precision preservation

2. **`AbsoluteFuzz.getQualifiedString`** (core/numerical/Fuzziness.scala ~line 375)
   - Current: Complex mask calculation with precision bugs
   - After: Simplified logic that respects value precision

3. **`HasValueDouble.render`** (core/numerical/Fuzziness.scala ~line 991)
   - Current: Used by everything, mixed purposes
   - After: Internal helper, not directly user-facing

---

## Risk Assessment

### Low Risk
- Adding `show` method (additive change, doesn't break existing code)
- Updating test expectations (just maintenance)
- Documentation improvements

### Medium Risk
- Changing `render` behavior (might break downstream users)
- Fixing `getQualifiedString` logic (complex, lots of edge cases)
- Scientific notation threshold changes

### High Risk
- Removing `.99f` formatting approach entirely
- Changing mask calculation fundamentally
- Breaking existing `render` contract

### Mitigation Strategies

1. **Incremental rollout**: Add `show` first, keep `render` unchanged initially
2. **Feature flag**: Use environment variable to switch between old/new rendering
3. **Extensive testing**: Property-based tests for round-trips
4. **Documentation**: Clear migration guide for users
5. **Deprecation period**: Mark old behavior as deprecated before removing

---

## Success Criteria

### Must Have (Phase 1-2)
✅ All round-trip tests pass with `render`  
✅ `show` produces human-friendly output  
✅ No precision bugs (trailing zeros correct)  
✅ Scientific notation thresholds work for all cases  
✅ Existing API compatibility maintained

### Should Have (Phase 3-4)
✅ Vulgar fractions in `show` where appropriate  
✅ Percentage notation for relative fuzz  
✅ `asAbsolute`/`asRelative` working correctly  
✅ Clear documentation of which method to use when  
✅ Property-based round-trip tests

### Nice to Have (Future)
- Locale awareness
- Configurable display preferences
- Engineering notation support
- SI unit prefix integration
- LaTeX output format

---

## Timeline Estimate

**Phase 1** (Add `show` method): 2-3 hours
- Low risk, mostly additive
- Main effort in test updates

**Phase 2** (Fix `render` precision): 4-6 hours
- Medium risk, requires careful debugging
- Mask calculation fix is complex

**Phase 3** (Enhance `show`): 2-3 hours
- Low risk, pure enhancement
- Mostly formatting logic

**Phase 4** (Fix qualified forms): 2-3 hours
- Medium risk, algebra module only
- Mostly test updates

**Total**: ~12-15 hours of focused work

---

## Open Questions

1. Should `render` ever use scientific notation, or only when absolutely necessary?
2. How should we handle very long repeating decimals (period > 20)?
3. Should `show` have a configurable precision parameter?
4. Do we need a `renderLaTeX` method for mathematical typesetting?
5. Should there be a `compact` format that drops all whitespace?

---

## References

- **NUMBER_NOTATION.md**: Current notation rules and symbol semantics
- **NumberNotationRoundTripSpec.scala**: Round-trip test suite (needs splitting)
- **Fuzziness.scala**: Core fuzziness rendering logic
- **MaybeFuzzy.scala**: Qualified form extensions (`asAbsolute`, etc.)
- **Git diff**: Changes that caused current issues (mostly in `getQualifiedString`)

---

## Notes from Implementation Sessions

### Session 1: Notation Cleanup (Feb 16, 2026)
- Established symbol semantics (`*`, `...`, `<>`)
- Added vulgar fraction support
- Added repeating decimal parsing
- Fixed scientific notation for very large numbers
- Result: All green with 4 ignores

### Lessons Learned
1. `getQualifiedString` is fragile - mask calculation depends on precise scaling
2. Scientific notation threshold affects multiple code paths
3. Round-trip requirements conflict with display preferences
4. Tests based on implementation details rather than contracts
5. Two parser modules (core vs expression) need to stay in sync

### Known Issues (TODOs)
- Uncertainty digit sometimes off by 1: `(10)` instead of `(11)`
- Trailing zeros in some fuzzy number formats
- Mask length calculation doesn't respect value precision
- `asAbsolute` tests currently ignored (need proper scientific notation)

---

*Document created: February 17, 2026*  
*Status: Band-aid applied, ready for major refactor*  
*Next steps: Implement Phase 1 (add `show` method)*