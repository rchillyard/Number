# Number Notation: Rendering and Parsing Rules

This document defines the notation used for rendering and parsing numbers in the Number library, including exact values,
fuzzy/approximate values, and repeating decimals.

## Symbol Semantics

### Core Notation Symbols

| Symbol     | Meaning                                               | Usage Context                                          |
|------------|-------------------------------------------------------|--------------------------------------------------------|
| `*`        | Fuzziness/uncertainty                                 | Indicates approximate value with Box fuzziness         |
| `...`      | Truncation (parsing) or Large denominator (rendering) | Parsing: creates fuzziness; Rendering: large rationals |
| `<digits>` | Repeating decimal pattern                             | Indicates exact rational with repeating sequence       |
| `(n)`      | Gaussian fuzziness                                    | Explicit uncertainty (n = digits of precision)         |
| `[n]`      | Box fuzziness                                         | Explicit uncertainty (n = digits of precision)         |

### Symbol Constants

These are defined in `WithFuzziness` companion object:

```scala
object WithFuzziness {
  val Asterisk: String = "*"
  val Ellipsis: String = "..."
  val RepeatOpen: String = "<"
  val RepeatClose: String = ">"
  val GaussianOpen: String = "("
  val GaussianClose: String = ")"
  val BoxOpen: String = "["
  val BoxClose: String = "]"
}
```

## Parsing Rules

### Input Notation → Internal Representation

| Input String | Parsed As      | Fuzziness                        | Example                         |
|--------------|----------------|----------------------------------|---------------------------------|
| `"2.0"`      | Exact rational | None                             | `Rational(2, 1)`                |
| `"2.0*"`     | Fuzzy rational | Box (±5 in last place)           | `FuzzyNumber(2.0, Box)`         |
| `"0.333..."` | Fuzzy rational | Box (±5 in last place)           | `FuzzyNumber(0.333, Box)`       |
| `"0.<3>"`    | Exact rational | None                             | `Rational(1, 3)`                |
| `"0.1<6>"`   | Exact rational | None                             | `Rational(1, 6)`                |
| `"2.5[2]"`   | Fuzzy rational | Box (±2 in specified place)      | `FuzzyNumber(2.5, Box(2))`      |
| `"2.5(2)"`   | Fuzzy rational | Gaussian (±2 in specified place) | `FuzzyNumber(2.5, Gaussian(2))` |
| `"1/3"`      | Exact rational | None                             | `Rational(1, 3)`                |

### Fuzziness Generation

When `*` or `...` appear in parsing without explicit uncertainty digits:

```scala
// Default Box fuzziness calculation:
// Uncertainty = ±5 × 10^(exponent - decimalPlaces - 1)

"2.5*"     → ±5 × 10^(-2) = ±0.05
"2.500*"   → ±5 × 10^(-4) = ±0.0005
"1.23e5*"  → ±5 × 10^4 = ±50,000
```

### Repeating Decimal Parsing

The notation `0.nonrepeating<repeating>` converts to an exact rational:

```scala
// Formula:
// result = integerPart + (nonRepeating / 10^len) + (repeating / (10^len - 1)) / 10^(nonRepLen)

"0.<3>"        → 1/3
"0.1<6>"       → 1/6  = 1/10 + 6/90
"0.<142857>"   → 1/7
"1.<3>"        → 4/3
"0.16<6>"      → 1/6
```

## Rendering Rules

### Internal Representation → Output String

| Number Type    | Condition                  | Rendered As                 | Example             |
|----------------|----------------------------|-----------------------------|---------------------|
| Exact rational | Small denominator          | As-is or decimal            | `2` or `2.0`        |
| Exact rational | Repeating pattern detected | `<repeating>` notation      | `0.<3>`             |
| Exact rational | Denominator > 100,000      | Truncated with `...`        | `0.123456789012...` |
| Fuzzy number   | Any                        | Value with `*` suffix       | `2.5*`              |
| Fuzzy number   | Large denominator          | Ellipsis removed, `*` added | `0.123456789012*`   |

### Repeating Decimal Detection

When rendering exact rationals, the library attempts to detect repeating patterns:

- Checks denominators that are products of small primes
- Identifies the period of repetition
- Formats as `integerPart.nonRepeating<repeating>`

Example: `Rational(1, 7)` → `"0.<142857>"`

If no pattern is found or denominator is too large:

- Small denominator (≤ 100,000): renders as decimal or fraction
- Large denominator (> 100,000): renders with `...` truncation

### Fuzziness Rendering

Fuzzy numbers always render with `*` suffix:

```scala
FuzzyNumber(2.5, fuzz = Some(Box(0.05)))  → "2.5*"
```

If the underlying value would render with `...` (large denominator), the ellipsis is **removed** and replaced with `*`:

```scala
// Internal: Rational(large_numerator, large_denominator) with fuzziness
// Would render as "0.123456789012..." if exact
// Actually renders as "0.123456789012*" because fuzzy
```

## Round-Trip Behavior

### Guaranteed Round-Trips

These notations parse and render identically:

| Original   | Parsed         | Re-rendered | Round-trip?         |
|------------|----------------|-------------|---------------------|
| `"2.0*"`   | Fuzzy(2.0)     | `"2.0*"`    | ✓ Yes               |
| `"0.<3>"`  | Rational(1,3)  | `"0.<3>"`   | ✓ Yes               |
| `"0.1<6>"` | Rational(1,6)  | `"0.1<6>"`  | ✓ Yes               |
| `"2.5[2]"` | Fuzzy with Box | `"2.5[2]"`  | ✓ Yes (if explicit) |

### Acceptable Non-Round-Trips

These notations change form but preserve semantic meaning:

| Original               | Parsed        | Re-rendered | Note                       |
|------------------------|---------------|-------------|----------------------------|
| `"0.333..."`           | Fuzzy(0.333)  | `"0.333*"`  | Both mean "approximate"    |
| `"0.3333333333333333"` | Rational(1,3) | `"0.<3>"`   | Exact pattern detected     |
| `"1/3"`                | Rational(1,3) | `"0.<3>"`   | Equivalent representations |

The transformation from `...` to `*` is acceptable because:

1. Both indicate approximation/fuzziness
2. The `*` notation is more explicit about uncertainty
3. The semantic meaning (fuzzy number) is preserved

## Implementation Notes

### Parser Modules

- **`NumberParser`**: Handles full number notation including fuzziness markers
- **`RationalParser`**: Handles basic rational and decimal parsing
- **`RepeatingDecimal`**: Case class for converting `<repeating>` notation to exact rationals

### Rendering Modules

- **`FuzzyNumber.render`**: Main rendering logic for fuzzy numbers
- **`Value.valueToString`**: Delegates to `Render.renderValue`
- **`Render.renderValue`**: Routes to appropriate rendering based on value type
- **`Rational.renderExact`**: Handles exact rational rendering with pattern detection
- **`Rational.findRepeatingSequence`**: Detects and formats repeating decimal patterns

### Key Rendering Decision Points

1. **Is the number fuzzy?**
    - Yes: Add `*` suffix (remove any `...` first)
    - No: Continue to exact rendering

2. **Can we detect a repeating pattern?**
    - Yes: Use `<repeating>` notation
    - No: Continue to decimal rendering

3. **Is the denominator too large?**
    - Yes (> 100,000): Truncate with `...`
    - No: Render as normal decimal or fraction

## Design Rationale

### Why Both `*` and `...`?

- **`*` (asterisk)**: Primary fuzziness indicator, used in both input and output
- **`...` (ellipsis)**: Natural notation for "continues" in mathematical writing
    - In **input**: Convenient shorthand for approximate values
    - In **output**: Indicates truncation of long decimals

### Why Allow Non-Round-Trip for Ellipsis?

The decision to allow `"0.333..."` → `"0.333*"` transformation was based on:

1. **Semantic equivalence**: Both notations convey "approximate value"
2. **Output clarity**: `*` is more explicit about fuzziness than `...`
3. **Simplicity**: Avoids needing to track "how did this fuzziness originate?"
4. **Practicality**: Users can input either notation naturally

### Why Angle Brackets for Repeating Decimals?

- **Distinctive**: Clearly different from other notation (not confused with fuzziness)
- **Compact**: More readable than alternatives like `0.3(3)` or `0.3̄`
- **Parseable**: Easy to parse unambiguously
- **Mathematical**: Suggests "this pattern repeats indefinitely"

## Testing Considerations

### Essential Test Cases

**Basic round-trips:**

```scala
"2.0*" → parse → render → "2.0*"
"0.<3>" → parse → render → "0.<3>"
```

**Repeating decimal conversion:**

```scala
"0.<3>" → Rational(1, 3)
"0.<142857>" → Rational(1, 7)
"0.16<6>" → Rational(1, 6)
```

**Fuzziness equivalence:**

```scala
"2.5*" and "2.5..." both → FuzzyNumber with Box fuzz
```

**Pattern detection:**

```scala
Rational(1, 3) → renders as "0.<3>"
Rational(1, 7) → renders as "0.<142857>"
```

**Large denominator handling:**

```scala
Rational(1, 200000) → renders with "..."
FuzzyNumber(1, 200000) → renders with "*" (ellipsis removed)
```

## Future Considerations

### Potential Enhancements

1. **User-configurable thresholds**: Allow users to set the denominator limit for ellipsis
2. **Alternative notations**: Support for vinculum (`0.3̄`) or parentheses (`0.3(3)`) for repeating decimals
3. **Percentage notation**: Special handling for `%` suffix
4. **Scientific notation**: Better integration with `e` notation and fuzziness

### Known Limitations

1. **Pattern detection limits**: Very long period repeating decimals may not be detected
2. **Prime factorization limits**: Large denominators may not be fully analyzed
3. **Unicode rendering**: Some platforms may not display special characters correctly

## References

- **Source files**: `NumberParser.scala`, `RationalParser.scala`, `FuzzyNumber.scala`, `Rational.scala`
- **Constants**: `WithFuzziness` companion object
- **Related documentation**: See `SKILL.md` files for additional guidance on number formatting

---

*Last updated: February 2026*
*For questions or clarifications, see the Number library documentation or raise an issue.*