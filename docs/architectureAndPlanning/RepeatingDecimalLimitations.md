# Repeating Decimal Rendering: Limitations and Known Issues

## Overview

The Number library can render rational numbers as repeating decimals using the `<>` notation
(e.g. `0.00<72992700>` for 1/137). This is handled by `Rational.findRepeatingSequence` and
its supporting methods in `Rational.scala` and `Prime.scala`.

---

## How It Works

For a rational number `n/d`, the decimal expansion eventually repeats with a period that
divides `φ(d)` (Euler's totient). For a prime denominator `p`, the period divides `p - 1`
(by Fermat's Little Theorem). The algorithm:

1. **`denominatorPrimeFactors`** — finds the prime factors of `d`
2. **`getPeriods`** — determines candidate period lengths from the prime factors' reciprocal periods
3. **`createRecurrenceString`** — generates enough decimal digits and searches for the repeating pattern
4. **`findRepeatingPattern`** — tests candidate period lengths against the digit string

---

## Known Limitations

### 1. BigNumber Length Limit (~1000 digits)

`createRecurrenceString` uses `BigNumber.value(n).divide(BigNumber.value(d))` to generate
the decimal expansion. This generates approximately 1000 characters. Since at least two
repetitions are needed to detect the pattern, the maximum detectable period length is
approximately **500 digits**.

This means primes `p` where the period of `1/p` exceeds ~500 will fail with:
```
Failure(RationalException("Rational.findRepeatingSequence: no sequence"))
```

Practically, this limits reliable detection to primes where `p - 1 < ~500`, i.e. roughly **`p < 500`**
(though many larger primes have short periods and will work fine).

### 2. `denominatorPrimeFactors` Search Limit

`denominatorPrimeFactors` calls `Prime.primeFactors(d, Some(MAX_PRIME_FACTORS * 3))` with
a search limit of `21`. This means prime factors larger than `21` are not found directly.

**Fix applied (Issue #184)**: When `primeFactors` returns empty but `d < MAXDECIMALDIGITS`,
the algorithm now falls back to `d - 1` as the candidate period. This correctly handles
prime denominators like `137` where the denominator itself is prime and larger than the
search limit.

### 3. `reciprocalPeriods` Lookup Table

`Prime.reciprocalPeriods` contains the known period lengths for the first 100 primes.
This is used as a fast lookup to determine candidate periods. If a prime is not in
`hundredPrimes` (i.e. it's larger than 541), `reciprocalPeriod` returns `None`, and the
algorithm falls back to `d - 1` via the fix described above.

### 4. `expandProducts` Size Limit

When `d - 1` has many distinct prime factors, `expandProducts` generates all products of
those factors to form period candidates. This is implemented for up to 7 distinct prime
factors. If `d - 1` has more than 7 distinct prime factors, an exception is thrown:
```
RationalException("Rational.getPeriods: not yet implemented for: ...")
```

In practice, numbers with 8+ distinct prime factors are extremely large (the product of
the first 8 primes is 9,699,690), so this limit is unlikely to be encountered for
reasonable denominators.

---

## Summary Table

| Denominator type              | Status  | Notes                                              |
|-------------------------------|---------|----------------------------------------------------|
| Factors of 2 and/or 5 only    | ✅ Works | Terminates, no repeating part                      |
| Small prime (≤ 541, in table) | ✅ Works | Uses `reciprocalPeriods` lookup                    |
| Prime 137 (period 8)          | ✅ Fixed | Issue #184 — uses `d-1` fallback                   |
| Prime with period < 500       | ✅ Works | `d-1` fallback + `BigNumber` length sufficient     |
| Prime with period > 500       | ❌ Fails | `BigNumber` length limit (~1000 chars)             |
| 1/137 specifically            | ✅ Fixed | Renders as `"0.00<72992700>"` (period 8)           |
| 1/541 (last in table)         | ✅ Works | Period 20, well within limits                      |

---

## Examples

```scala
Rational(3).invert.renderExact    // "0.<3>"        — period 1
Rational(7).invert.renderExact    // "0.<142857>"   — period 6
Rational(37).invert.renderExact   // "0.<027>"      — period 3
Rational(137).invert.renderExact  // "0.00<72992700>" — period 8 (fixed in Issue #184)
Rational(541).invert.renderExact  // period 20, works
```

---

## Potential Future Improvements

1. **Increase `BigNumber` precision** — allowing longer periods to be detected
2. **Compute period directly** — instead of searching, compute the multiplicative order
   of 10 modulo `d` directly: the smallest `k` such that `10^k ≡ 1 (mod d)`. This would
   be exact and not limited by string length.
3. **Extend `reciprocalPeriods`** — add more primes beyond the first 100
4. **Increase `denominatorPrimeFactors` search limit** — or remove it entirely for
   small denominators

The most impactful improvement would be (2) — computing the period directly via
modular arithmetic, which is both exact and efficient for any size denominator.

---

*Document created: February 2026*  
*Related: Issue #184 — `findRepeatingSequence` fails for 1/137*