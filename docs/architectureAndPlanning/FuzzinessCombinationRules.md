# Fuzziness Combination Rules

## Overview

When two `Fuzziness` values are combined (via the `*` method or `combine`), the result
depends on the magnitudes and shapes of the two operands. The rules are applied in order,
with the first matching rule taking precedence.

## Definitions

* `DoublePrecisionTolerance` (≈ 2.2E-16): the machine epsilon for double-precision arithmetic.
* `DoublePrecisionFloor` = `DoublePrecisionTolerance * 2` (≈ 4.4E-16): the threshold below
  which a fuzz value is considered to be pure double-precision noise.
* `negligibleRatio` = 9: if the ratio of the larger to the smaller magnitude exceeds this,
  the smaller is considered negligible relative to the larger.

## Rules

### Rule 1: Double-precision floor

If either magnitude is below `DoublePrecisionFloor`, the two fuzz values are combined by
scaling the larger magnitude by `sqrt(2)`, keeping its shape unchanged.

*Rationale*: double-precision noise combined with double-precision noise is still
double-precision noise. Promoting to a more complex shape (e.g. Box→Trapezoid) would
add spurious complexity with no meaningful gain in accuracy.

### Rule 2: Negligible ratio

If the ratio of the larger magnitude to the smaller exceeds `negligibleRatio` (9), the
smaller is ignored and the larger is returned unchanged.

*Rationale*: a fuzz contribution that is 10× or more smaller than the existing fuzz
has negligible effect on the combined uncertainty. Combining it would promote the shape
(Box→Trapezoid, Trapezoid→Gaussian) without meaningfully changing the magnitude.

### Rule 3: Same shape — Box⊗Box

If both operands are `Box`, combine via the Trapezoid convolution formula:

* If ratio > `negligibleRatio`: return the larger as `Box` (already handled by Rule 2).
* Otherwise: return `AbsoluteFuzz(a + b, Trapezoid(a, b))` where `a ≤ b` are the two
  half-widths.

*Rationale*: the convolution of two uniform distributions is a trapezoidal distribution.
This is an exact result, not an approximation.

### Rule 4: Involving Trapezoid or Gaussian

If either operand is `Trapezoid` or `Gaussian`, convert both to Gaussian via their
respective sigma values and combine via quadrature (root-sum-of-squares):

* `Gaussian`: sigma = magnitude
* `Box`: sigma = `magnitude / sqrt(3)`
* `Trapezoid(a, b)`: sigma = `sqrt((a² + b²) / 3)`

Return `AbsoluteFuzz(sqrt(sigma1² + sigma2²), Gaussian)`.

*Rationale*: by the Central Limit Theorem, the convolution of distributions tends toward
Gaussian. Once a Trapezoid or Gaussian is present, further convolution is best
approximated as Gaussian.

## Shape promotion summary

| this \ convolute | Box       | Trapezoid | Gaussian |
|------------------|-----------|-----------|----------|
| Box              | Trapezoid | Gaussian  | Gaussian |
| Trapezoid        | Gaussian  | Gaussian  | Gaussian |
| Gaussian         | Gaussian  | Gaussian  | Gaussian |

(Subject to Rules 1 and 2 suppressing promotion when magnitudes are very different.)

## `maybeAddFuzz`

With these rules in place, `maybeAddFuzz` is identical to `addFuzz`. The intelligence
for suppressing spurious shape promotion lives entirely within `Fuzziness.*`.