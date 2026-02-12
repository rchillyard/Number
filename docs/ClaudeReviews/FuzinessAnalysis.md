/*
* ANALYSIS: Fuzziness.scala Wiggle Room Calculations
*
* Reviewing consistency and accuracy of wiggle calculations for Box and Gaussian distributions.
  */

================================================================================
OVERVIEW
================================================================================

The wiggle function computes the range ±t such that probability p of finding
a random point within that range, given a distribution with extent/scale l.

Key insight: HIGHER p means SMALLER wiggle (stricter confidence requirement)

================================================================================
BOX DISTRIBUTION (Uniform) - Lines 703-710
================================================================================

def wiggle(l: Double, p: Double): Double = p match {
case 0.0 => Double.PositiveInfinity
case 1.0 => 0
case _ => l / 2
}

ANALYSIS:
✅ CORRECT for edge cases:
- p=0.0: Infinite wiggle room (anything is possible)
- p=1.0: Zero wiggle room (only exact value accepted)

⚠️  ISSUE: For all other p values (0 < p < 1), returns constant l/2

PROBLEM: This ignores the actual value of p!
- p=0.1 → wiggle = l/2
- p=0.5 → wiggle = l/2
- p=0.9 → wiggle = l/2

EXPECTED BEHAVIOR for Box (uniform distribution):
For a uniform distribution with half-width l:
- probability(x) = 2x/l for 0 ≤ x < l
- probability(x) = 1 for x ≥ l

To have probability p of being within ±wiggle:
2 * wiggle / l = p  (for p < 1)
wiggle = p * l / 2

CORRECTED FORMULA should be:
def wiggle(l: Double, p: Double): Double = p match {
case 0.0 => Double.PositiveInfinity
case 1.0 => l // At ±l, we have 100% of the distribution
case _ => p * l / 2
}

However, looking at your test data:
Fuzz=0.1, p=0.1 → wiggle=0.09496... ≈ 0.095 * fuzz

This doesn't match Box formula! Let me check...

================================================================================
THE TWIST: normalizeShape CONVERSION
================================================================================

From lines 346-349 (AbsoluteFuzz.normalizeShape):
def normalizeShape: Fuzziness[T] = shape match {
case Gaussian => this
case Box => AbsoluteFuzz(Box.toGaussianAbsolute(magnitude), Gaussian)
}

From lines 221-224 (RelativeFuzz.normalizeShape):
def normalizeShape: Fuzziness[T] = shape match {
case Gaussian => this
case Box => RelativeFuzz(Box.toGaussianRelative(tolerance), Gaussian)
}

AH! When Box is normalized to Gaussian (lines 669-690):
private val uniformToGaussian = 1.0 / math.sqrt(3)  // ≈ 0.577

def toGaussianRelative(x: Double): Double = x * uniformToGaussian
def toGaussianAbsolute[T](t: T): T = scale(t, uniformToGaussian)

So Box with magnitude m becomes Gaussian with stddev = m / √3

================================================================================
GAUSSIAN DISTRIBUTION - Lines 780-781
================================================================================

def wiggle(l: Double, p: Double): Double =
l / sigma * erfInv(1 - p)

Where sigma = √0.5 = 0.707...

ANALYSIS:
✅ MATHEMATICALLY CORRECT

This uses the inverse error function to find x such that:
erf(x * sigma / l) = 1 - p

For a normal distribution with mean 0 and stddev l:
P(-x ≤ X ≤ x) = erf(x / (√2 * l))

The formula accounts for variance = 1/2 representation.

Let me verify with your test data:
Fuzz=0.1, p=0.1 → wiggle = ?

But wait! Fuzz=0.1 with Box gets converted:
Gaussian_stddev = 0.1 / √3 ≈ 0.0577

Then wiggle = 0.0577 / 0.707 * erfInv(0.9)
= 0.0577 / 0.707 * 1.163...
≈ 0.095

✅ MATCHES YOUR TEST OUTPUT!

================================================================================
PROBABILITY FUNCTIONS - Consistency Check
================================================================================

Box.probability (lines 719-722):
def probability(l: Double, x: Double): Double = x match {
case y if y >= l => 1
case _ => 2 * x / l
}

✅ CORRECT: For uniform distribution [-l, l], probability of being within ±x is:

- x/l * 2 for x < l (linear from 0 to 1)
- 1 for x ≥ l

Gaussian.probability (lines 790-795):
def probability(l: Double, x: Double): Double = x match {
case Double.PositiveInfinity => 1
case _ => erf(x * sigma / l)
}

✅ CORRECT: Uses error function to compute CDF

================================================================================
CONVOLUTION OPERATIONS
================================================================================

AbsoluteFuzz.* (lines 335-344):
def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] =
if (this.shape == convolute.shape)
convolute match {
case AbsoluteFuzz(m, _) =>
AbsoluteFuzz(..., Gaussian.convolutionSum(...), shape)
...
}

⚠️ ISSUE: Always uses Gaussian.convolutionSum regardless of shape!
Should use shape-specific convolution.

RelativeFuzz.* (lines 208-216):
def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] =
if (this.shape == convolute.shape)
convolute match {
case RelativeFuzz(t, Box) => RelativeFuzz(tolerance + t, shape)
case RelativeFuzz(t, _) => RelativeFuzz(Gaussian.convolutionProduct(...), shape)
...
}

✅ GOOD: Box uses simple addition (correct for uniform distributions)
✅ GOOD: Gaussian uses proper convolution formula

================================================================================
SUMMARY & RECOMMENDATIONS
================================================================================

✅ CORRECT:

1. Gaussian.wiggle - mathematically sound using erfInv
2. Gaussian probability and convolution operations
3. Box → Gaussian conversion factor (1/√3)
4. RelativeFuzz convolution handles Box correctly

⚠️ ISSUES FOUND:

1. **Box.wiggle ignores p parameter** (except for p=0 and p=1)
    - Current: Always returns l/2
    - Should return: p * l / 2 (for p < 1), or l (for p = 1)
    - IMPACT: LOW in practice because Box is always normalized to Gaussian before wiggle is called

2. **AbsoluteFuzz.* uses Gaussian convolution for Box shapes**
    - Should check shape and use appropriate convolution
    - For Box: simple addition of magnitudes
    - For Gaussian: root sum squares
    - IMPACT: MEDIUM - could give incorrect uncertainty propagation

3. **Documentation mismatch in Box.wiggle comment** (line 700-701)
    - Says "actual `p` value is ignored" but doesn't explain WHY
    - Should document that Box is converted to Gaussian for wiggle calculations

================================================================================
RECOMMENDED FIXES
================================================================================

1. Fix Box.wiggle to respect p parameter:
   def wiggle(l: Double, p: Double): Double = p match {
   case 0.0 => Double.PositiveInfinity
   case 1.0 => l
   case _ => p * l / 2 // Linear interpolation
   }

2. Fix AbsoluteFuzz.* to handle Box shapes properly:
   def *(convolute: Fuzziness[T], independent: Boolean): Fuzziness[T] =
   if (this.shape == convolute.shape)
   convolute match {
   case AbsoluteFuzz(m, Box) =>
   AbsoluteFuzz(tv.plus(magnitude, m), Box)  // Simple addition
   case AbsoluteFuzz(m, _) =>
   AbsoluteFuzz(..., Gaussian.convolutionSum(...), shape)
   ...
   }

3. Add documentation explaining conversion strategy:
   /**
    * NOTE: Box distributions are typically converted to Gaussian via
    * normalizeShape before wiggle is called, using the factor 1/√3.
    * This is why the Box.wiggle implementation ignores p values
    * between 0 and 1 - in practice, Gaussian.wiggle is used.
      */

================================================================================
OVERALL ASSESSMENT
================================================================================

The wiggle calculations are **FUNCTIONALLY CORRECT** for your use case because:

1. Box shapes are normalized to Gaussian before wiggle is called
2. Gaussian.wiggle is mathematically sound
3. The √3 conversion factor is correct

The issues are primarily:

- Code maintainability (Box.wiggle doesn't match its contract)
- Potential bugs if Box.wiggle is called directly without normalization
- Convolution mixing for AbsoluteFuzz

Your test results confirm the implementation works correctly in practice! ✅