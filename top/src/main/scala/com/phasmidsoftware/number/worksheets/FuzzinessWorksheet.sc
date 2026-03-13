// FuzzinessWorksheet.sc

import com.phasmidsoftware.number.core.numerical.Number
import com.phasmidsoftware.number.core.numerical.Number.FuzzOps
import Number.NumberIsFractional.given
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.expression.expr.Expression

val myPi = 3.1415927 ~ 12
myPi.render // Expect "3.1415927(12)"

val alpha = Number("0.0072973525693(11)")
alpha.render // Expect "0.0072973525693(11)"

(myPi * alpha).show // Expect "0.0229253095(87)"

val g: Expression = Number("9.81*")
val t: Expression = Number("16.5*")

(g * t).show // Expect "161.865±0.35%"

val gTsquared = g * t * t
gTsquared.show // Expect "2670.7725±0.31%"

val twoPi = Number.twoPi
val twoPiAllSquared = twoPi * twoPi
twoPiAllSquared.show

// The length of the pendulum that swings with period 16.5 seconds
(gTsquared / twoPiAllSquared).show

// Now, let's look at adding and subtracting two fuzzy numbers.
val alphaEstimate = Number("0.007297352569(65)")

val alphaError = alpha - alphaEstimate
alphaError.show

// Can we reasonably say that our estimate is correct?
alphaError.isProbablyZero() // Expect true

// What about numbers that have Box-type fuzziness?
// proton-electron mass ratio
val ratio = Number(1836.15267343)
val ratioEstimate = Number(1836.1526734)

// This is supposed to generate a fuzzy number with Trapezoid-type fuzziness.
val ratioError = ratio - ratioEstimate
ratioError.show
ratioError.isProbablyZero()