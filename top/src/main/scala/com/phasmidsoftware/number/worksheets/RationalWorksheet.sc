/**
  * This is an example worksheet for Rational.
  * It shows different ways to define a Rational number,
  * including the :/ operator, as well as the r-interpolator.
  */

import com.phasmidsoftware.number.algebra.util.LatexRenderer as LatexRendererOps
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.*

val two: Rational = 1 + 1
two.render// should be Rational 2

import com.phasmidsoftware.number.algebra.eager.RationalNumber.given
import com.phasmidsoftware.number.algebra.util.LatexRenderer.{LatexRendererOps, nthRoot}
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}

two.toLatex

val three = 2 + Rational.one
three.render // should be Rational 3
three.toLatex

val tenOverThree = 10 :/ 3
tenOverThree.render // should be 3.<3> indicating a recurrence
tenOverThree.toLatex

// Rationals are good at ensuring no loss of (apparent) precision. For example...
val good: Rational = (6 :/ 10) / 3
good.render // should be ⅕, NOT 0.19999999999999998
good.toLatex

// Whereas, if we just use Double precision arithmetic, this happens...
val notSoGood: Double = 6.0 / 10.0 / 3 // will likely be rendered as 0.19999999999999998

val infinity = 0.invert
infinity.render // should be +ve infinity
infinity.toLatex

val twoThirds = 2 :/ 3
twoThirds.render // should be ⅔
val twoThirdsAlt = r"2/3"
twoThirdsAlt.toLatex

val oneSixth = 6.invert
oneSixth.render // should be ⅙
val oneSixthAlt1 = r"1/6"
val oneSixthAlt2 = Rational.one / 6
oneSixth.toLatex

val threeFourths = 3 :/ 4
threeFourths.render // should be ¾
val threeFourthsAlt = r"3/4"
threeFourths.toLatex

val fourFifths = 4 :/ 5
fourFifths.render // should be ⅘
val fourFifthsAlt = r"4/5"
fourFifths.toLatex

val sixSevenths = 6 :/ 7
sixSevenths.render // should be 0.<857142>
val sixSeventhsAlt = r"6/7"
sixSevenths.toLatex

val tenElevenths = 10 :/ 11
tenElevenths.render // should be 0.<90>
val tenEleventhsAlt = r"10/11"
tenElevenths.toLatex

val twelveThirteenths = 12 :/ 13
twelveThirteenths.render // should be 0.<923076>
val twelveThirteenthsAlt = r"12/13"
twelveThirteenths.toLatex

// The following does not find a repeating sequence because we only generate 1000 characters in a rational string
val z = 7918 :/ 7919
z.render // should be 7918/7919
val zAlt = r"7918/7919"
z.toLatex