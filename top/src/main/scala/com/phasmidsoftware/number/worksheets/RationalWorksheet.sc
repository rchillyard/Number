/**
  * This is an example worksheet for Rational.
  * It shows different ways to define a Rational number,
  * including the :/ operator, as well as the r-interpolator.
  */

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational._

val two: Rational = 1 + 1
two.render// should be Rational 2

val three = 2 + Rational.one
three.render // should be Rational 3

val tenOverThree = 10 :/ 3
tenOverThree.render // should be 3.<3> indicating a recurrence

// Rationals are good at ensuring no loss of (apparent) precision. For example...
val good: Rational = (6 :/ 10) / 3
good.render // should be 0.2, NOT 0.19999999999999998

// Whereas, if we just use Double precision arithmetic, this happens...
val notSoGood: Double = 6.0 / 10.0 / 3 // will likely be rendered as 0.19999999999999998

val infinity = 1 :/ 0
infinity.render // should be +ve infinity

val twoThirds = 2 :/ 3
twoThirds.render // should be 0.<6>
val twoThirds = r"2/3"

val oneSixth = 1 :/ 6
oneSixth.render // should be 0.1<6>
val oneSixth = r"1/6"
val oneSixth = Rational.one / 6

val threeFourths = 3 :/ 4
threeFourths.render // should be 0.75
val threeFourths = r"3/4"

val fourFifths = 4 :/ 5
fourFifths.render // should be 0.8
val fourFifths = r"4/5"

val sixSevenths = 6 :/ 7
sixSevenths.render // should be 0.<857142>
val sixSevenths = r"6/7"

val tenElevenths = 10 :/ 11
tenElevenths.render // should be 0.<90>
val tenElevenths = r"10/11"

val twelveThirteenths = 12 :/ 13
twelveThirteenths.render // should be 0.<923076>
val twelveThirteenths = r"12/13"

// The following does not find a repeating sequence because we only generate 1000 characters in a rational string
val z = 7918 :/ 7919
z.render // should be 7918/7919
val z = r"7918/7919"