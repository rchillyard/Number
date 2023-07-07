/**
  * This is an example worksheet for Rational.
  */

import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.number.core.Rational._

val two: Rational = 1 + 1 // should be Rational 2

val three = 2 + Rational.one // should be Rational 3

val tenOverThree = 10 :/ 3 // should be 3.<3> indicating a recurrence

val ten = tenOverThree * 3 // should be Rational 10, NOT 9.999999999999

val notSoGood = (10.0 / 3) * 3 // will likely be rendered as 9.999999999999

val infinity = 1 :/ 0 // should be +ve infinity

val twoThirds = 2 :/ 3 // should be 0.<6>

val threeFourths = 3 :/ 4 // should be 0.75

val fourFifths = 4 :/ 5 // should be 0.8 (but for now it is 4/5)

val sixSevenths = 6 :/ 7 // should be 0.<857142>

val tenElevenths = 10 :/ 11 // should be 0.<90>

val twelveThirteenths = 12 :/ 13 // should be 0.<923076>
