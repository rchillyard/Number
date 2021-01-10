/**
  * This is an example worksheet for Rational.
  */

import com.phasmidsoftware.number.core.Rational
import com.phasmidsoftware.number.core.Rational._

val two: Rational = 1 + 1 // should be Rational 2

val three = 2 + Rational.one // should be Rational 3

val tenOverThree = 10 :/ 3 // should be Rational 10/3

val ten = tenOverThree * 3 // should be Rational 10, not 9.999999999999

val notSoGood = (10.0 / 3) * 3 // will most probably be 9.999999999999

val infinity = 1 :/ 0 // should be +ve infinity
