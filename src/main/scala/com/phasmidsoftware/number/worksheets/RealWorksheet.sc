/**
 * This is an example worksheet for Real.
 * NOTE that Number is deprecated for application use -- it's designed for internal use.
 */

import com.phasmidsoftware.number.core.Number.one
import com.phasmidsoftware.number.core.Rational.RationalHelper
import com.phasmidsoftware.number.core.Real.RealOps
import com.phasmidsoftware.number.core.{Field, Fuzzy, Number, Rational, Real}

val three: Number = 2 + one

// NOTE see the corresponding expression in Expression.sc
val half: Rational = r"1/2"
val root3: Field = three.doPower(Number(half))
val twoFuzzy: Field = (root3 add one) multiply (root3 add -one)

val ok = implicitly[Fuzzy[Field]].same(0.8)(twoFuzzy, Number.two)

val two: Number = 1 + one

val halfToo = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number, viz. 1 over root 2.
val piBy4 = Number.pi doDivide 4
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize

// Parsing from Strings: Exact
Real("1.1") // only one decimal place
Real("1.00") // ends in two zeroes
Real("1.0100") // ditto
Real("1.100") // ditto

// Parsing from Strings: Fuzzy
Real("1.010") // more than two decimal places
Real("1.100*") // ends in "*"
Real("1.100...") // ends in "..."
Real("1.1000[5]") // has explicit (box) error bounds
Real("1.1000(5)") // has explicit (Gaussian) error bounds

