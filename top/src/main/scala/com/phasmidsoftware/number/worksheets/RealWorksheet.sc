/**
  * This is an example worksheet for Real.
  * NOTE that Number is deprecated for application use -- it's designed for internal use.
  */

import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import com.phasmidsoftware.number.core.numerical.Constants.one
import com.phasmidsoftware.number.core.numerical.Real.RealOps
import com.phasmidsoftware.number.core.numerical.{Constants, Field, Fuzzy, Number, Real}

val three: Field = 2 + one


// NOTE see the corresponding expression in Expression.sc
val half: Field = Real(r"1/2")
val root3: Field = three power half
val twoFuzzy: Field = (root3 add one) multiply (root3 add -one)

val ok = implicitly[Fuzzy[Field]].same(0.8)(twoFuzzy, Constants.two)

val two: Real = 1 + one

val halfToo = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number, viz. 1 over root 2.
val piBy4 = Number.pi doDivide 4
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize

// Parsing from Strings: Exact
Real("1.0") // fewer than 15 decimal places
Real("0.57721566490153") // ditto

// Parsing from Strings: Fuzzy
Real("1.100*").render // ends in Asterisk
Real("1.100...").render // ends in Ellipsis
Real("1.1000[1]").render // has explicit (box) error bounds
Real("1.1000[5]").render // has explicit (box) error bounds
Real("1.1000(5)").render // has explicit (Gaussian) error bounds
Real("0.577215664901533").render // 15 or more decimal places
Real("1.<3>").render // 4/3

