/**
  * This is an example worksheet for Real.
  * NOTE that Number is deprecated for application use -- it's designed for internal use.
  */

import com.phasmidsoftware.number.core.Rational.RationalHelper
import com.phasmidsoftware.number.core.Real.{RealOps, _}
import com.phasmidsoftware.number.core.{Field, Fuzzy, Number, Real}

val three: Field = 2 + one

// NOTE see the corresponding expression in Expression.sc
val half: Field = Real(Number(r"1/2"))
val root3: Field = three power half
val twoFuzzy: Field = (root3 add one) multiply (root3 add -one)

val ok = implicitly[Fuzzy[Field]].same(0.8)(twoFuzzy, Real.two)

val two: Real = 1 + Real.one

val halfToo = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number, viz. 1 over root 2.
val piBy4 = Number.pi doDivide 4
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize

