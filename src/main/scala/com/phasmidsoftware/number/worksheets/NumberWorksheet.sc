/**
  * This is an example worksheet for Number.
  */

import com.phasmidsoftware.number.core.Number.{NumberOps, _}
import com.phasmidsoftware.number.core.{Field, Fuzzy, Number}

val three: Number = 2 + one // should be GeneralNumber 3

// NOTE see the corresponding expression in Expression.sc
val root3 = Number(3).sqrt
val two = (root3 add one) multiply (root3 add negate(one))

import com.phasmidsoftware.number.core.Field._

val ok = implicitly[Fuzzy[Field]].same(0.8)(two, Number.two)


val half = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number.
val piBy4 = Number.pi doDivide 4
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize

