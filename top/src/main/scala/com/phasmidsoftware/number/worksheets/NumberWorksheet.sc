/**
  * This is an example worksheet for Number in the core module.
  * NOTE that some of the types are now deprecated for application use -- they're designed for internal use.
  */

import com.phasmidsoftware.number.core.expression.{BiFunction, Literal, Power}
import com.phasmidsoftware.number.core.inner.Rational.RationalOps
import com.phasmidsoftware.number.core.inner.{AnyContext, PureNumber, Rational, RestrictedContext}
import com.phasmidsoftware.number.core.numerical.Number.√
import com.phasmidsoftware.number.core.numerical.{Field, Fuzzy, Number, Real}

val three: Number = 2 + Number.one // should be GeneralNumber 3

// NOTE see the corresponding expression in Expression.sc
val root3 = Real(√(3))
val two = (root3 add Real(Number.one)) multiply (root3 subtract Real(1))

val ok = {
  implicitly[Fuzzy[Field]].same(0.8)(two, two)
}

val half = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number.
val piBy4 = Number.pi doDivide 4
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize

val biFunction = BiFunction(Literal(3), Literal(Rational(1, 2)), Power)
biFunction.evaluate(AnyContext)