/**
  * This is an example worksheet for Number in the core module.
  * NOTE that some of the types are now deprecated for application use -- they're designed for internal use.
  */

import cats.implicits.*
import com.phasmidsoftware.number.algebra.core.AnyContext
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalOps
import com.phasmidsoftware.number.expression.expr.*

val root3 = Root.√(3)
val two = (root3 + 1) * (root3 - 1)
val half = 1 :/ 2

val infinity = 1 :/ 0 // should be infinity

// NOTE Demonstrate that sin(π/4) is an exact number.
val piBy4 = Pi / 4
piBy4.show
val sinePiBy4 = piBy4.sin
val oneHalf = (sinePiBy4 * sinePiBy4)
oneHalf.show

val biFunction = BiFunction(Literal(3), Literal(Rational(1, 2)), Power)
biFunction.evaluate(AnyContext)