/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  */

import com.phasmidsoftware.number.core.expression.{Expression, One}
import com.phasmidsoftware.number.core.numerical.{Constants, Number}
import com.phasmidsoftware.number.expression.expr.∅

val six = ∅ + 1 + 2 + 3
six.materialize.render

val fortyTwo = ∅ * 6 * 7
fortyTwo.materialize.render

val root3 = Expression(3).sqrt
val root3PlusOne: Expression = root3 + Expression.one
val root3MinusOne = root3 + Expression(Constants.minusOne)

// Note the use of an implicit converter from Expression to Number.
val x: Number = root3PlusOne
val y: Number = root3MinusOne

// Two should be exactly 2
val two: Number = root3PlusOne * root3MinusOne

val half: Expression = One / 2
// This should be rendered as ½
half.materialize
