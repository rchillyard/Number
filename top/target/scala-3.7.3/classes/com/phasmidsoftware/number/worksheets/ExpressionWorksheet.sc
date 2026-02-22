/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  */

import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.core.numerical.{Constants, Number}
import com.phasmidsoftware.number.expression.expr.Expression.fromString
import com.phasmidsoftware.number.expression.expr.{Expression, One, ∅}

val six = ∅ + 1 + 2 + 3
six.materialize

val fortyTwo = ∅ * 6 * 7
fortyTwo.materialize

val root3 = Expression(3).sqrt
val root3PlusOne: Expression = root3 + Expression.one
val root3MinusOne = root3 + Expression(-1)

val x: Eager = root3PlusOne.materialize
val y: Eager = root3MinusOne.materialize

// Two should be exactly 2
val two: Expression = root3PlusOne * root3MinusOne

val half: Expression = One / 2
// This should be rendered as ½
half.materialize
