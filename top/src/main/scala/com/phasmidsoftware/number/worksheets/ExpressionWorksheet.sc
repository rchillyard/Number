/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  */

import com.phasmidsoftware.number.core.expression.{Expression, One}
import com.phasmidsoftware.number.core.{Constants, Number}

val root3 = Expression(3).sqrt

// For some reason I haven't quite figure out, the "+" operator is masked by the implicits mechanism and can't be used.
// "plus" is a synonym.
val root3PlusOne = root3 plus Expression.one
val root3MinusOne = root3 plus Expression(Constants.minusOne)

// Note the use of an implicit converter from Expression to Number.
val x: Number = root3PlusOne
val y: Number = root3MinusOne

// Two should be exactly 2
val two: Number = root3PlusOne * root3MinusOne

val half: Expression = One / 2
// This should be rendered as ½
half.materialize
