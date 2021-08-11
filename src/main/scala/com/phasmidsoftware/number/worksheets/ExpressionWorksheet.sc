/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  *
  */

import com.phasmidsoftware.number.core.Number.{negate, one}
import com.phasmidsoftware.number.core.{Expression, Literal, Number}

val root3: Expression = Literal(3).sqrt

val root3PlusOne = root3 plus Expression.one
val root3MinusOne = root3 plus Expression(negate(one))

val x: Number = root3PlusOne
val y: Number = root3MinusOne
val two: Number = root3PlusOne * root3MinusOne
