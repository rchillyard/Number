/**
  * This worksheet illustrates the use of Expressions (for lazy evaluation).
  *
  */

import com.phasmidsoftware.number.core.Number.one
import com.phasmidsoftware.number.core.{Expression, Number, Real}

val root3 = Expression(3).sqrt

// For some reason I haven't quite figure out, the "+" operator is masked by the implicits mechanism and can't be used.
// "plus" is a synonym.
val root3PlusOne = root3 plus Expression.one
val root3MinusOne = root3 plus Expression(Real(one))

// Note the use of an implicit converter from Expression to Number.
val x: Number = root3PlusOne
val y: Number = root3MinusOne
val two: Number = root3PlusOne * root3MinusOne
