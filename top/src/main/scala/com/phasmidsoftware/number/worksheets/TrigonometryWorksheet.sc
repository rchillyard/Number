/**
  * This worksheet demonstrates some of the trigonometric expressions and identities.
  */

import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.expression.expr.{E, I, Pi}

val iPi = I * Pi
val euler = E ∧ iPi
euler.materialize.render

val piBy2 = Pi / 2
piBy2.cos
piBy2.sin
piBy2.tan

// You should see
val p: String = (piBy2 / 2).cos.materialize
val q: String = (piBy2 / 3).cos.materialize
val x: String = (Pi / 3).sin.materialize
