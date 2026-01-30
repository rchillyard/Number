import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.expression.expr.{ConstI, E, Pi}

/**
  * This worksheet demonstrates some of the trigonometric expressions and identities.
  */

val iPi = ConstI * Pi
val euler = E ∧ iPi
euler.asNumber

val piBy2 = Pi / 2  
piBy2.cos
piBy2.sin
piBy2.tan

val p: Eager = (piBy2 / 2).cos.materialize
val q: Eager = (piBy2 / 3).cos.materialize
val x: Eager = (Pi / 3).sin.materialize
