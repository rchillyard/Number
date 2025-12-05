import com.phasmidsoftware.number.core.Constants.{e, i, pi, piBy2}
import com.phasmidsoftware.number.core.Number
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.expression.Expression.ExpressionOps

/**
  * This worksheet demonstrates some of the trigonometric expressions and identities.
  */

val iPi = Expression(i) * pi
val euler = Expression(e) ∧ iPi
euler.asNumber

piBy2.cos
piBy2.sin
piBy2.tan

val p: Number = (Expression(piBy2) / 2).cos
val q: Number = (Expression(piBy2) / 3).cos
val x: Number = (Expression(pi) / 3).sin
