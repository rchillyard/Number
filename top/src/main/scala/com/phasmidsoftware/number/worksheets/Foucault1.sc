import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.expression.expr.{ConstPi, Expression}
import com.phasmidsoftware.number.parse.ExpressionParser.lazymath

val g: Expression = Eager("9.81*")
val t: Expression = Eager("16.5*")
val expression = g * ((t / ConstPi / 2) ∧ 2)

// NOTE the length should be 67.65(25)
// Do we have a way to force the fuzziness to be Gaussian? I think we do.
val length: Eager = expression.materialize
length.render
