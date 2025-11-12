import com.phasmidsoftware.number.core.Number._
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.{Constants, Number, Real}

val g = Expression(Real("9.81*"))
val t = Expression(Real("16.5*"))
val expression = g * ((t / Constants.twoPi) ∧ 2)

// NOTE the length should be 67.65(25)
val length: Number = expression
