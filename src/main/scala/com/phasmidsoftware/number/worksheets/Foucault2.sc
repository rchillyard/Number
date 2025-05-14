import com.phasmidsoftware.number.core.Number._
import com.phasmidsoftware.number.core.{Constants, Number, Real}
import com.phasmidsoftware.number.expression.Expression

val g = Expression(Real("9.81*"))
val t = Expression(Real("16.487(41)"))
val expression = g * ((t / Constants.twoPi) ^ 2)
val length: Number = expression
