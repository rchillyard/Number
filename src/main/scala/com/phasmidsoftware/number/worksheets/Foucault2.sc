import com.phasmidsoftware.number.core.Number._
import com.phasmidsoftware.number.core.{Expression, Number}

val g = Expression(Number("9.81*"))
val t = Expression(Number("16.487(41)"))
val expression = g * ((t / twoPi) ^ 2)
val length: Number = expression
