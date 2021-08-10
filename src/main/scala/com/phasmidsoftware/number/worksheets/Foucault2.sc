import com.phasmidsoftware.number.core.Number
import com.phasmidsoftware.number.core.Number._

val g = Number("9.81*")
val t = Number("16.487(41)")
val expression = g * ((t / twoPi) ^ 2)
val length: Number = expression
