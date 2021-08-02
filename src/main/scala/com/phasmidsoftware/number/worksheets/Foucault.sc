import com.phasmidsoftware.number.core.{Expression, Number}

val g = Number("9.81*")
g.fuzz
val t = Number("16.5(2)")
t.fuzz
val tScaled: Expression = t / Number.twoPi
tScaled.asNumber.get.fuzz
val square: Expression = tScaled ^ 2
square.asNumber.get.fuzz
val length = g * square
val xLength = length.asNumber

