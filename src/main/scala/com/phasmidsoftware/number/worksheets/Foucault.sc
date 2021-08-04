import com.phasmidsoftware.number.core.{Expression, Number}

val g = Number("9.81*")
val t = Number("16.5(3)")
val oneOver2pi = Number(0.159154943091895)
val tScaled: Expression = t * oneOver2pi
val square: Expression = tScaled ^ 2
val length = g * square
val xLength = length.asNumber

