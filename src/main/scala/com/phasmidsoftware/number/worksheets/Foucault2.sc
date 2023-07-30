import com.phasmidsoftware.number.core.{Expression, Field, Number, Real}

val g = Expression(Real("9.81*"))
val t = Expression(Real("16.487(41)"))
val expression = g * ((t / Number.twoPi) ^ 2)
val length: Field = expression.materialize
