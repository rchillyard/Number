import com.phasmidsoftware.number.core.{Expression, Field, Number, Real}

val g = Expression(Real("9.81*"))
val t = Expression(Real("16.5*"))
val tDividedBy2Pi = t / Number.twoPi
val tBy2PiSquared = tDividedBy2Pi ^ 2
val expression = g * tBy2PiSquared

// NOTE the length should be 67.65(25)
val length: Field = expression.materialize
