// Latex parsing and rendering

import com.phasmidsoftware.number.algebra.eager.{Eager, InversePower, QuadraticSolution, RationalNumber}
import com.phasmidsoftware.number.algebra.util.LatexRenderer.*
import com.phasmidsoftware.number.expression.expr.∅

val phi = Eager.phi
val psi = Eager.psi

phi.toLatex
psi.toLatex

val one = (∅ + phi :+ psi).materialize
one.toLatex

val base = RationalNumber.half
val offset = InversePower(2, 2)
val solution = QuadraticSolution(base, offset, 0, imaginary = false)
solution.toLatex // should be "\frac{1}{2} + \sqrt{2}"

