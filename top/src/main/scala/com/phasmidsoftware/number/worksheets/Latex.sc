// Latex parsing and rendering

import com.phasmidsoftware.number.algebra.eager.{Eager, InversePower, QuadraticSolution, RationalNumber}
import com.phasmidsoftware.number.algebra.util.LatexRenderer.*
import com.phasmidsoftware.number.expression.expr.∅
import com.phasmidsoftware.number.parse.ExpressionParser.{lazymath, puremath}

// These strings are in Latex format.
// You can use the Unicode or the escape mechanism
val phi = lazymath"𝛗"
val psi = lazymath"""\psi"""

phi.toLatex
psi.toLatex

val phiSquared = puremath"$phi^2"

phiSquared.toLatex

val one = (phi :+ psi).materialize
one.toLatex

val base = RationalNumber.half
val offset = InversePower(2, 2)
val solution = QuadraticSolution(base, offset, 0, imaginary = false)
solution.toLatex // should be "\frac{1}{2} + \sqrt{2}"

val phi1 = lazymath"\frac{1+\sqrt{5}}{2}"
phi1.toLatex
phi1.materialize.render