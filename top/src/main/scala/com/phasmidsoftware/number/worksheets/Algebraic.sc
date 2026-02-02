/**
  * This worksheet illustrates the use of Algebraic fields
  */

import com.phasmidsoftware.number.algebra.eager.Eager.half
import com.phasmidsoftware.number.algebra.eager.{Eager, QuadraticSolution, Solution}
import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import com.phasmidsoftware.number.expression.expr.Root

// phi, the Golden Ratio
val phi = Root.phi
val psi = Root.psi

Root.phi

val phiPlus1 = phi + 1

val phiSquared = (phi * phi).simplify

// an approximation to phi
val phiApprox = phi.fuzzy

// psi, the conjugate of Phi
val conjugate = psi

// an approximation to psi
val psiSolution = psi.fuzzy

// the sum of phi and psi should be 1
val sum = phi add psi

// the product of phi and psi should be -1
val product = phi * psi

// the equation that defines phi: x∧2 + px + qx = 0 where p = q = -1
val goldenRationEquation = phi.equation

// The value of the product should be -r from the equation
val productString = product.materialize

val phiSquareApprox = phiSquared.fuzzy

// We define the Algebraic for the golden ratio from two numbers
val myPhi: Solution = QuadraticEquation.goldenRatioEquation.solve(0)
myPhi
myPhi match {
  case q: QuadraticSolution =>
    q
  case _ =>
    "not a QuadraticSolution"
}
