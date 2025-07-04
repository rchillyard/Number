import com.phasmidsoftware.number.core.algebraic.Algebraic.{phi, psi}
import com.phasmidsoftware.number.core.algebraic.{Algebraic_Quadratic, QuadraticSolution}
import com.phasmidsoftware.number.core.{Constants, Real}/**
  * This worksheet illustrates the use of Algebraic fields
  */

// phi, the Golden Ratio
phi.render

val phiPlus1 = (phi add 1)

// an approximation to phi
val phiApprox = phi.solve.asField

// psi, the conjugate of Phi
val conjugate = psi.render

// an approximation to psi
val psiSolution = psi.solve.asField

// the sum of phi and psi should be 1
val sum = (phi add psi).asNumber

// the product of phi and psi should be -1
val product = phi * psi

// the equation that defines phi: x^2 + px + qx = 0 where p = q = -1
val goldenRationEquation = phi.equation

// The value of the product should be -q from the equation
val productString = product.render

// phi^2 should be the same thing as phi + 1
val phiSquared = phi.square

val phiSquareApprox = phiSquared.solve.asField

// We define the Algebraic for the golden ratio from two numbers
val maybeAlgebraic = for {
  base <- Constants.half.asNumber
  offset <- (Constants.root5 divide Real(2)).asNumber
  name <- Algebraic_Quadratic.apply(base, offset).maybeName
} yield name
maybeAlgebraic.get

// We can also define an Algebraic from a Solution.
val solution = phi.solve.asInstanceOf[QuadraticSolution]
val algebraic: Algebraic_Quadratic = Algebraic_Quadratic(solution)
algebraic.maybeName.get
