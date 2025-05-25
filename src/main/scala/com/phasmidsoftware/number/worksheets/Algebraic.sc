import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic.{phi, psi}/**
  * This worksheet illustrates the use of Algebraic fields
  */

// phi, the Golden Ratio
phi.render

// an approximation to phi
phi.solve.asField

// psi, the conjugate of Phi
psi.render

// an approximation to psi
psi.solve.asField

// the product of phi and psi
val product = phi * psi

// the equation that defines phi: x^2 + px + qx = 0 where p = q = -1
phi.equation

// The value of the product should be -q from the equation
product.render

// phi^2 should be the same thing as phi + 1
val phiSquared = phi.square

phiSquared.solve.asField


