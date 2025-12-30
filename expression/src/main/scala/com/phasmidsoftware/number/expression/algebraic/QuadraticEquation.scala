/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.algebraic

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.eager.RationalNumber.convRationalRationalNumber
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Constants.sPhi
import com.phasmidsoftware.number.core.numerical.Field
import com.phasmidsoftware.number.expression.expr.{Expression, Literal}

/**
  * Represents a (monic) equation of the form `x² + p*x + q = 0`, where `p` and `q` are rational coefficients.
  * A graph of this equation appears as follows: an upright parabola
  * (where p and q are both positive and (½p)⋏2 > q):
  * <pre>
  *          x                        |              x
  *           x                       |             x
  *             x                     |            x
  *                x              > ½p <         x
  *       -------------x----------+---|-------x------------------ v
  *                         x         |  x                        q
  *       - - - - - - - - - - - - x - + - - - - - - - - - - - - - ⋏
  *                                   |
  *</pre>
  * The roots of the equation (if such exist) are at `-w ± √(w∧2-q)` where `w = ½p`
  * For more information regarding quadratic equations, see:
  * [[https://en.wikipedia.org/wiki/Quadratic_equation]]
  *
  * @constructor Creates an instance of the equation with coefficients `p` and `q`.
  * @param p the coefficient of the linear term (`x`).
  * @param q the constant term of the equation.
  */
case class QuadraticEquation(p: Rational, q: Rational) extends Equation {
  /**
    * Retrieves the branch at the specified index.
    *
    * @param index the index of the branch to retrieve, where the index starts from 0.
    * @return the element of type `T` at the specified branch index.
    *         If the index is out of range, the behavior is implementation-specific.
    */
  def branched(index: Int): Expression = Literal(solve(index))

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` class.
    * If appropriate, an `Algebraic` can be converted into Complex form.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    *
    * @return an `Option[Algebraic]`, where `Some(solution)` contains the solution for the specified branch,
    *         or `None` if no solution exists for the given branch.
    */
  def solve(branch: Int): Solution = {
    require(branch == 0 || branch == 1, "Quadratic has only 2 roots")

    if (discriminant == Rational.zero)
      // Repeated roots
      QuadraticSolution(RationalNumber(-p / 2), RationalNumber.zero, branch, false)
    else if (discriminant >= Rational.zero)
      // Real roots
      QuadraticSolution(RationalNumber(-p / 2), squareRoot(discriminant / 4), branch, false)
    else
      // Complex roots
      QuadraticSolution(RationalNumber(-p / 2), squareRoot(-discriminant / 4), branch, true)
  }

  /**
    * Shifts the origin of the equation by transforming its `p` and `q` components
    * based on the provided transformation functions dependent on the parameter `c`.
    *
    * @param c the `Rational` value used to determine the shift applied to the equation.
    * @return a new `Quadratic` instance with its origin shifted according to the transformations.
    */
  def shiftOrigin(c: Rational): QuadraticEquation = transform((p, _) => p - 2 * c, (p, q) => c ∧ 2 - p * c + q)

  /**
    * Returns the number of branches for this instance.
    *
    * @return an `Int` representing the number of branches, which is always 2.
    */
  def branches: Int = 2

  /**
    * Yields the inverse of this Equation. CONSIDER Does this make sense??
    * CONSIDER should we make it an abstract method on Equation?
    */
  def invert: QuadraticEquation =
    transform((p, q) => p / q, (_, q) => q.invert)

  /**
    * Computes the sum of the conjugates for this instance of `Quadratic`.
    * The sum of conjugates is mathematically derived as the negation of the `p` coefficient
    * in a quadratic equation of the form `x∧2 + p*x + q = 0`.
    *
    * @return a `Rational` value representing the sum of conjugates.
    */
  def conjugateSum: Rational =
    p.negate

  /**
    * Computes the product of the conjugate roots of the quadratic equation represented by this instance.
    * The product of the conjugates is determined directly from the coefficients of the quadratic equation
    * and is equivalent to the constant term divided by the leading coefficient.
    *
    * This is a special case of Vieta's formula for the product of the roots of a polynomial.
    *
    * @return a `Rational` value representing the product of the conjugate roots of the quadratic equation.
    */
  def conjugateProduct: Rational =
    q

  /**
    * Transforms the current instance of `Quadratic` by applying provided functions
    * to the `p` and `q` components.
    *
    * @param fP a function that transforms the `p` component.
    * @param fQ a function that transforms the `q` component.
    * @return a new `Quadratic` instance with `p` transformed using `fP` and `q` transformed using `fQ`.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): QuadraticEquation =
    copy(p = fP(p, q), q = fQ(p, q))

  /**
    * Computes the discriminant of a quadratic equation based on the formula `p∧2 - 4 * q`.
    * The discriminant is a critical component in determining the nature of the roots of a
    * quadratic equation.
    * If the discriminant is positive, then there are two distinct roots;
    * if negative, then there are no real roots;
    * if zero, then there's a double root.
    *
    * @return an `Int` representing the discriminant.
    */
  lazy val discriminant: Rational =
    p * p - 4 * q

  override def toString: String = s"Quadratic Equation: x∧2 ${QuadraticEquation.termSign(p)}x ${QuadraticEquation.termSign(q)} = 0"

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of Algebraic_Quadratic; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[QuadraticEquation]

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * @param x the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  infix def *(x: Rational): Equation =
    copy(p = x * p, q = x * x * q)

  /**
    * Calculates the square root of a given rational number as a monotone function.
    *
    * @param r the rational number for which the square root is to be computed.
    * @return a monotone representation of the square root of the given rational number.
    */
  private def squareRoot(r: RationalNumber): Monotone =
    InversePower(2, r).normalize.asMonotone
}

/**
  * Represents a linear equation of the form `x + r = r` where `r` is a rational number.
  * The slope of the line representing the equation is one, therefore the intercepts are:
  * `x = -r` when `y = 0` (the solution/root); and
  * `y = r` when `x = 0`.
  * This class provides various operations and transformations specific to linear equations.
  */
case class LinearEquation(r: Rational) extends Equation {
  /**
    * Method to compute the number of branches related to a specific computation or process.
    * For example, a solution to a quadratic equation has two branches, one for the real part and one for the imaginary part.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int = 1

  /**
    * Retrieves the branch at the specified index.
    *
    * @param index the index of the branch to retrieve, where the index starts from 0.
    * @return the element of type `T` at the specified branch index.
    *         If the index is out of range, the behavior is implementation-specific.
    */
  def branched(index: Int): Expression = Literal(solve(index))

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Algebraic` or of `Complex` class.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    *
    * @return a `Field`, which is either an `Algebraic` (real-valued) or a `Complex`.
    */
  def solve(branch: Int): Solution =
    LinearSolution(RationalNumber(-r))()

  /**
    * Transforms the current equation by applying the provided functions to its components.
    *
    * @param fP a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the first component of the equation.
    *
    * @param fQ a function that takes two `Rational` parameters and produces a `Rational` result.
    *           It is applied to the second component of the equation.
    *
    * @return a new `Equation` instance that is the result of applying the specified transformations
    *         to the components of the current equation.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): Equation = ??? // TODO implement me

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * @param factor the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  infix def *(factor: Rational): Equation = copy(r = r * factor)

  /**
    * Produces an inverted version of the current equation.
    * The inversion process involves switching or rearranging key components
    * of the equation, depending on its mathematical structure.
    *
    * @return a new `Equation` instance representing the inverted form of the current equation.
    */
  def invert: Equation = ??? // TODO implement me

  /**
    * Shifts the origin of the given equation by the specified rational value.
    * This method adjusts the equation's reference point, effectively translating
    * it to a new position in its domain.
    *
    * Given this equation `P(x) = 0`, such that `z` is a solution,
    * this method returns the equation `P(y) = 0` such that `z + c` is a solution.
    *
    * CONSIDER a better name for this.
    *
    * @param c the rational value by which to shift the origin of the equation.
    * @return a new `Equation` instance with its origin shifted by the specified value.
    */
  def shiftOrigin(c: Rational): LinearEquation = ??? // TODO implement me
}

/**
  * The `Quadratic` object provides commonly used quadratic equations and utility methods related to them.
  */
object QuadraticEquation {

  /**
    * Creates a quadratic equation whose solutions are plus or minus the square root of the given `Rational` number.
    * The quadratic equation is defined as `Quadratic(0, -r)`, where the first term is zero
    * and the second term is the negation of the input parameter.
    *
    * @param r the `Rational` number used as the coefficient for the square root equation.
    * @return a `Quadratic` instance that represents the square root equation.
    */
  def squareRootEquation(r: Rational): QuadraticEquation = QuadraticEquation(Rational.zero, r.negate)

  /**
    * An approximation of the golden ratio (φ), which is an irrational number
    * often encountered in mathematics, art, and nature. Its value is approximately
    * 1.6180339887498948.
    */
  val phiApprox = Real(sPhi)

  /**
    * A predefined quadratic equation relating to the golden ratio.
    * The equation is represented as `x² + x - 1 = 0`.
    * The roots of this equation are the golden ratio (phi) and its conjugate (psi).
    */
  val goldenRatioEquation: QuadraticEquation = QuadraticEquation(Rational.negOne, Rational.negOne)

  /**
    * Represents the equation `x² - 2 = 0`, which defines the relationship for the square root of 2.
    * This specific quadratic equation has the following form:
    *
    * `p = 0` (no linear term) and `q = -2` (constant term).
    *
    * Geometrically, this equation corresponds to a parabola, and its roots are at ±√2.
    * The equation is a notable instance of a quadratic, useful in various mathematical contexts.
    */
  val rootTwoEquation: QuadraticEquation = squareRootEquation(Rational.two)

  /**
    * Formats the sign and absolute value of a given Field instance as a String.
    * The resulting string includes a sign ("+ " for positive or "- " for negative)
    * if the `add` parameter is true, followed by the absolute value of the Field
    * rendered as a String.
    *
    * @param x   the Field instance to be formatted.
    * @param add a Boolean flag indicating whether to prepend a sign ("+ " or "- ") to the result;
    *            defaults to true.
    *
    * @return a formatted String representation of the Field's sign and absolute value.
    */
  private def termSign(x: Field, add: Boolean = true): String = (if (add) if (x.signum < 0) "- " else "+ " else "") + x.abs.render
}
