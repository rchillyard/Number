/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{one, two}
import com.phasmidsoftware.number.core.RQR.goldenRatioEquation
import com.phasmidsoftware.number.core.Real.NaN
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, SquareRoot, Value}
import java.util.Objects


/**
  * Represents a specialized form of a solution derived from an equation, specifically
  * associated with a quadratic root representation (RQR). This case class extends the
  * `Solution` class and provides operations for algebraic and numerical transformations
  * of the solution, along with utilities to obtain properties like branch, sign, and
  * whether the solution is real or imaginary.
  *
  * @param equation An instance of the `Equation` class that encapsulates the quadratic equation.
  * @param pos      A boolean value indicating a position or branch in the solution space.
  */
case class Solution_RQR(equation: Equation, pos: Boolean) extends Solution() {

  /**
    * Represents the computed result of casting the `equation` field to the specific type `RQR`.
    * This value is derived by performing a type-casting operation on the `equation` property.
    */
  val rqr: RQR = equation.asInstanceOf[RQR]

  /**
    * Determines the branch index based on the state of the `pos` condition.
    * If `pos` is true, this method returns 0; otherwise, it returns 1.
    *
    * @return the branch index as an `Int`: 0 if `pos` is true, 1 otherwise.
    */
  def branch: Int = if (pos) 0 else 1

  /**
    * Returns an optional name for the current `Solution_RQR` instance based on its
    * state and branch index.
    *
    * This method determines the branch index by evaluating the boolean condition `pos`.
    * If `pos` is true, the branch index is 0; otherwise, it is 1. The resulting branch
    * index is passed to the `maybeName` method of the `rqr` field to retrieve the
    * associated name.
    *
    * @return an `Option[String]` containing the symbolic name for the corresponding
    *         branch, or `None` if no name is applicable.
    */
  override def maybeName: Option[String] = rqr.maybeName(if (pos) 0 else 1)

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse toString

  /**
    * A lazy value representing a tuple of three components that contribute
    * to the quadratic root representation:
    *
    * 1. The first element is a `Field` obtained by dividing the negation of `p` by 2.
    * 2. The second element is a `Rational` value that is either +1/2 or -1/2, depending on the
    * sign of `pos`.
    * 3. The third element is the square root of the discriminant treated as a `Field`.
    */
  lazy val value: (Field, Rational, Field) =
    (
        Real(Number(-rqr.p) doDivide two),
        if (pos) Rational.half else -Rational.half,
        Real(Number(rqr.discriminant).sqrt)
    )

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * TESTME
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int = normalize.signum

  /**
    * Computes the absolute value of this `Solution_RQR` instance.
    * If the sign of the instance is negative, it scales the instance by -1 to return a non-negative value.
    * Otherwise, it returns the instance itself.
    *
    * @return a `Numerical` instance representing the absolute value of this instance.
    */
  def abs: Numerical =
    if (signum < 0) scale(-1) else this

  /**
    * Scales the current `Solution_RQR` instance by a given `Rational` value.
    * TESTME
    *
    * @param x the scaling factor represented as a `Rational`.
    * @return a new `Solution_RQR` instance with scaled values of `p` and `q`.
    */
  def scale(x: Rational): Solution =
    copy(equation = rqr.scale(x))

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case r@Solution_RQR(_, _, _) =>
      val (iBase, iz, iSqrt) = value
      val (oBase, oz, oSqrt) = r.value
      val crossTerms =
        if (oSqrt == iSqrt && iBase == oBase)
          (iz + oz) * oSqrt * oBase
        else
          iz * oSqrt * iBase + oz * iSqrt * oBase
      (iBase multiply oBase) + crossTerms + iz * oz * (iSqrt multiply oSqrt)
    case r@Real(ExactNumber(_, _)) =>
      val (iBase, iz, iSqrt) = value
      iBase * r + iz * iSqrt * r
  }

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = x match {
    case Real(ExactNumber(n, PureNumber)) if Value.maybeRational(n).contains(rqr.p) =>
      copy(equation = rqr.transform((p, _) => p + 2, (p, q) => 1 + p + q))
    case Real(ExactNumber(n, PureNumber)) if Value.maybeRational(n).contains(-rqr.p) =>
      copy(equation = rqr.transform((p, _) => p - 2, (p, q) => 1 - p + q))
    case _ =>
      asNumber map (_ add x) getOrElse NaN
  }

  /**
    * Raises this Field to the power of the specified number.
    *
    * TESTME
    *
    * @param p the exponent, provided as a Number.
    * @return the result of raising this Field to the power p.
    */
  def power(p: Number): Field = p match {
    case ExactNumber(n, PureNumber) =>
      Value.maybeInt(n) match {
        case Some(-1) =>
          invert
        case Some(0) =>
          Real(one) // CONSIDER What about Solution_RQR(name, 0, 0, pos)?
        case Some(1) =>
          this
        case Some(2) =>
          square
        case Some(k) if k > 0 =>
          (1 to k).foldLeft[Field](Real(one))((a, _) => a multiply this)
      }
    case _ =>
      ???
  }

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Solution =
    copy(equation = rqr.transform((p, q) => p / q, (_, q) => q.invert))

  /**
    * Method to determine if this Numerical is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical.
    * @return true if they are most probably the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean = x match {
    case s: Solution =>
      val (a, b, c) = value
      val (d, e, f) = s.value
      a == d && b == e && c == f
    case _ =>
      asNumber match {
        case Some(n) =>
          n.isSame(x)
        case None =>
          false
      }
  }

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean = rqr.discriminant >= 0

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean = !isReal && rqr.p.isZero

  /**
    * Computes the square of the current `Solution` by transforming its associated equation and toggling its position.
    *
    * @return a new `Solution` instance with the squared transformation applied.
    */
  def square: Solution =
    copy(equation = rqr.transform((p, q) => 2 * q - (p ∧ 2), (_, q) => q ∧ 2))

  /**
    * Negates the current `Solution` by scaling it with a factor of -1.
    *
    * @return a new `Solution` instance representing the negation of the current instance.
    */
  def negate: Solution =
    scale(-1)

  /**
    * Converts this quadratic root expression into an optional `Number` if possible.
    * The result depends on the discriminant:
    * - If the discriminant is negative, no numeric representation exists, and `None` is returned.
    * - If the discriminant is zero, it represents a double root, and a single exact `Number` instance is returned.
    * - If the discriminant is positive, a (fuzzy) `Number` is calculated and returned.
    *
    * @return an `Option[Number]` representing the numeric equivalent of this quadratic root if calculable; otherwise, `None`.
    */
  def asNumber: Option[Number] = {
    if (rqr.discriminant < 0) None
    else if (rqr.discriminant.isZero) Some(Number(rqr.p.halve))
    else Some(Number(rqr.discriminant, SquareRoot).negateConditional(!pos) doSubtract Number(rqr.p) doDivide Number.two)
  }

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field = {
    val (base, branch, squareRoot) = value
    val offset = branch * squareRoot
    base + offset
  }

  /**
    * For now, we consider a Solution_RQR to be exact, even though we can't write it out exactly.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = {
    val (base, r, offset) = value
    base.isExact && offset.isExact && r.signum >= 1
  }

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of Solution_RQR; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[Solution_RQR]

  /**
    * Compares this `Solution_RQR` instance to another object for equality.
    * The comparison checks if the other object is of the same type and has equivalent values
    * for the properties of `p`, `q`, and `pos`.
    *
    * @param other the object to compare with this instance for equality.
    * @return `true` if the provided object is a `Solution_RQR` and has the same values
    *         for the relevant properties; otherwise, `false`.
    */
  override def equals(other: Any): Boolean = other match {
    case that: Solution_RQR =>
      that.canEqual(this) &&
          equation == that.equation &&
          pos == that.pos
    case _ =>
      false
  }

  /**
    * Computes the hash code for this instance based on its state.
    *
    * This method takes into account the values of the fields `p`, `q`, and `pos`,
    * ensuring that the hash code reflects the identity and equality of this object.
    *
    * @return an `Int` representing the hash code of this instance.
    */
  override def hashCode(): Int = {
    Objects.hash(equation, pos)
  }

  override def toString: String = {
    val (b, c, o) = value
    s"$b ${rqr.termSign(c)} * ${rqr.termSign(o, add = false)}"
  }
}

/**
  * Represents a reduced quadratic equation of the form `x² + p*x + q = 0`, where `p` and `q` are rational coefficients.
  * Instances of this class enforce specific constraints on the equation, such as ensuring non-complex solutions
  * and non-zero coefficients, as outlined in the constructor.
  *
  * @constructor Creates an instance of the reduced quadratic equation with coefficients `p` and `q`.
  *              It enforces conditions to ensure the validity of the equation.
  * @param p the coefficient of the linear term (`x`).
  * @param q the constant term of the equation.
  */
case class RQR(p: Rational, q: Rational) extends Equation {
  require(!p.isZero || !q.isZero, "may not have p and q equal to zero")
  // TODO remove the following restriction--there's no reason why we cannot have complex solutions
  require(discriminant >= 0, s"discriminant ($discriminant) must not be negative--this equation is $this")

  /**
    * Determines the name associated with the given branch index, if applicable.
    * The method evaluates specific conditions and returns a symbolic name for
    * branches that match these conditions.
    *
    * @param branch the branch index for which the name is being determined.
    *               Valid indices are typically 0 and 1; other indices will return None.
    * @return an `Option[String]` containing the name of the branch as a string if
    *         a name can be determined, `None` otherwise.
    */
  def maybeName(branch: Int): Option[String] =
    if (p == Rational.negOne && q == Rational.negOne)
      branch match {
        case 0 => Some("\uD835\uDED7")
        case 1 => Some("\uD835\uDED9")
        case _ => None
      }
    else
      None

  /**
    * Attempts to find a solution for a mathematical equation corresponding to the given branch.
    * Solutions are represented as an instance of the `Solution` class.
    * If appropriate, a `Solution` can be converted into Complex form.
    *
    * @param branch the branch index for which the solution is being sought.
    *               The branch index identifies specific solutions for equations that may have multiple solutions.
    * @return an `Option[Solution]`, where `Some(solution)` contains the solution for the specified branch,
    *         or `None` if no solution exists for the given branch.
    */
  def solve(branch: Int): Option[Solution] =
    if (branch >= 0 && branch < 2)
      Some(Solution_RQR(this, branch == 0))
    else
      None

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
  def invert: Equation =
    scale(q.invert)

  /**
    * Transforms the current instance of `RQR` by applying provided functions
    * to the `p` and `q` components.
    *
    * @param fP a function that transforms the `p` component.
    * @param fQ a function that transforms the `q` component.
    * @return a new `RQR` instance with `p` transformed using `fP` and `q` transformed using `fQ`.
    */
  def transform(fP: (Rational, Rational) => Rational, fQ: (Rational, Rational) => Rational): RQR =
    copy(p = fP(p, q), q = fQ(p, q))

  /**
    * Computes the discriminant of a quadratic equation based on the formula `p^2 - 4 * q`.
    * The discriminant is a critical component in determining the nature of the roots of a
    * quadratic equation.
    * If the discriminant is positive then there are two distinct roots;
    * if negative, then there are no real roots;
    * if zero, then there's a double root.
    *
    * @return an `Int` representing the discriminant.
    */
  def discriminant: Rational =
    p * p - 4 * q

  override def toString: String = s"Reduced Quadratic Equation: x^2 ${termSign(p)}x ${termSign(q)} = 0"

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of Solution_RQR; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[RQR]

  /**
    * Scales the equation by a given rational factor.
    * We are essentially transforming the equation into a new equation with coefficients `x * p` and `x^2 * q`.
    *
    * The equation is represented by coefficients `p` and `q`, and scaling is applied as:
    * - `p` is multiplied by `x`
    * - `q` is multiplied by `x^2`
    *
    * @param x the scaling factor as a `Rational`.
    * @return a new `Equation` resulting from scaling the current equation by the factor `x`.
    */
  def scale(x: Rational): Equation =
    copy(p = x * p, q = x * x * q)

  /**
    * Generates a string representation of the sign and the absolute value of the given rational number.
    * The sign is determined based on the value of the input: a negative sign ("-") for negative numbers
    * and a positive sign ("+") for non-negative numbers. The absolute value is appended to the sign.
    *
    * @param x the rational number whose sign and absolute value are to be rendered as a string
    * @return a string in the format "+ n" or "- n", where "n" represents the absolute value of the input
    */
  def termSign(x: Field, add: Boolean = true): String = (if (add) if (x.signum < 0) "- " else "+ " else "") + x.abs.render
}

object RQR {
  val goldenRatioEquation = new RQR(Rational.negOne, Rational.negOne)
}

/**
  * Companion object for the `Solution_RQR` class.
  * Provides an unapply method for pattern matching.
  */
object Solution_RQR {
  def unapply(rqr: Solution_RQR): Option[(Option[String], Equation, Boolean)] =
    Some(rqr.maybeName, rqr.equation, rqr.pos)

  val phi = new Solution_RQR(goldenRatioEquation, true)
  val psi = new Solution_RQR(goldenRatioEquation, false)
}
