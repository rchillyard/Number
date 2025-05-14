/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{one, two}
import com.phasmidsoftware.number.core.Real.NaN
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, SquareRoot, Value}

/**
  * An abstract class representing a root of a reduced quadratic equation of the form `x^2 + px + q = 0`
  * The value of the root is `(-p/2) ± sqrt((-p/2)^2 - q)`.
  * The class extends `Solution,` which in turn extends `Field`.
  *
  * TODO rename this class as ReducedQuadraticRoot and remove or rename the other class with that name.
  *
  * @constructor Constructs a reduced quadratic root with parameters `p`, `q`, and a boolean `pos` to determine the sign of the root.
  * @param p The coefficient of x in the reduced quadratic equation.
  * @param q The constant term in the reduced quadratic equation.
  * @param pos Determines if the positive or negative branch of the root is chosen.
  */
case class RQR(override val maybeName: Option[String], p: Rational, q: Rational, pos: Boolean) extends Solution {
  require(!p.isZero || !q.isZero, "may not have p and q equal to zero")
  // TODO remove the following restriction--there's no reason why we cannot have complex solutions
  require(discriminant >= 0, "discriminant must not be negative")

  /**
    * Returns the number of branches for this instance.
    *
    * @return an `Int` representing the number of branches, which is always 2.
    */
  def branches: Int = 2

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
        Real(Number(-p) doDivide two),
        if (pos) Rational.half else -Rational.half,
        Real(Number(discriminant).sqrt)
    )

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean = discriminant >= 0

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean = !isReal && p.isZero

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int = ???


  /**
    * Scales the current `RQR` instance by a given `Rational` value.
    *
    * @param x the scaling factor represented as a `Rational`.
    * @return a new `RQR` instance with scaled values of `p` and `q`.
    */
  def scale(x: Rational): RQR =
    copy(p = x * p, q = x * x * q)

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field =
    scale(q.invert)

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = x match {
    case Real(ExactNumber(n, PureNumber)) if Value.maybeRational(n).contains(p) =>
      copy(p = -p)
    case Real(ExactNumber(n, PureNumber)) if Value.maybeRational(n).contains(-p) =>
      copy(p = -3 * p, q = -q, pos = !pos)
    case _ =>
      asNumber map (_ add x) getOrElse NaN
  }

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case r@RQR(_, _, _, _) =>
      val (iBase, iz, iSqrt) = value
      val (oBase, oz, oSqrt) = r.value
      val crossTerms =
        if (oSqrt == iSqrt && iBase == oBase)
          (iz + oz) * oSqrt * oBase
        else
          iz * oSqrt * iBase + oz * iSqrt * oBase
      (iBase multiply oBase) + crossTerms + iz * oz * (iSqrt multiply oSqrt)
    case r@Real(ExactNumber(v, f)) =>
      val (iBase, iz, iSqrt) = value
      iBase * r + iz * iSqrt * r
  }

  /**
    * Raises this Field to the power of the specified number.
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
          Real(one) // CONSIDER What about RQR(name, 0, 0, pos)?
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

  def square: Solution =
    copy(p = -3 * p, q = -q, pos = !pos)

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
    * For now, we consider a RQR to be exact, even though we can't write it out exactly.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = {
    val (base, r, offset) = value
    base.isExact && offset.isExact && r.signum >= 1
  }

  /**
    * Converts this quadratic root expression into an optional `Number` if possible.
    * The result depends on the discriminant:
    * - If the discriminant is negative, no numeric representation exists, and `None` is returned.
    * - If the discriminant is zero, it represents a double root, and a single exact `Number` instance is returned.
    * - If the discriminant is positive, a (fuzzy) `Number` is calculated and returned.
    *
    * @return an `Option[Number]` representing the numeric equivalent of this quadratic root if calculable; otherwise, `None`.
    */
  override def asNumber: Option[Number] = {
    if (discriminant < 0) None
    else if (discriminant.isZero) Some(Number(p.halve))
    else Some(Number(discriminant, SquareRoot).negateConditional(!pos) doSubtract Number(p) doDivide Number.two)
  }

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

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse toString

  /**
    * Determines if the given object can be considered equal to this instance.
    * This method is used to support the implementation of the equality operation.
    *
    * @param other the object to be compared against this instance
    * @return true if the given object is an instance of RQR; false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[RQR]

  /**
    * Compares this `RQR` instance to another object for equality.
    * The comparison checks if the other object is of the same type and has equivalent values
    * for the properties of `p`, `q`, and `pos`.
    *
    * @param other the object to compare with this instance for equality.
    * @return `true` if the provided object is a `RQR` and has the same values
    *         for the relevant properties; otherwise, `false`.
    */
  override def equals(other: Any): Boolean = other match {
    case that: RQR =>
      that.canEqual(this) &&
          p == that.p &&
          q == that.q &&
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
    val state = Seq(p, q, pos)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
  * Companion object for the `RQR` class.
  * Provides an unapply method for pattern matching.
  */
object RQR {
  def unapply(rqr: RQR): Option[(Option[String], Rational, Rational, Boolean)] =
    Some(rqr.maybeName, rqr.p, rqr.q, rqr.pos)

  val phi = new RQR(Some("\uD835\uDED7"), -1, -1, true)
  val psi = new RQR(Some("\uD835\uDED9"), -1, -1, false)
}
