/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{one, two}

/**
  * An abstract class representing a root of a reduced quadratic equation of the form `x^2 + px + q = 0`
  * The value of the root is `(-p/2) ± sqrt((-p/2)^2 - q)`.
  * The class extends `Solution,` which in turn extends `Field`.
  *
  * TODO rename this class as ReducedQuadraticRoot and remove or rename the other class with that name.
  *
  * NOTE that, for now at least, the types of p and q are Int.
  *
  * @constructor Constructs a reduced quadratic root with parameters `p`, `q`, and a boolean `pos` to determine the sign of the root.
  * @param p   The coefficient in the quadratic equation.
  * @param q   The constant term in the quadratic equation.
  * @param pos Determines if the positive or negative branch of the root is chosen.
  */
case class RQR(val name: String, val p: Int, val q: Int, val pos: Boolean) extends Solution {
  require(p != 0 || q != 0, "may not have p and q equal to zero")
  require(discriminant >= 0, "discriminant must not be negative")

  def branches: Int = 2

  /**
    * This method computes a pair of values. The first (0th) branch can always be found simply by
    * adding the two parts together.
    *
    * @return a tuple where the first element is a `Number` representing the base value,
    *         and the second element is an `Option[Number]` with a different `Factor` than the first element.
    */
  lazy val value: (Number, Option[Number]) = {
    val disc = one.make(Rational(discriminant, 4))
    val squareRoot = disc.make(SquareRoot)
    (
        Number(-p) doDivide two,
        Some(if (pos) squareRoot else squareRoot.makeNegative)
    )
  }

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean = ??? // Depends on pos

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean = ???

  /**
    * Determine the "sign" of this field.
    * For a real-valued quantity (Real or Number), we try to determine if it is to the right, left or at the origin.
    * For a complex number, we get the signum of the real part.
    *
    * @return +1 if to the right of the origin, -1 if to the left, 0 if at the origin.
    */
  def signum: Int = ???

  /**
    * Yields the inverse of this Field.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field = ???

  /**
    * Method to "normalize" a field.
    *
    * @return a Field which is in canonical form.
    */
  def normalize: Field = {
    val (base, maybeOffset) = value
    val offset = maybeOffset.getOrElse(Number.zero)
    Real(if (pos) base doAdd offset else base doSubtract offset)
  }

  /**
    * For now, we consider a RQR to be exact, even though we can't write it out exactly.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  override def isExact: Boolean = {
    val (base, maybeOffset) = value
    base.isExact && {
      maybeOffset match {
        case None =>
          true
        case Some(offset) =>
          val actualOffset = if (pos) offset else offset.makeNegative
          actualOffset.isExact
      }
    }
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
    else if (discriminant == 0) Some(Number(Rational(p, 2)))
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
  def discriminant: Int =
    p * p - 4 * q

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String = name

  /**
    * Provides a string representation of the object by returning the `name` field.
    *
    * @return a `String` that represents the `name` of this quadratic root instance.
    */
  override def toString: String = name

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
  def unapply(rqr: RQR): Option[(String, Int, Int, Boolean)] =
    Some(rqr.name, rqr.p, rqr.q, rqr.pos)

  val phi = new RQR("phi", -1, -1, true)
  val psi = new RQR("psi", -1, -1, false)
}
