/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.*

import java.util.Objects

/**
  * This class is designed to model an exact Numeral.
  * See GeneralNumber for more details on the actual representation.
  *
  * TODO implement scientific notation by having factors such os 10∧3, 10∧6, etc. (alternatively, add a separate parameter)
  *
  * @param nominalValue the nominalValue of the Number, expressed as a nested Either type.
  * @param factor       the scale factor of the Number: valid scales are: PureNumber, Radian, and NatLog.
  */
case class ExactNumber(override val nominalValue: Value, override val factor: Factor) extends GeneralNumber(nominalValue, factor, None) {
  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  lazy val isExact: Boolean = true

  /**
    * Determines if this ExactNumber is equivalent to another Numerical object (x).
    *
    * @param x the other Numerical object to compare against. It could be of type Real, FuzzyNumber, ExactNumber, or Complex.
    * @return true if the two Numerical objects are considered the same, otherwise false.
    */
  def isSame(x: Numerical): Boolean = x match {
    case Real(n) =>
      isSame(n)
    case n: FuzzyNumber =>
      n.isSame(this)
    case n@ExactNumber(v, f) =>
      Value.isEqual(nominalValue, v) && factor == f || doSubtract(n).isZero
    case c: Complex =>
      c.isSame(Real(this))
  }

  /**
    * Compares this instance with another object to determine equality.
    * Equality is determined based on specific conditions for the `ExactNumber` type.
    * NOTE that there may be a few rare situations where we return false for two "equal" numbers.
    * However, generally speaking, numbers should be normalized before equality testing.
    *
    * @param obj the object to be compared with this instance
    * @return true if the specified object is equal to this instance; false otherwise
    */
  override def equals(obj: Any): Boolean = (this, obj) match {
    case (ExactNumber(v1, f1), ExactNumber(v2, f2)) if f1 == f2 =>
      Value.isEqual(v1, v2)
    case _ =>
      false
  }

  /**
    * Generates a hash code for this `ExactNumber` object.
    * The hash code is computed based on the `nominalValue` and `factor` fields
    * and is therefore consistent with `equals`.
    *
    * @return an integer hash code value for this object.
    */
  override def hashCode(): Int = Objects.hash(nominalValue, factor)

  /**
    * @return true if this Number is equal to zero.
    */
  lazy val isZero: Boolean = GeneralNumber.isZero(this)

  /**
    * Method to make some trivial simplifications of this ExactNumber.
    *
    * @return either this Number or a simplified Number.
    */
  lazy val simplify: Number = (factor, nominalValue) match {
    case (Logarithmic(_), Right(0)) =>
      Number.one
    case (Logarithmic(_), Left(Right(Rational.negInfinity))) =>
      Number.zero
    case (Logarithmic(_), Left(Right(Rational.infinity))) =>
      ExactNumber(Value.fromRational(Rational.infinity), PureNumber)
    case (Euler, Right(1)) => // this is `e∧i𝛑`, which equals `-1` by Euler's Identity
      Number(-1)
    case (Euler, Left(Right(Rational.half))) => // this is `e∧i𝛑/2`, which equals `i` by Euler's Identity
      Number.i
    // XXX this handles all roots (of which there are currently only SquareRoot and CubeRoot)
    case (NthRoot(n), v) =>
      v match {
        case Right(x) =>
          (Rational.squareRoots.get(x) map (make(_, PureNumber))).getOrElse(this)
        case Left(Right(r)) =>
          r.root(n) match {
            case Some(x) =>
              ExactNumber(Value.fromRational(x), PureNumber)
            case _ =>
              this
          }
        case Left(Left(Some(_))) =>
          scale(PureNumber)
        case _ =>
          this
      }
    case _ =>
      this
  }

  /**
    * Add this Number to n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the sum of this and n.
    */
  def doAdd(n: Number): Number = this match {
    case x: GeneralNumber =>
      GeneralNumber.plus(x, n)
  }

  /**
    * Multiply this Number by n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number =
    GeneralNumber.times(this, n)

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number =
    GeneralNumber.power(this, p)

  /**
    * Method to compare this Number with another.
    *
    * @param other the other Number.
    * @return -1, 0, or 1 according to the relative magnitudes.
    */
  def compare(other: Number): Int =
    Number.doCompare(this, other)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * CONSIDER should be invoke simplify from other make methods?
    *
    * @param v the value.
    * @param f the factor.
    * @return an ExactNumber.
    */
  def make(v: Value, f: Factor): Number =
    ExactNumber(v, f)

  /**
    * We cannot add fuzziness to an Exact number, so we return the equivalent FuzzyNumber.
    *
    * @param fo the (optional) fuzziness.
    * @return a Number.
    */
  def make(fo: Option[Fuzziness[Double]]): Number =
    FuzzyNumber(nominalValue, factor, fo)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the nominalValue and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param v  the nominalValue (a Double).
    * @param f  Factor.
    * @param fo optional fuzz.
    * @return either a Number.
    */
  def make(v: Double, f: Factor, fo: Option[Fuzziness[Double]]): Number = fo match {
    case None =>
      ExactNumber(Value.fromDouble(Some(v)), f)
    case Some(_) =>
      FuzzyNumber(Value.fromDouble(Some(v)), f, fo)
  }

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  lazy val signum: Int =
    Number.signum(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this `FuzzyNumber` cannot be distinguished from zero with `p` confidence, then
    * the result will be zero.
    *
    * @param p the confidence desired (ignored).
    * @return an `Int` which is negative, zero, or positive, according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * Method to determine if this Number is probably zero.
    * It is used particularly when comparing two Numbers to see if they are the same.
    *
    * @param p the confidence desired. Ignored.
    * @return true if this Number is actually zero.
    */
  def isProbablyZero(p: Double = 0.5): Boolean = isZero

  /**
    * Render this ExactNumber as a String representation.
    *
    * @return the String representation of this ExactNumber.
    */
  lazy val render: String = factor match {
    case SquareRoot =>
      val sb = new StringBuilder()
      sb.append(SquareRoot.render(nominalValue))
      sb.toString
    case _ =>
      toString
  }

  /**
    * Render this ExactNumber in String form, including the factor.
    *
    * @return a String
    */
  override lazy val toString: String = modulate match {
    case Number.pi =>
      Radian.toString
    case ExactNumber(Right(-1), Radian) =>
      "-" + Radian.toString
    case _ =>
      val sb = new StringBuilder()
      factor match {
        case Euler =>
          sb.append(s"e∧i${Value.valueToString(nominalValue, skipOne = true)}𝛑")
        case Logarithmic(_) =>
          sb.append(factor.render(nominalValue))
        case Scalar(_) =>
          sb.append(Value.valueToString(nominalValue, skipOne = false))
          sb.append(factor.toString)
        case InversePower(_) =>
          sb.append(factor.render(nominalValue))
        case _ =>
          throw CoreException(s"factor is not matched: $factor")
      }
      sb.toString
  }
}

/**
  * Companion object for the `ExactNumber` class.
  *
  * Provides factory methods for creating instances of `ExactNumber`. These methods allow
  * creation with a given `Factor` or default to the `PureNumber` factor. This object encapsulates
  * logic to initialize `ExactNumber` instances with consistent internal representations and initializes
  * the required values.
  */
object ExactNumber {

  /**
    * Creates a new instance of `ExactNumber` from a given integer value and a specified factor.
    *
    * @param x      the integer value to be encapsulated by the `ExactNumber`.
    * @param factor the factor associated with the `ExactNumber` instance.
    * @return a new `ExactNumber` instance with the specified value and factor.
    */
  def apply(x: Int, factor: Factor): ExactNumber =
    new ExactNumber(Value.fromInt(x), factor)

  /**
    * Creates an instance of `ExactNumber` with the value `x` and a default factor of `PureNumber`.
    *
    * @param x an integer representing the value of the `ExactNumber`.
    * @return an instance of `ExactNumber` with the specified value and the `PureNumber` factor.
    */
  def apply(x: Int): ExactNumber =
    apply(x, PureNumber)

  def apply(x: Rational): ExactNumber =
    apply(Value.fromRational(x), PureNumber)

  /**
    * Calculates the logarithm of a given `ExactNumber` `x` with base `b`.
    * The method searches for an integer value `i` in the range -10 to 10 such that
    * raising `b` to the power of `i` equals `x`. If such an `i` is found, it is
    * returned wrapped in an `Option` as a `Number`. Otherwise, returns `None`.
    *
    * @param x the number for which the logarithm is to be calculated.
    * @param b the base of the logarithm.
    * @return an `Option` containing the logarithm result as a `Number` if
    *         an integer solution exists within the searched range, or `None` otherwise.
    */
  def log(x: ExactNumber, b: ExactNumber): Option[Number] =
    (for {
      i <- -10 to 10
      z <- Option.when((b `power` i) == x)(i)
    } yield z).headOption.map(Number(_)) // TODO we can improve on this, I think

}
