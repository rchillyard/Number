package com.phasmidsoftware.number.core

import scala.util._

/**
  * This class is designed to model an exact Numeral.
  * See GeneralNumber for more details on the actual representation.
  *
  * TODO implement scientific notation by having factors such os 10^3, 10^6, etc. (alternatively, add a separate parameter)
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  */
case class ExactNumber(override val value: Value, override val factor: Factor) extends GeneralNumber(value, factor, None) {

  /**
    * Add this Number to n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the sum of this and n.
    */
  def doAdd(n: Number): Number = this match {
    case x: GeneralNumber => GeneralNumber.plus(x, n)
  }

  /**
    * Multiply this Number by n.
    *
    * NOTE: there is currently a significant difference between GeneralNumber.plus and FuzzyNumber.plus
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number = GeneralNumber.times(this, n)

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number = Number.power(this, p)

  /**
    * Method to compare this Number with another.
    *
    * @param other the other Number.
    * @return -1, 0, or 1 according to the relative magnitudes.
    */
  def compare(other: Number): Int = Number.doCompare(this, other)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return an ExactNumber.
    */
  def make(v: Value, f: Factor): Number = ExactNumber(v, f)

  /**
    * We cannot add fuzziness to an Exact number so we return the equivalent FuzzyNumber.
    *
    * @param fo the (optional) fuzziness.
    * @return a Number.
    */
  def make(fo: Option[Fuzziness[Double]]): Number = FuzzyNumber(value, factor, fo)

  /**
    * If the result of invoking f on this is a Double, then there will inevitably be some loss of precision.
    *
    * CONSIDER rewriting this so that we don't have to override the method. But be careful!
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return a Number which is the square toot of this, possibly fuzzy, Number.
    */
  override protected def makeFuzzyIfAppropriate(f: Number => Number, relativePrecision: Int): Number = {
    val z = f(this)
    z.value match {
      case v@Left(Left(Some(_))) => FuzzyNumber(v, z.factor, Some(Fuzziness.createFuzz(relativePrecision)))
      case v => z.make(v) // NOTE: do not attempt to convert this to a FuzzyNumber because it will cause a stack overflow
    }
  }

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    * the result will be zero.
    *
    * TEST me
    *
    * @param p the confidence desired (ignored).
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * @param p the confidence desired. Ignored if isZero is true.
    * @return true if this Number is equivalent to zero with at least p confidence.
    */
  def isProbablyZero(p: Double): Boolean = isZero
}
