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
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new Number.
    */
  def this(v: Value) = this(v, Scalar)

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

  /**
    * Action to render this ExactNumber as a String.
    *
    * TEST me
    *
    * @return a String.
    */
  override def render: String = toString

  def asFuzzyNumber: FuzzyNumber = FuzzyNumber(value, factor, None)

}
