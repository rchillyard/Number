/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.recover
import com.phasmidsoftware.number.core.Number.{NumberIsFractional, NumberIsOrdering}
import com.phasmidsoftware.number.core.Real.createFromRealField

import scala.language.implicitConversions
import scala.util.Try

/**
  * Sub-class of Field: as opposed to Complex.
  *
  * TODO remove (direct) Number references and replace with Real.
  *
  * @param x a Number which represents the value of this Real.
  */
case class Real(x: Number) extends Field {
  /**
   * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
   *
   * @return an optional `Factor`.
   */
  def maybeFactor: Option[Factor] = x.maybeFactor

  /**
    * Method to determine if this Real has infinite magnitude.
    *
    * @return true if the magnitude of this Field is infinite.
    */
  def isInfinite: Boolean = x.isInfinite

  /**
    * Method to determine if this Real has zero magnitude.
    * Zero is the additive identity.
    *
    * @return true if the magnitude of this Field is zero.
    */
  def isZero: Boolean = x.isZero

  /**
    * Method to determine if this Complex is real-valued.
    *
    * @return true if this is not imaginary.
    */
  def isReal: Boolean = !isImaginary

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is a root of a negative number.
    */
  def isImaginary: Boolean = x.isImaginary

  /**
    * Method to determine if this Field is equivalent to another Field (x).
    *
    * @param f the other field.
    * @return true if they are the same, otherwise false.
    */
  def isSame(f: Numerical): Boolean = f match {
    case Real(y) => (x doSubtract y).isZero
    case c: Complex => c.isSame(this)
    case n: Number => isSame(Real(n))
  }

  /**
    * Method to determine if this Real has unity magnitude.
    * Unity is the multiplicative identity.
    *
    * @return true if the magnitude of this Field is one.
    */
  def isUnity: Boolean = x.isUnity

  /**
    * Add y to this Real and return the result.
    * See Number.plus for more detail.
    *
    * @param y the addend.
    * @return the sum.
    */
  def add(y: Field): Field = createFromRealField(x.add(y))

  /**
    * Multiply this Real by y and return the result.
    *
    * * @param y the multiplicand.
    * * @return the product.
    */
  def multiply(y: Field): Field = x.multiply(y)

  /**
    * Divide this Real by y and return the result.
    *
    * @param y the divisor.
    * @return the quotient.
    */
  def divide(y: Field): Field = createFromRealField(x.divide(y))

  /**
    * Change the sign of this Real.
    */
  def unary_- : Field = createFromRealField(-x)

  /**
    * Raise this Real to the power p where p is a Number.
    *
    * If the Number of this Real is exact and if the exponent p is rational, then we convert x to a ComplexPolar first
    * and raise that to power p.
    *
    * @param p a Number.
    * @return this Real raised to power p.
    */
  def power(p: Number): Field = p match {
    case y@ExactNumber(Value(_), _) => x.power(Real(y))
    case ExactNumber(Value(_, _: Rational), _) => asComplex.power(p)
    case n => asComplex.power(n)
  }

  /**
    * Raise this Real to the power p.
    *
    * If the Number of this Real is exact and if the exponent p is rational, then we convert x to a ComplexPolar first
    * and raise that to power p.
    *
    * @param p a Field.
    * @return this Real raised to power p.
    */
  def power(p: Field): Field = p match {
    case Real(m) if m.isRational => asComplex power m
    case Real(m) => x.power(Real(m))
    case c: Complex => asComplex power c
  }

  def sqrt: Field = power(Real(Rational.half))

  /**
    * Yields the inverse of this Real.
   * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field = createFromRealField(x.invert)

  /**
    * Method to determine the sine of this Real.
   * The result will be a Real with PureNumber factor.
    *
    * @return the sine of this.
    */
  def sin: Field = Real(x.sin)

  /**
    * Method to determine the cosine of this Real.
   * The result will be a Real with PureNumber factor.
    *
    * @return the cosine.
    */
  def cos: Field = Real(x.cos)

  /**
    * Method to determine the tangent of this Real.
   * The result will be a Real with PureNumber factor.
    *
    * @return the tangent
    */
  def tan: Field = Real(x.tan)

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Real): Field = Real(x.atan(y.x))

  /**
    * Method to determine the natural log of this Real.
   * The result will be a Real with PureNumber factor.
    *
    * @return the natural log of this.
    */
  def log: Field = Real(x.log)

  /**
    * Method to raise e to the power of this Real.
    * The result will be a Real with NatLog factor.
    *
    * @return the e to the power of this.
    */
  def exp: Field = Real(x.exp)

  /**
    * Method to determine the sense of this Real: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int = x.signum

  /**
    * Method to "normalize" a field.
    *
    * @return a Real which is in canonical form.
    */
  def normalize: Field = createFromRealField(x.normalize)

  def compare(that: Field): Int = that match {
    case Real(y) => x.compare(y)
    case z: Complex => asComplex.compare(z)
  }

  /**
    * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
    *
   * @param context      the (optional) context in which we want to evaluate this Expression.
    *                    if factor is None then, the result will depend solely on whether this is exact.
    * @return true if this NumberLike object is exact in the context of factor, else false.
    */
  def isExactInContext(context: Context): Boolean = x.isExactInContext(context)

  /**
    * Method to return the x of this Real.
    *
    * @return Some(x).
    */
  def asNumber: Option[Number] = Some(x)

  /**
    * Method to return this Real as an Option[Real]..
    *
    * @return Some(this).
    */
  def asReal: Option[Real] = Some(this)

  /**
    * Method to return this Real as a Complex.
    * If this is a Real number x, return ComplexPolar(x) otherwise, return this.
    *
    * @return a Complex.
    */
  def asComplex: Complex = ComplexPolar(x)

  /**
    * Method to render this Field in a presentable manner.
    *
    * @return a String
    */
  def render: String = x.render

  override def toString: String = x.toString

  /**
   * Converts the value of this Real to a Double.
   * If the conversion is unsuccessful, it throws a NumberException.
   *
   * @return the Double representation of this Real.
   */
  def toDouble: Double = recover(x.toDouble, NumberException("Real.toDouble: logic error: x"))
}

/**
 * The `Real` object provides a representation and associated operations for real numbers.
 * It offers methods to create `Real` instances from various types (String, Int, Double, Rational),
 * and provides several utilities, implicit conversions, and typeclass instances for operations and comparisons.
 */
object Real {
  /**
   * Constructs a new Real from the given string representation of a number.
   *
   * @param w the string to be parsed and converted into a Real.
   * @return a new Real instance representing the parsed number.
   */
  def apply(w: String): Real = Real(Number(w))

  /**
   * Constructs a `Real` object from an integer value.
   *
   * @param x the integer value to be converted to a `Real`.
   * @return a `Real` instance representing the supplied integer.
   */
  def apply(x: Int): Real = Real(Number(x))

  /**
   * Converts a double value into a Real object.
   *
   * @param d the double value to be converted
   * @return a Real object representing the provided double value
   */
  def apply(d: Double): Real = Real(Number(d))

  /**
   * Converts a given Rational number into a Real number.
   *
   * @param r the Rational number to be converted.
   * @return the corresponding Real number representation of the input Rational.
   */
  def apply(r: Rational): Real = Real(Number(r))

  def atan(x: Field, y: Field): Real = (for (a <- x.asNumber; b <- y.asNumber) yield Real(a atan b)).getOrElse(Real(Number.NaN))

  val atanFunction: (Field, Field) => Real = atan

  /**
   * Creates a Real instance from a given Field if it can be represented as a real number.
   *
   * If the input Field is already a Real, it is returned as-is. If the input is a BaseComplex
   * instance and is identified as real (i.e., has no imaginary component), the corresponding
   * Real value is extracted. If the input cannot be represented as a Real, an exception is thrown.
   *
   * @param x the input Field to be converted to a Real.
   * @return a Real instance representing the input Field.
   * @throws NumberException if the input Field is not real or cannot be converted to a Real.
   */
  def createFromRealField(x: Field): Real = x match {
    case r: Real => r
    case c: BaseComplex if c.isReal =>
      c.asNumber match {
        case Some(value) => Real(value)
        case None => throw NumberException(s"Real.createFromRealField: x cannot be represented as a Real: $x")
      }
    case _ => throw NumberException(s"Real.createFromRealField: x is not real: $x")
  }

  /**
    * Method to parse a String and yield a Try[Number].
    *
    * NOTE: this method indirectly invokes apply(Rational, Factor, Option of Fuzz[Double] )
    *
    * @param w the String to be parsed.
    * @return a Number.
    */
  def parse(w: String): Try[Real] = Number.parse(w) map (Real(_))

  implicit def convertFromNumber(x: Number): Field = Real(x)

  /**
    * Implicit class to operate on Numbers introduced as integers.
    *
    * CONSIDER generalizing this to inputs of Values (or Rationals, Doubles).
    *
    * @param x an Int to be treated as a Real.
    */
  implicit class RealOps(x: Int) {

    /**
      * Add this x (a Real) and yield a Real.
      *
      * @param y the addend, a Real.
      * @return a Real whose value is x + y.
      */
    def +(y: Real): Real = createFromRealField(Real(x) add y)

    /**
      * Multiply x by y (a Real) and yield a Real.
      *
      * @param y the multiplicand, a Real.
      * @return a Real whose value is x * y.
      */
    def *(y: Real): Real = createFromRealField(Real(x) multiply y)

    /**
      * Divide x by y (a Real) and yield a Real.
      *
      * @param y the divisor, a Real.
      * @return a Real whose value is x / y.
      */
    def /(y: Real): Real = *(createFromRealField(y.invert))

    /**
      * Divide x by y (an Int) and yield a Real.
      * NOTE: the colon is necessary in order to coerce the left hand operand to be a Real.
      *
      * @param y the divisor, an Int.
      * @return a Real whose value is x / y.
      */
    def :/(y: Int): Real = /(Real(y))
  }

  /**
    * Following are the definitions required by Ordering[Real]
    */
  trait RealIsOrdering extends Ordering[Real] {
    /**
     * When we do a compare on NatLog numbers, they are in the same order as PureNumber numbers (i.e. monotonically increasing).
      * It's not necessary to convert exact numbers to fuzzy numbers for this purpose, we simply
     * pretend that the NatLog numbers are PureNumber numbers.
      *
      * @param x the first Real.
      * @param y the second Real.
      * @return an Int representing the order.
      */
    def compare(x: Real, y: Real): Int = NumberIsOrdering.compare(x.x, y.x)
  }

  implicit object RealIsOrdering extends RealIsOrdering

  /**
    * Following are the definitions required by Numeric[Real]
    */
  trait RealIsNumeric extends Numeric[Real] with RealIsOrdering {
    def plus(x: Real, y: Real): Real = createFromRealField(x add y)

    def minus(x: Real, y: Real): Real = plus(x, negate(y))

    def times(x: Real, y: Real): Real = createFromRealField(x multiply y)

    def negate(x: Real): Real = createFromRealField(-x)

    def fromInt(x: Int): Real = Real(x)

    def parseString(str: String): Option[Real] = Number.parse(str).map(Real(_)).toOption

    def toInt(x: Real): Int = toLong(x).toInt

    def toLong(x: Real): Long = NumberIsFractional.toLong(x.x)

    def toDouble(x: Real): Double = x.toDouble

    def toFloat(x: Real): Float = toDouble(x).toFloat
  }

  /**
    * CONSIDER inlining this method or making it private.
    *
    * @param x the first number.
    * @param y the second number.
    * @return the order.
    */
  def doCompare(x: Real, y: Real): Int = RealIsOrdering.compare(x, y)

  /**
    * Following are the definitions required by Fractional[Real]
    */
  trait RealIsFractional extends Fractional[Real] with RealIsNumeric {
    def div(x: Real, y: Real): Real = times(x, createFromRealField(y.invert))
  }

  implicit object RealIsFractional extends RealIsFractional with RealIsNumeric with RealIsOrdering

}