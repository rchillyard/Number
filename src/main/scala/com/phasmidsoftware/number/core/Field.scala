/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.misc.FP.recover
import scala.language.implicitConversions

/**
  * Trait which describes the behavior of all Numbers and Complex instances.
  * See [[https://en.wikipedia.org/wiki/Field_(mathematics)]].
  *
  * Currently, the only sub-classes of Field are Number and Complex.
  *
  * The operations supported are addition, subtraction, multiplication and division.
  * By inference, we should be able to raise an instance of Field to a numeric power.
  */
trait Field extends Numerical with Approximatable with Ordered[Field] {

  /**
    * Method to determine if this Field is represented by a Complex number.
    *
    * @return true if this is Complex.
    */
  def isComplex: Boolean = this match {
    case ComplexCartesian(_, _) | ComplexPolar(_, _, _) => true
    case _ => false
  }

  /**
    * Method to determine if this Field is real-valued (i.e. the point lies on the real axis).
    *
    * @return true if not imaginary.
    */
  def isReal: Boolean

  /**
    * Method to determine if this Field is imaginary-valued (i.e. the point lies on the imaginary axis).
    *
    * @return true if this is imaginary.
    */
  def isImaginary: Boolean

  /**
    * Add x to this Field and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field

  /**
    * Synonym for add.
    *
    * @param x the addend.
    * @return the result.
    */
  def +(x: Field): Field = add(x)

  /**
    * Subtract x from this Field and return the result.
    *
    * @param x the subtrahend.
    * @return the difference of this - x.
    */
  def subtract(x: Field): Field = this + -x

  /**
    * Synonym for subtract.
    *
    * @param x the subtrahend.
    * @return <code>this - x</code>.
    */
  def -(x: Field): Field = subtract(x)

  /**
    * Multiply this Field by x and return the result.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field

  /**
   * Multiply this Field by the given Field and return the result.
   *
   * @param x the multiplicand, an instance of Field.
   * @return the product of this Field and the given Field.
   */
  def *(x: Field): Field = multiply(x)

  /**
    * Divide this Field by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field

  /**
    * Divide this Field by another Field.
    *
    * TESTME
    *
    * @param x the other Field.
    * @return the quotient of this and x.
    */
  def /(x: Field): Field = divide(x)

  /**
    * Raise this Field to the power p.
    *
    * @param p an Int.
    * @return this Field raised to power p.
    */
  def power(p: Int): Field = p match {
    case 0 => Real(Number.one)
    case 1 => this
    case _ => power(Number(p)) // TODO need to simplify the result. See NumberSpec: power/work for squaring Log2
  }

  /**
   * Raises this Field to the power of the specified number.
   *
   * @param p the exponent, provided as a Number.
   * @return the result of raising this Field to the power p.
   */
  def power(p: Number): Field

  /**
    * Raise this Field to the power p.
    *
    * @param p a Field.
    * @return this Field raised to power p.
    */
  def power(p: Field): Field

  /**
    * Computes and returns an approximate numerical value for this expression.
    *
    * @return a `Double` representing the approximation of this expression.
    */
  def approximation: Option[Real] = asNumber map (Real(_))

  /**
    * Computes the sine of this Field.
    *
    * @return the sine of this Field, as an instance of Field.
    */
  def sin: Field

  /**
    * Computes the trigonometric cosine of this Field.
    *
    * @return the cosine of this Field.
    */
  def cos: Field

  /**
    * Computes the tangent of this Field.
    *
    * @return the tangent of this Field as a new Field.
    */
  def tan: Field

  /**
    * Calculates the arctangent (inverse tangent) of the given Real number.
    *
    * @param y the Real number whose arctangent is to be calculated.
    * @return the arctangent of the specified Real number, represented as a Field.
    */
  def atan(y: Real): Field

  /**
    * Computes the natural logarithm (log base e) of this Field.
    *
    * @return a new Field representing the result of the logarithmic computation.
    */
  def log: Field

  /**
    * Computes the exponential of this Field.
    *
    * @return a Field representing the exponential of this instance.
    */
  def exp: Field
}

/**
 * Companion object Field provides utility methods and implicit conversions for the Field type.
 * It also acts as a container for implicit definitions and helpers relating to the Field type.
 */
object Field {
  /**
    * Attempt to force the given field to be a Number.
    * Because this may throw an Exception, it is much better to use asNumber, an instance method of Field.
    *
    * @param field the given field.
    * @return a Number if field is a Number, otherwise, this will throw a NumberException.
    */
  def convertToNumber(field: Field): Number = recover(field.asNumber, NumberException(s"$field is not a Number"))

  /**
    * Implicit converter from `Rational` value to a `Field` value.
    *
    * @param r the Rational value to be converted.
    * @return a Field representation of the provided Rational value.
    */
  implicit def convertRationalToField(r: Rational): Field = Real(r)

  /**
    * Definition of concrete (implicit) type class object for Field being Fuzzy.
    */
  implicit object FieldIsFuzzy extends Fuzzy[Field] {
    /**
      * Method to determine if x1 and x2 can be considered the same with a probability of p.
      *
      * @param p  a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless x1 actually is x2.
      * @param x1 a value of X.
      * @param x2 a value of X.
      * @return true if x1 and x2 are considered equal with probability p.
      */
    def same(p: Double)(x1: Field, x2: Field): Boolean = x1.add(-x2).asNumber.exists(_.isProbablyZero(p))
  }
}
