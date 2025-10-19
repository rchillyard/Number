/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.{NumberIsFractional, NumberIsOrdering}
import com.phasmidsoftware.number.core.Real.createFromRealField
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Solution}
import com.phasmidsoftware.number.core.inner.{Factor, Rational, Value}
import com.phasmidsoftware.number.misc.FP.recover
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
    * @return false.
    */
  def isAlgebraic: Boolean = false

  /**
    * @return false.
    */
  def isComplex: Boolean = false

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = x.isExact

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
    case Real(n: ExactNumber) => n.isSame(x)
    case Real(y) => (x doSubtract y).isZero
    case c: Complex => c.isSame(this)
    case n: Number => isSame(Real(n))
    case s: Algebraic => s.isSame(this)
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
  def add(y: Field): Field =
    createFromRealField(x.add(y))

  /**
    * Multiply this Real by y and return the result.
    *
    * * @param y the multiplicand.
    * * @return the product.
    */
  def multiply(y: Field): Field =
    x.multiply(y)

  /**
    * Divide this Real by y and return the result.
    *
    * @param y the divisor.
    * @return the quotient.
    */
  def divide(y: Field): Field =
    createFromRealField(x.divide(y))

  /**
    * Change the sign of this Real.
    */
  def unary_- : Field =
    createFromRealField(-x)

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
    case y@ExactNumber(Value(_), _) =>
      x.power(Real(y))
    case ExactNumber(Value(_, _: Rational), _) =>
      asComplex.power(p)
    case n =>
      asComplex.power(n)
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
    case Real(m) if m.isRational =>
      asComplex power m
    case Real(m) =>
      x.power(Real(m))
    case c: Complex =>
      asComplex power c
  }

  /**
    * Squares this Field instance by multiplying it with itself.
    *
    * @return the result of squaring this Field.
    */
  def square: Field = Real(x.square)

  /**
    * Computes the square root of this Real number.
    * This is equivalent to raising the number to the power of 1/2.
    *
    * @return the square root as a Field.
    */
  def sqrt: Field = power(Real(Rational.half))

  /**
    * Yields the inverse of this Real.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Field =
    createFromRealField(x.invert)

  /**
    * Method to determine the sine of this Real.
    * The result will be a Real with PureNumber factor.
    *
    * @return the sine of this.
    */
  def sin: Field =
    Real(x.sin)

  /**
    * Method to determine the cosine of this Real.
    * The result will be a Real with PureNumber factor.
    *
    * @return the cosine.
    */
  def cos: Field =
    Real(x.cos)

  /**
    * Method to determine the tangent of this Real.
    * The result will be a Real with PureNumber factor.
    *
    * @return the tangent
    */
  def tan: Field =
    Real(x.tan)

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Real): Field =
    Real(x.atan(y.x))

  /**
    * Method to determine the natural log of this Real.
    * The result will be a Real with PureNumber factor.
    *
    * @return the natural log of this.
    */
  def ln: Field =
    x.ln

  /**
    * Method to raise e to the power of this Real.
    * The result will be a Real with NatLog factor.
    *
    * @return the e to the power of this.
    */
  def exp: Field =
    Real(x.exp)

  /**
    * Method to determine the sense of this Real: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int =
    x.signum

  /**
    * Returns the absolute value of this Real.
    *
    * @return a Real representing the absolute value of this.
    */
  def abs: Real =
    Real(x.abs)

  /**
    * Method to "normalize" a field.
    *
    * @return a Real which is in canonical form.
    */
  def normalize: Field =
    x match {
      case Number.i =>
        ComplexCartesian(Number.zero, Number.one)
      case _ =>
        createFromRealField(this)
    }

  def compare(that: Field): Int = that match {
    case Real(y) =>
      x.compare(y)
    case z: Complex =>
      asComplex.compare(z)
  }

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
    * Converts the true value of this Real to a Double.
    * If the conversion is unsuccessful, it throws a NumberException.
    *
    * @return the Double representation of this Real, wrapped in Option.
    */
  def maybeDouble: Option[Double] =
    x.toPureNumber.toNominalDouble

  /**
    * Converts the true value of this Real to a Double.
    * If the conversion is unsuccessful, it throws a NumberException.
    *
    * @return the Double representation of this Real.
    */
  def toDouble: Double =
    recover(maybeDouble, NumberException("Real.toDouble: logic error: x"))

  /**
    * Compares the current object with the specified object for equality.
    *
    * CONSIDER eliminating this method as it is simply the default implementation.
    *
    * @param obj the object to compare with the current object
    * @return true if the specified object is of type `Real` and its value matches the current object's value, false otherwise
    */
  override def equals(obj: Any): Boolean = obj match {
    case that: Real =>
      x == that.x
    case _ =>
      false
  }

  /**
    * Computes a hash code value for the object.
    * This method is intended to provide a consistent hash code implementation
    * based on the `hashCode` of the encapsulated value or component.
    *
    * @return an integer hash code value for the object
    */
  override def hashCode(): Int =
    x.hashCode()
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
  def apply(w: String): Real =
    Real(Number(w))

  /**
    * Constructs a `Real` object from an integer value.
    *
    * @param x the integer value to be converted to a `Real`.
    * @return a `Real` instance representing the supplied integer.
    */
  def apply(x: Int): Real =
    Real(Number(x))

  /**
    * Converts a double value into a Real object.
    *
    * @param d the double value to be converted
    * @return a Real object representing the provided double value
    */
  def apply(d: Double): Real =
    Real(Number(d))

  /**
    * Converts a given Rational number into a Real number.
    *
    * @param r the Rational number to be converted.
    * @return the corresponding Real number representation of the input Rational.
    */
  def apply(r: Rational): Real =
    Real(Number(r))

  /**
    * Converts a `NumberLike` instance into a `Real`.
    *
    * The conversion operation determines the specific subtype of `NumberLike` and handles it accordingly.
    * If the input is a `Rational`, it delegates the processing to `apply(Rational)`.
    * If the input is a `Number`, it delegates the processing to `apply(Number)`.
    * Throws a `NumberException` if the input `NumberLike` cannot be converted into a `Real`.
    *
    * @param n the `NumberLike` instance to be converted to a `Real`.
    * @return a `Real` representing the converted `NumberLike` input.
    * @throws NumberException if the input cannot be converted into a `Real`.
    */
  def apply(n: NumberLike): Real = n match {
    case x: Real =>
      x
    case r: Rational =>
      apply(r)
    case x: Number =>
      apply(x)
    case x: Algebraic =>
      apply(x.solve)
    case solution: Solution =>
      apply(solution.asField)
    case _ =>
      throw NumberException(s"Real.apply: cannot convert $n to a Real")
  }

  /**
    * Computes the arctangent of two `Field` values and returns the result as a `Real`.
    *
    * NOTE that Expression defines a more precise version of atan.
    *
    * This operation calculates the arctangent of the ratio `x / y`, using their numeric representations.
    * If either `x` or `y` cannot be converted into a numeric representation, the result is `NaN`.
    *
    * @param x the first `Field` input, representing the numerator in the arctangent calculation.
    * @param y the second `Field` input, representing the denominator in the arctangent calculation.
    * @return a `Real` value representing the arctangent of the arguments, or `NaN` if the inputs are not valid numbers.
    */
  def atan(x: Field, y: Field): Real =
    (for (a <- x.asNumber; b <- y.asNumber) yield Real(a atan b)).getOrElse(Real(Number.NaN))

  def log(x: Field, y: Field): Real =
    (for (a <- x.asNumber; b <- y.asNumber) yield Real(a log b)).getOrElse(Real(Number.NaN))

//  val atanFunction: (Field, Field) => Real = atan

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
    case r: Real =>
      r
    case c: BaseComplex if c.isReal =>
      c.asNumber match {
        case Some(value) =>
          Real(value)
        case None =>
          throw NumberException(s"Real.createFromRealField: x cannot be represented as a Real: $x")
      }
    case _ =>
      throw NumberException(s"Real.createFromRealField: x is not real: $x")
  }

  /**
    * Method to parse a String and yield a Try[Number].
    *
    * NOTE: this method indirectly invokes apply(Rational, Factor, Option of Fuzz[Double] )
    *
    * @param w the String to be parsed.
    * @return a Number.
    */
  def parse(w: String): Try[Real] =
    Number.parse(w) map (Real(_))

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
    def +(y: Real): Real =
      createFromRealField(Real(x) add y)

    /**
      * Multiply x by y (a Real) and yield a Real.
      *
      * @param y the multiplicand, a Real.
      * @return a Real whose value is x * y.
      */
    def *(y: Real): Real =
      createFromRealField(Real(x) multiply y)

    /**
      * Divide x by y (a Real) and yield a Real.
      *
      * @param y the divisor, a Real.
      * @return a Real whose value is x / y.
      */
    def /(y: Real): Real =
      *(createFromRealField(y.invert))

    /**
      * Divide x by y (an Int) and yield a Real.
      * NOTE: the colon is necessary in order to coerce the left hand operand to be a Real.
      *
      * @param y the divisor, an Int.
      * @return a Real whose value is x / y.
      */
    def :/(y: Int): Real =
      /(Real(y))
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
    def compare(x: Real, y: Real): Int =
      NumberIsOrdering.compare(x.x, y.x)
  }

  implicit object RealIsOrdering extends RealIsOrdering

  /**
    * Following are the definitions required by Numeric[Real]
    */
  trait RealIsNumeric extends Numeric[Real] with RealIsOrdering {
    /**
      * Adds two Real numbers and returns the result.
      *
      * @param x The first Real number to be added.
      * @param y The second Real number to be added.
      * @return The result of adding x and y as a Real number.
      */
    def plus(x: Real, y: Real): Real =
      createFromRealField(x add y)

    /**
      * Subtracts the second real number from the first real number.
      *
      * @param x the first operand of type Real
      * @param y the second operand of type Real to be subtracted from the first operand
      * @return the result of subtracting the second operand from the first operand, of type Real
      */
    def minus(x: Real, y: Real): Real =
      plus(x, negate(y))

    /**
      * Multiplies two Real numbers and returns the result as a Real.
      *
      * @param x the first Real number
      * @param y the second Real number
      * @return the product of x and y as a Real
      */
    def times(x: Real, y: Real): Real =
      createFromRealField(x multiply y)

    /**
      * Computes the negation of the given Real number.
      *
      * @param x the Real number to be negated
      * @return a new Real number representing the negation of the input value
      */
    def negate(x: Real): Real =
      createFromRealField(-x)

    /**
      * Converts an integer value to a `Real` instance.
      *
      * @param x the integer value to be converted.
      * @return the `Real` instance corresponding to the given integer value.
      */
    def fromInt(x: Int): Real =
      Real(x)

    /**
      * Parses a given string and attempts to convert it into an `Option[Real]`.
      *
      * @param str the input string that needs to be parsed
      * @return an `Option[Real]` representing the parsed value if successful, or `None` if the parsing fails
      */
    def parseString(str: String): Option[Real] =
      Number.parse(str).map(Real(_)).toOption

    /**
      * Converts a Real number to an Int by first converting it to a Long,
      * then narrowing it to an Int.
      *
      * @param x the Real number to be converted
      * @return the integer representation of the Real number
      */
    def toInt(x: Real): Int =
      toLong(x).toInt

    /**
      * Converts a `Real` value to its corresponding `Long` representation.
      *
      * @param x the `Real` value to be converted
      * @return the `Long` representation of the given `Real` value
      */
    def toLong(x: Real): Long =
      NumberIsFractional.toLong(x.x)

    /**
      * Converts a Real number into a Double representation.
      *
      * @param x the Real number to be converted
      * @return the Double representation of the given Real number
      */
    def toDouble(x: Real): Double =
      x.toDouble

    /**
      * Converts a Real number to a Float representation.
      *
      * @param x the Real number to be converted.
      * @return the Float representation of the given Real number.
      */
    def toFloat(x: Real): Float =
      toDouble(x).toFloat
  }

  /**
    * CONSIDER inlining this method or making it private.
    *
    * @param x the first number.
    * @param y the second number.
    * @return the order.
    */
  def doCompare(x: Real, y: Real): Int =
    RealIsOrdering.compare(x, y)

  /**
    * Following are the definitions required by Fractional[Real]
    */
  trait RealIsFractional extends Fractional[Real] with RealIsNumeric {
    /**
      * Divides one `Real` number by another.
      *
      * @param x the numerator as a `Real` number
      * @param y the denominator as a `Real` number, should not be zero
      * @return the result of dividing `x` by `y` as a `Real` number
      */
    def div(x: Real, y: Real): Real =
      times(x, createFromRealField(y.invert))
  }

  implicit object RealIsFractional extends RealIsFractional with RealIsNumeric with RealIsOrdering

  /**
    * Represents a constant Real number corresponding to "Not a Number" (NaN).
    *
    * This object is a specialized instance of `Real`, serving as a marker
    * for undefined or unrepresentable numerical values within the Real number context.
    *
    * Inherits all behavior and operations defined by the `Real` class, but
    * specific methods and operations involving NaN may yield NaN or
    * other behavior consistent with the IEEE 754 standard for floating-point arithmetic.
    */
  object NaN extends Real(Number.NaN)
}