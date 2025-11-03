/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{inverse, negate}
import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.inner.Value.{fromDouble, fromInt, fromRational}
import com.phasmidsoftware.number.core.inner._
// TODO eliminate references to expression package
import com.phasmidsoftware.number.expression.{Expression, ExpressionException}
import com.phasmidsoftware.number.misc.FP.{optional, toTry}
import com.phasmidsoftware.number.parse.NumberParser
import com.phasmidsoftware.number.parse.RationalParser.parseComponents
import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.util._

/**
  * Trait to model numbers as a sub-class of Field and such that we can order Numbers.
  * That's to say that Numbers have linear domain and all belong, directly or indirectly, to the set R (real numbers).
  *
  * CONSIDER storing all numbers in the form `r e to the power of i theta`.
  *
  * CONSIDER eliminate extending Field
  *
  * TODO remove references to Expression
  *
  * Every number has three properties:
  * * nominalValue: Value
  * * factor: Factor
  * * fuzz: (from extending Fuzz[Double]).
  */
trait Number extends Fuzz[Double] with Ordered[Number] with Numerical {

  /**
    * The nominal value of this `Number`.
    *
    * @return the nominalValue.
    */
  def nominalValue: Value

  /**
    * The factor of this Number.
    * Ordinary numbers are of PureNumber factor, angles have factor Radian, and natural logs have factor NatLog.
    *
    * @return the factor.
    */
  def factor: Factor

  /**
    * Retrieves an optional Factor instance.
    *
    * @return `Some(factor)`.
    */
  def maybeFactor: Option[Factor] = Some(factor)

  /**
    * Method to determine if this is a valid Number.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  def isValid: Boolean

  /**
    * Method to determine if this is an imaginary Number,
    * that's to say a number with negative value and SquareRoot as its factor.
    *
    * @return true if imaginary.
    */
  def isImaginary: Boolean

  /**
    * Method to make some trivial simplifications of this Number (only if exact).
    *
    * @return either this Number or a simplified Number.
    */
  def simplify: Number

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired.
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int

  /**
    * Method to apply a function to this Number.
    *
    * @param f      a function Double=>Double.
    * @param dfByDx the first derivative of f.
    * @return a Try[Number] which is the result of applying f to this Number.
    */
  def applyFunc(f: Double => Double, dfByDx: Double => Double): Try[Number]

  /**
    * Converts the current number instance into a pure number representation.
    * The conversion is based on the factor and fuzz values associated with the number.
    * NOTE that even if `this Number` is exact, the result may not be.
    *
    * @return A new instance of Number in pure number representation, performing the necessary conversion
    *         and scaling if required.
    */
  def toPureNumber: Number = (factor, fuzz) match {
    case (PureNumber, _) =>
      this
    case (f, z) =>
      f.convert(nominalValue, PureNumber) match {
        case Some(value) =>
          Number.create(value, PureNumber, z)
        case None =>
          scale(PureNumber)
      }
  }

  /**
    * Method to get the nominalValue of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible nominalValue to the nominal nominalValue,
    *         otherwise None if this is invalid.
    */
  def toNominalDouble: Option[Double] = maybeNominalDouble

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  def maybeNominalDouble: Option[Double]

  /**
    * Method to determine if this Number is actually represented is an Integer.
    *
    * @return true if exact and integral.
    */
  def isInteger: Boolean

  /**
    * Method to determine if this Number is actually represented as a Rational.
    *
    * @return true if exact and rational.
    */
  def isRational: Boolean

  /**
    * Method to get the value of this Number as an (optional) Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.convertDouble(x).
    *
    * @return an Option of Rational.
    */
  def toNominalRational: Option[Rational]

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int]

  /**
    * Method to get the value of this Number as an Int.
    *
    * TESTME
    *
    * @return an Option of Long. If this Number cannot be converted to a Long, then None will be returned.
    */
  def toLong: Option[Long] =
    toNominalRational map (_.toLong)

  /**
    * Method to get the value of this Number as an (optional) BigInt.
    * This will return Some(x) only if this is an Int, or a Rational with unit denominator.
    *
    * @return an Option of BigInt.
    */
  def toBigInt: Option[BigInt]

  /**
    * Method to get the value of this Number as an (optional) BigInt.
    * This will return Some(x) only if this is an Int, or a Rational with unit denominator.
    *
    * @return an Option of BigDecimal.
    */
  def toBigDecimal: Option[BigDecimal]

  /**
    * Method to determine if this Number is positive.
    * Use case: does the String representation not start with a "-"?
    *
    * CONSIDER evaluating toString instead.
    *
    * @return true if this Number is greater than or equal to 0.
    */
  def isPositive: Boolean

  /**
    * Negative of this Number.
    */
  def makeNegative: Number

  /**
    * Negates the value of the instance conditionally based on the input parameter.
    *
    * @param cond A boolean parameter that determines whether to negate the value.
    *             If true, the negation function `makeNegative` is applied.
    *             Otherwise, the current instance is returned unchanged.
    * @return The negated value as a `Number` if the condition is true,
    *         or the current instance if the condition is false.
    */
  def negateConditional(cond: Boolean): Number =
    if (cond) makeNegative else this

  /**
    * Add this Number to n.
    *
    * @param n another Number.
    * @return the sum of this and n.
    */
  def doAdd(n: Number): Number

  /**
    * Subtract n from this Number
    *
    * @param n another Number.
    * @return the difference of this and n.
    */
  def doSubtract(n: Number): Number =
    doAdd(negate(n))

  /**
    * Multiply this Number by n.
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number

  /**
    * Perform an exact scalar multiplication of this `Number` by the scale factor `x`.
    * Implemented by `doMultiple(Rational)` where you should consult NOTE
    *
    * @param x an Int.
    * @return a new Number which is this Number scaled by z.
    */
  def doMultiple(x: Int): Number =
    doMultiple(Rational(x))

  /**
    * Perform an exact scalar multiplication of this `Number` by the scale factor `r`.
    *
    * NOTE that numbers with `Logarithmic` factors cannot be scaled exactly in this way.
    *
    * @param r a Rational.
    * @return a new Number which is this Number scaled by z.
    */
  def doMultiple(r: Rational): Number =
    GeneralNumber.times(this, r)

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e., this/n.
    */
  def doDivide(n: Number): Number


  /**
    * Add x to this Number and return the result.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field =
    x match {
      case Real(n) if n.isImaginary =>
        ComplexCartesian.fromImaginary(n) doAdd Complex(this)
      case Real(n) =>
        Real(doAdd(n))
      case c@BaseComplex(_, _) => // TESTME
        c.add(this.asComplex)
      case s: Algebraic =>
        s add Real(this)
      case _ =>
        throw NumberException(s"logic error: add not supported for this addend: $x")
    }

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number

  // NOTE Following are methods defined in Field.

  /**
    * Multiplies the current instance by another Field and returns the result.
    * Handles special cases such as multiplication with zero, one, or specific constants.
    *
    * NOTE this code seems almost identical to the times method in GeneralNumber
    * CONSIDER combining them and using the Factor facilities more.
    *
    * @param x the Field instance to be multiplied with the current instance
    * @return the result of the multiplication as a Field
    */
  def multiply(x: Field): Field = (this, x) match {
    case (Number.zero, _) | (_, Constants.zero) =>
      Constants.zero
    case (Number.one, _) =>
      x
    case (_, Constants.one) =>
      Real(this)
    case (Number.i, Constants.pi) | (Number.pi, Constants.i) =>
      Constants.iPi
    case (n, Constants.i) =>
      n.asComplex.rotate
    case (a@ExactNumber(va, fa), Real(b@ExactNumber(vb, fb))) =>
      fa.multiply(va, vb, fb) match {
        case Some((v, f, _)) =>
          Real(a.make(v, f))
        case None =>
          doMultiply(b).normalize
      }
    case (_, Real(n)) =>
      doMultiply(n).normalize
    case (_, c@BaseComplex(_, _)) =>
      c.multiply(this.asComplex)
    case (_, s: Algebraic) =>
      s multiply Real(this)
    case _ =>
      throw NumberException("logic error: multiply not supported for non-Number multiplicands")
  }

  /**
    * Divide this Number by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = x match {
    case Real(n) =>
      Real(doDivide(n))
    case c@BaseComplex(_, _) => // TESTME
      c.divide(x)
  }

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field =
    Real(makeNegative)

  def power(p: Number): Number = p match {
    case Number.zero =>
      Number.one
    case Number.one =>
      this
    case Number.negOne =>
      inverse(this)
    case Number.two =>
      this doMultiply this
    case _ =>
      doPower(p)
  }

  /**
    * Calculates the square of a number by multiplying the number by itself.
    * CONSIDER different implementations according to the factor.
    *
    * @return the result of squaring this number
    */
  def square: Number = this doMultiply this

  /**
    * Raise this Number to the power p.
    *
    * @param p a Field.
    * @return this Number raised to power p.
    */
  def power(p: Field): Field = p match {
    case Real(n) =>
      Real(doPower(n))
    case ComplexCartesian(x, y) =>
      ComplexPolar(doPower(x), y) // CONSIDER is this correct? // TESTME
    case _ =>
      throw NumberException("logic error: power not supported for non-Number powers")
  }

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    *
    * CONSIDER allowing logarithmic numbers to be inverted simply by changing the sign of the value.
    */
  def invert: Field =
    Real(getInverse)

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is PureNumber, since we cannot directly invert Numbers with other
    * factors.
    *
    * CONSIDER allowing logarithmic numbers to be inverted simply by changing the sign of the value.
    */
  def getInverse: Number =
    Number.inverse(convertToNumber(normalize))

  /**
    * Yields the square root of this Number.
    * If possible, the result will be exact.
    */
  def sqrt: Number

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with PureNumber factor.
    *
    * @return the sine of this.
    */
  def sin: Number

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with PureNumber factor.
    *
    * @return the cosine.
    */
  def cos: Number

  /**
    * Method to determine the tangent of this Number.
    * The result will be a Number with PureNumber factor.
    *
    * @return the tangent
    */
  def tan: Number =
    (nominalValue, factor) match { // CONSIDER modulating first.
      case (Left(Right(r)), Radian) =>
        r match {
          case Rational(Rational.bigOne, Rational.bigThree) =>
            Number.root3
          case Rational(Rational.bigThree, Rational.bigFour) | Rational(Rational.bigSeven, Rational.bigFour) => // TESTME
            negate(Number.one)
          case Rational(Rational.bigOne, Rational.bigFour) | Rational(Rational.bigFive, Rational.bigFour) =>
            Number.one
          case Rational(Rational.bigOne, Rational.bigSix) =>
            inverse(Number.root3)
          case _ =>
            sin doDivide cos
        }
      case _ =>
        sin doDivide cos
    }

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number

  /**
    * Returns the log of this Number in base b.
    *
    * @param b the base for which to calculate the logarithm
    * @return the logarithm of the input number to base b
    */
  def log(b: Number): Number

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with `PureNumber` factor.
    * CONSIDER renaming this as "ln" but keep in mind that the Java library calls the function "log."
    *
    * @return the natural log of this.
    */
  def ln: Field

  /**
    * Method to raise e to the power of this number.
    * The result will be a Number with NatLog factor.
    *
    * @return the e to the power of this.
    */
  def exp: Number

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int

  /**
    * Method to yield the absolute value of this Number.
    *
    * @return this if its positive, else - this.
    */
  def abs: Number

  /**
    * @return Some(this).
    */
  def asNumber: Option[Number] = Some(this)

  /**
    * Method to return this Number as a Complex.
    *
    * @return Complex(this) as appropriate.
    */
  def asComplex: Complex = if (isImaginary)
    ComplexCartesian.fromImaginary(this)
  else if (isZero)
    ComplexCartesian(Number.zero)
  else
    ComplexPolar(this)

  /**
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    * In other words,  in the case where f is not factor, the numerical value of the result's value will be different
    * from this value.
    *
    * @param f the new factor for the result.
    * @return a Number based on this and factor.
    */
  def scale(f: Factor): Number

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * @param other the Number to be compared with.
    * @param p     the confidence expressed as a fraction of 1 (0.5 would be a typical value).
    * @return -1, 0, 1 as usual.
    */
  def fuzzyCompare(other: Number, p: Double): Int

  /**
    * Return optional Fuzziness of Box shape, such that
    * <code>this</code> would be considered just within the resulting tolerance.
    *
    * @param other another Number: the ideal or target value.
    * @return an optional relative Fuzziness.
    */
  def asComparedWith(other: Number): Option[Fuzziness[Double]]

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadic(other: Number, f: Factor)(op: DyadicOperation): Option[Number]

  /**
    * Evaluate a monadic operator on this.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def transformMonadic(f: Factor)(op: MonadicOperation): Option[Number]

  /**
    * Evaluate a query operator on this.
    *
    * @param op the appropriate QueryOperation.
    * @return a T.
    */
  def query[T](op: QueryOperation[T], defaultVal: => T): T

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return either a Number.
    */
  def make(v: Value, f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the factor will change.
    * This method does not need to be followed by a call to specialize.
    *
    * @param f the factor.
    * @return either a Number.
    */
  def make(f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Number.
    */
  def make(v: Value): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return either a Number.
    */
  def make(v: Int, f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Number.
    */
  def make(v: Int): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param r a Rational.
    * @param f Factor.
    * @return either a Number.
    */
  def make(r: Rational, f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Number.
    */
  def make(v: Rational): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value (a Double).
    * @param f Factor.
    * @return either a Number.
    */
  def make(v: Double, f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param v  the value (a Double).
    * @param f  Factor.
    * @param fo optional fuzz.
    * @return either a Number.
    */
  def make(v: Double, f: Factor, fo: Option[Fuzziness[Double]]): Number

  /**
    * Make a copy of this Number, with the same value and factor but with a different value of fuzziness.
    *
    * @param fo the (optional) fuzziness.
    * @return a Number.
    */
  def make(fo: Option[Fuzziness[Double]]): Number

  /**
    * Return a Number which uses the most restricted type possible.
    * A Number based on a Double will yield a Number based on a Rational (if the conversion is exact).
    * A Number based on a Rational will yield a Number based on a BigInt (if there is a unit denominator).
    * A Number based on a BigInt will yield a Number based on a Int (if it is sufficiently small).
    *
    * CONSIDER do we really need this in Number?
    *
    * @return a Number with the same magnitude as this.
    */
  def specialize: Number

  /**
    * Method to ensure that the value is within some factor-specific range.
    * In particular, Radian=based numbers are modulated to the range 0..2
    *
    * @return this or an equivalent Number.
    */
  def modulate: Number

  /**
    * @param p the confidence desired. Ignored if isZero is true.
    * @return true if this Number is equivalent to zero with at least p confidence.
    */
  def isProbablyZero(p: Double = 0.5): Boolean
}

object Number {

  /**
    * Applies the given input to create a Number instance.
    *
    * @param x the input value of type NumberLike, which can be a Rational,
    *          a Number, or another acceptable form from which a Number
    *          can be created.
    * @return a Number instance created from the input x if its type matches
    *         the expected forms. Throws a NumberException otherwise.
    */
  def apply(x: NumberLike): Number = x match {
    case r@Rational(_, _) => zero.make(r)
    case n: Number => n
    case _ => throw NumberException(s"Cannot create a Number from $x")
  }

  /**
    * Provides an implicit object for handling operations on `Number` with fractional, numeric, and ordering capabilities.
    *
    * This implicit object combines the functionality of `NumberIsFractional`, `NumberIsNumeric`, and `NumberIsOrdering`,
    * enabling division operations along with general numeric and ordering functionalities for the `Number` type.
    */
  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  /**
    * Exact value of 0
    */
  lazy val zero: Number = 0
  /**
    * Exact value of -0
    */
  lazy val negZero: Number = Rational.negZero
  /**
    * Exact value of 1
    */
  lazy val one: Number = 1
  /**
    * Exact value of -1
    */
  lazy val negOne: Number = -1
  /**
    * Exact value of 2
    */
  lazy val two: Number = 2
  /**
    * A constant value representing the number three as a `Number` type.
    */
  lazy val three: Number = 3
  /**
    * Exact value of 1/2
    */
  lazy val half: Number = Rational.half
  /**
    * Exact value of 10
    */
  lazy val ten: Number = Rational.ten
  /**
    * Exact value of pi
    */
  lazy val pi: Number = Number(1, Radian)
  /**
    * Exact value of 𝛑
    */
  //noinspection NonAsciiCharacters
  lazy val `𝛑`: Number = pi
  /**
    * Exact value of 2 pi
    */
  lazy val twoPi: Number = Number(2, Radian)
  /**
    * Exact value of pi/2
    */
  lazy val piBy2: Number = Number(Rational.half, Radian)
  /**
    * Exact value of pi/4
    */
  lazy val piBy4: Number = Number(Rational(1, 4), Radian)
  /**
    * Represents the mathematical constant π/3.
    * It is defined as a `Number` with a rational value of 1/3
    * and a unit of measurement as `Radian`.
    */
  lazy val piBy3: Number = Number(Rational(1, 3), Radian)
  /**
    * Exact value of -pi
    */
  lazy val minusPi: Number = negate(pi)
  /**
    * Exact value of zero radians
    */
  lazy val zeroR: Number = Number(0, Radian)
  /**
    * Exact value of e
    */
  lazy val e: Number = ExactNumber(1, NatLog)

  /**
    * Implicit class which takes a Double, and using method ~ and an Int parameter,
    * yields a Number with the appropriate degree of fuzziness.
    * This class provides an alternative to having to parse a fuzzy number from a String.
    *
    * @param x a Double.
    */
  implicit class FuzzOps(x: Double) {
    /**
      * Method to yield a (scalar) Number, whose value is x, and whose fuzziness is Gaussian with standard deviation
      * defined by y.
      * The magnitude of the fuzziness is determined by the number of decimal places of x.
      * However, if the decimal part of the number ends in a zero, you should use the method in FuzzStringOps instead.
      *
      * @param y a two-digit Int.
      * @return a Number with absolute, Gaussian fuzziness, whose std. dev. is y.
      */
    def ~(y: Int): Number =
      if (y >= 10 && y < 100) {
        val p = y * math.pow(10.0, -BigDecimal(x).scale)
        Number(x, PureNumber, Some(AbsoluteFuzz(implicitly[Valuable[Double]].fromDouble(p), Gaussian)))
      }
      else
        throw NumberException(s"The ~ operator for defining fuzz for numbers must be followed by two digits: " + y)
  }

  /**
    * Implicit class to operate on Numbers introduced as integers.
    *
    * CONSIDER generalizing this to inputs of Values (or Rationals, Doubles).
    *
    * @param x an Int to be treated as a Number.
    */
  implicit class NumberOps(x: Int) {

    /**
      * Add this x (a Number) and yield a Number.
      *
      * @param y the addend, a Number.
      * @return a Number whose value is x + y.
      */
    def +(y: Number): Number =
      Number(x) doAdd y

    /**
      * Multiply x by y (a Number) and yield a Number.
      *
      * @param y the multiplicand, a Number.
      * @return a Number whose value is x * y.
      */
    def *(y: Number): Number =
      Number(x) doMultiply y

    /**
      * Divide x by y (a Number) and yield a Number.
      *
      * @param y the divisor, a Number.
      * @return a Number whose value is x / y.
      */
    def /(y: Number): Number =
      convertToNumber(Number(x) multiply y.invert)

    /**
      * Divide x by y (an Int) and yield a Number.
      * NOTE: the colon is necessary in order to coerce the left hand operand to be a Number.
      *
      * @param y the divisor, an Int.
      * @return a Number whose value is x / y.
      */
    def :/(y: Int): Number = /(y)

    /**
      * Raise x to the power of y (an Int) and yield a Number.
      *
      * @param y the exponent, an Int.
      * @return a Number whose value is x / y.
      */
    def ∧(y: Int): Number = x ∧ y // TESTME

    /**
      * Raise x to the power of y (an Rational) and yield a Number.
      *
      * @param y the exponent, a Rational.
      * @return a Number whose value is x / y.
      */
    def ∧(y: Rational): Number = x ∧ y // TESTME
  }

  /**
    * Exact value of i
    */
  val i: Number = ExactNumber(-1, SquareRoot)
  /**
    * Exact value of the Number √2 (not Complex)
    */
  val root2: Number = Number(2, SquareRoot)

  /**
    * Implicit converter from Expression to Number.
    * CONSIDER we should try to move this implicit converter into Expression...
    * but be warned--it's not easy!
    *
    * @param x the Expression to be converted.
    * @return the equivalent exact Number.
    */
  //noinspection Annotator
  implicit def convertExpression(x: Expression): Number =
    x.materialize match {
      case Real(n) =>
        n
      case _ =>
        throw ExpressionException(s"Expression $x cannot be converted implicitly to a Number")
    }

  /**
    * Implicit converter from Int to Number.
    *
    * @param x the Int to be converted.
    * @return the equivalent Number.
    */
  implicit def convertInt(x: Int): Number = Number(x)

  /**
    * Implicit converter from Double to Number.
    * TESTME
    *
    * @param x the Double to be converted.
    * @return the equivalent Number.
    */
  implicit def convertDouble(x: Double): Number = Number(x)

  /**
    * Exact value of √3
    */
  val root3: Number = Number(3, SquareRoot)

  /**
    * Implicit class which takes a String, and using method ~ and an Int parameter,
    * yields a Number with the appropriate degree of fuzziness.
    * This class provides an alternative to having to parse a fuzzy number from a String.
    * Really, the only function it provides is the ability to put the error bounds after the exponent.
    * It is parallel to FuzzOps except for two differences:
    * the input is a String, and this is is so that trailing zeros in the fractional part don't get ignored,
    * thus messing up the fuzziness;
    * And, secondly, the result of ~ is a Try[Number], not just a Number.
    *
    * @param w a String.
    */
  implicit class FuzzStringOps(w: String) {
    /**
      * Method to yield a (scalar) Number, whose value is x, and whose fuzziness is Gaussian with standard deviation
      * defined by y.
      * The magnitude of the fuzziness is determined by the number of decimal places of x.
      *
      * @param n a two-digit Int.
      * @return a Try[Number] with absolute, Gaussian fuzziness, whose std. dev. is y.
      */
    def ~(n: Int): Try[Number] =
      for {
        x <- parse(w) // XXX We don't strictly need this now we also have components
        components <- parseComponents(w)
        f <- toTry(components._3, Failure(NumberException(s"no fractional part: " + w)))
        exp = components._4.getOrElse("0")
        e <- toTry(implicitly[Numeric[Int]].parseString(exp), Failure(NumberException(s"Logic error: " + exp)))
        y <- toTry(optional[Int](x => x >= 10 && x < 100)(n), Failure(NumberException(s"The ~ operator for defining fuzz for numbers must be followed by two digits: " + n)))
        p = y * math.pow(10, e - f.length)
      } yield x.make(Some(AbsoluteFuzz(implicitly[Valuable[Double]].fromDouble(p), Gaussian)))
  }

  /**
    * Exact value of √5
    */
  val root5: Number = Number(5, SquareRoot)

  /**
    * NOTE: this unapply method does not match on the fuzz of a Number.
    *
    * @param arg a Number to be unapplied.
    * @return optional Value and Factor.
    */
  def unapply(arg: Number): Option[(Value, Factor)] =
    Some(arg.nominalValue, arg.factor)

  def createFromDouble(x: Double): Number =
    createFromDouble(x, PureNumber)

  /**
    * CONSIDER why do we need this method?
    *
    * NOTE not all double values should be given fuzz.
    *
    * @param x      a Double.
    * @param factor a Factor.
    * @return a Number formed from x and factor using standard double precision fuzziness.
    */
  def createFromDouble(x: Double, factor: Factor): Number =
    apply(x, factor, Some(Fuzziness.doublePrecision))

  /**
    * Method to construct a Number from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Double, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    x match {
      case Double.NaN =>
        Number(None, factor, fuzz)
      case _ =>
        Number(Some(x), factor, fuzz)
    }

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double], factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    create(fromDouble(xo), factor, fuzz)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value      the value of the Number, expressed as a nested Either type.
    * @param actualFuzz the fuzziness of this Number.
    * @return a Number.
    */
  def create(value: Value, actualFuzz: Fuzziness[Double]): Number =
    create(value, PureNumber, Some(actualFuzz))

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value the value of the Number, expressed as a nested Either type.
    * @return a Number.
    */
  def create(value: Value): Number =
    create(value, PureNumber)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: PureNumber, Radian, and NatLog.
    * @return a Number.
    */
  def create(value: Value, factor: Factor): Number =
    create(value, factor, None)

  /**
    * Method to construct a Number from a String.
    * This is by far the best way of creating the number that you really want.
    *
    * @param x the String representation of the value.
    * @return a Number based on x.
    */
  def apply(x: String, factor: Factor): Number =
    parse(x) match {
      // CONSIDER we should perhaps process n (e.g. to modulate a Radian value)
      case Success(n) =>
        n.make(factor)
      case Failure(e) =>
        throw NumberExceptionWithCause(s"apply(String, Factor): unable to parse $x", e)
    }

  /**
    * Method to construct a Number from a String.
    * This is by far the best way of creating the number that you really want.
    *
    * @param x the String representation of the value.
    * @return a Number based on x.
    */
  def apply(x: String): Number =
    parse(x) match {
      // CONSIDER we should perhaps process n (e.g. to modulate a Radian value)
      case Success(n) =>
        n
      case Failure(e) =>
        throw NumberExceptionWithCause(s"apply(String, Factor): unable to parse $x", e)
    }

  /**
    * Method to construct a unit Number with explicit factor.
    *
    * @param factor the appropriate factor
    * @return a unit Number with the given factor.
    */
  def apply(factor: Factor): Number =
    Number(1, factor)

  /**
    * Method to construct a Number from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Int, factor: Factor): Number =
    Number(x, factor, None)

  /**
    * Method to construct a Number from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Int, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    create(fromInt(x), factor, fuzz)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * CONSIDER modulate the result so that, in the case of a multiple of Radian, we restrict the range to 0 to 2pi immediately.
    * However, note that this will change the behavior such that it is no longer possible to have the constant 2pi.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: PureNumber, Radian, and NatLog.
    * @param fuzz   the fuzziness of this Number, wrapped in Option.
    * @return a Number.
    */
  def create(value: Value, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    (fuzz match {
      case None =>
        ExactNumber(value, factor)
      case _ =>
        FuzzyNumber(value, factor, fuzz)
    }).specialize

  /**
    * Method to construct a Number from an Int.
    *
    * @param x the Int value.
    * @return a Number based on x.
    */
  def apply(x: Int): Number =
    Number(x, PureNumber)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x a BigInt value.
    * @return a Number based on x.
    */
  def apply(x: BigInt): Number =
    Number(x, PureNumber)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigInt, factor: Factor): Number =
    Number(x, factor, None)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigInt, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    apply(Rational(x), factor, fuzz)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x a Rational value.
    * @return a Number based on x.
    */
  def apply(x: Rational): Number =
    Number(x, PureNumber)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Rational, factor: Factor): Number =
    Number(x, factor, None)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x the BigDecimal value.
    * @return a Number based on x.
    */
  def apply(x: BigDecimal): Number =
    Number(x, PureNumber)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigDecimal, factor: Factor): Number =
    Number(x, factor, None)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigDecimal, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    Number(Rational(x), factor, fuzz)

  /**
    * Method to construct a Number from a Rational.
    * NOTE: this method is invoked indirectly by parse(String).
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Rational, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number =
    create(fromRational(x), factor, fuzz)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo an optional Double.
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double]): Number =
    Number(xo, PureNumber)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double], factor: Factor): Number =
    Number(xo, factor, None)

  /**
    * Method to construct a Number from a Double.
    *
    * @param x the Double value.
    * @return a Number based on x.
    */
  def apply(x: Double): Number =
    Number(x, PureNumber)

  /**
    * Method to construct an invalid Number.
    *
    * @return a invalid Number.
    */
  def apply(): Number = NaN

  /**
    * Invalid number.
    */
  val NaN: Number = Number(None)

  /**
    * Represents a constant value for positive infinity.
    * This value is defined using a `Number` wrapper around `Double.PositiveInfinity`.
    */
  val Infinity: Number = Number(Double.PositiveInfinity)

  private val numberParser = NumberParser

  /**
    * Method to parse a String and yield a Try[Number].
    *
    * NOTE: this method indirectly invokes apply(Rational, Factor, Option of Fuzz[Double] )
    *
    * @param w the String to be parsed.
    * @return a Number.
    */
  def parse(w: String): Try[Number] = {
    val ny: Try[Number] = numberParser.parseNumber(w) map (_.specialize)
    ny flatMap (n => if (n.isValid) Success(n) else Failure(NumberException(s"parse: cannot parse $w as a Number")))
  }

  /**
    * Following are the definitions required by Ordering[Number]
    */
  trait NumberIsOrdering extends Ordering[Number] {
    /**
      * When we do a compare on NatLog numbers, they are in the same order as PureNumber numbers (i.e. monotonically increasing).
      * It's not necessary to convert exact numbers to fuzzy numbers for this purpose, we simply
      * pretend that the NatLog numbers are PureNumber numbers.
      *
      * @param x the first Number.
      * @param y the second Number.
      * @return an Int representing the order.
      */
    def compare(x: Number, y: Number): Int = {
      if (x == Number.NaN && y == Number.NaN) 0
      else if (x == Number.NaN || y == Number.NaN) throw NumberException("cannot compare NaN with non-NaN")
      else if (x.factor == NatLog && y.factor == NatLog)
        compare(x.make(PureNumber), y.make(PureNumber)) // TESTME why do we need to convert to PureNumber?
      else if (x.factor == Euler && y.factor == Euler)
        compare(x.make(Radian), y.make(Radian)) // TESTME why do we need to convert to Radian?
      else {
        // CONSIDER invoking the compare method in GeneralNumber.
        GeneralNumber.plus(x, Number.negate(y)).signum
      }
    }
  }

  implicit object NumberIsOrdering extends NumberIsOrdering

  /**
    * Method to construct a Number from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Double, factor: Factor): Number =
    Number(x, factor, None)

  /**
    * CONSIDER inlining this method or making it private.
    *
    * @param x the first number.
    * @param y the second number.
    * @return the order.
    */
  def doCompare(x: Number, y: Number): Int =
    NumberIsOrdering.compare(x, y)

  /**
    * Prepares and specializes a given optional number.
    *
    * @param no An optional value of type Number to be prepared.
    * @return A specialized Number derived from the prepared input.
    */
  def prepareWithSpecialize(no: Option[Number]): Number =
    prepare(no).specialize

  /**
    * Method to deal with a Scale factor change.
    *
    * TODO: this will work for FuzzyNumber but only if the fuzz is relative, and even then perhaps not for NatLog conversions.
    *
    * @param n      the Number to be scaled.
    * @param factor the factor to which it should be converted.
    * @return the resulting Number (equivalent in value, but with a potentially different scale factor).
    */
  def scale(n: Number, factor: Factor): Number =
    (n.factor, factor) match {
      case (a, b) if a == b =>
        n
      case (NatLog, PureNumber) =>
        prepare(n.transformMonadic(factor)(MonadicOperationExp))
      case (PureNumber, NatLog) =>
        prepare(n.transformMonadic(factor)(MonadicOperationLog))
      case (NthRoot(_), PureNumber) if Value.signum(n.nominalValue) < 0 =>
        // CONSIDER we should handle i, the imaginary number here
        Number.NaN
      case (SquareRoot, PureNumber) =>
        prepare(n.transformMonadic(factor)(MonadicOperationSqrt)) // CONSIDER use of convert
      case (InversePower(r), PureNumber) =>
        prepare(n.composeDyadic(r.invert, factor)(DyadicOperationPower)) // CHECK that this handles fuzz correctly
      case (NatLog, Scalar(_)) | (Scalar(_), NatLog) | (Logarithmic(_), NthRoot(_)) =>
        scale(scale(n, PureNumber), factor)
      case (PureNumber, Logarithmic(_)) =>
        scale(scale(n, NatLog), factor)
      case (PureNumber, NthRoot(f)) =>
        convertScalarToRoot(n, factor, f)
      case (NthRoot(f), NatLog) =>
        convertRootToNatLog(n, factor, f)
      case (Scalar(_), Scalar(_)) =>
        prepare(n.factor.convert(n.nominalValue, factor) map (v => n.make(v, factor)))
      case (Logarithmic(_), Logarithmic(_)) =>
        prepare(n.factor.convert(n.nominalValue, factor) map (v => n.make(v, factor)))
      case (pr@InversePower(p), qr@InversePower(q)) =>
        pr.raise(n.nominalValue, Value.fromRational(q / p), PureNumber) match {
          case Some((v, _, _)) =>
            n.make(v, qr)
          case _ =>
            prepare(n.factor.convert(n.nominalValue, factor) map (v => n.make(v, factor)))
        }
      case (Euler, _) | (_, Euler) =>
        throw NumberException(s"Number.scale: scaling between ${n.factor} and $factor factors is not supported")
//        n // CONSIDER will this cause problems? Should we throw an Exception instead?
      case (Logarithmic(_), Scalar(_)) =>
        scale(scale(n, NatLog), factor)
      case (InversePower(_), Logarithmic(_)) =>
        scale(scale(n, NatLog), factor)
      case (InversePower(_), Scalar(_)) =>
        scale(n, factor)
      case _ =>
        throw NumberException(s"Number.scale: scaling between ${n.factor} and $factor factors is not supported")
    }

  // CONSIDER some of the following should probably be moved to GeneralNumber

  /**
    * Method to change the sign of this Number.
    * The meaning of "change the sign" is in terms of pure numbers (PureNumber, Radian).
    * For any other factor, we convert <code>x</code> into PureNumber form (which will most likely introduce fuzziness).
    * Imaginary numbers cannot be negated--they must first be converted to Complex form and then negated.
    *
    * @param x a Number to be negated.
    * @return <code>-x</code> unless the negative cannot be represented, in which case a NumberException will be thrown.
    */
  @tailrec
  def negate(x: Number): Number =
    x.factor match {
      case p@Scalar(_) =>
        prepare(x.transformMonadic(p)(MonadicOperationNegate))
      case NthRoot(_) if Value.signum(x.nominalValue) < 0 =>
        throw NumberException(s"cannot negate imaginary number: $x")
      case _ =>
        negate(x.scale(PureNumber))
    }

  /**
    * Computes the inverse of the given number based on its factor type.
    *
    * @param x the number to be inverted
    * @return the inverted number, processed according to its factor type
    */
  def inverse(x: Number): Number = x.factor match {
    case PureNumber =>
      prepare(x.transformMonadic(PureNumber)(MonadicOperationInvert))
    case f@NthRoot(_) =>
      f.invert(x.nominalValue) match {
        case Some((v, f, _)) =>
          ExactNumber(v, f)
        case None =>
          prepare(x.transformMonadic(f)(MonadicOperationInvert))
      }
    case _ =>
      negate(x.scale(PureNumber))
  }

  /**
    * CONSIDER move this to GeneralNumber as an instance method.
    *
    * @param x a Number.
    * @return -1, 0, or 1 according to its sign.
    */
  def signum(x: Number): Int = x match {
    case z: GeneralNumber =>
      z.query(QueryOperationSignum, 0)
  }

  /**
    * Implement sin of a Number.
    * See [[https://en.wikipedia.org/wiki/Sine_and_cosine]].
    *
    * CONSIDER implementing this (and cos) as part of exp method (providing a Complex parameter, of course).
    *
    * CONSIDER implementing this (and cos) by using MonadicOperationSin throughout. But NOTE that said operation will need enhancement before it can work identically.
    *
    * CONSIDER returning an Expression rather than a Number. That would enable an exact result for 1/12 and 5/12 pi.
    *
    * @param x a Number, typically in Radians, but if not, then will be converted.
    * @return a PureNumber Number which represents the sine of x.
    */
  def sin(x: Number): Number =
    // TODO much of the logic here is a repeat of what's in transformMonadic.
    x.scale(Radian).transformMonadic(Radian)(MonadicOperationModulate(-1, 1, circular = true)) match {
      case Some(z) =>
        if (z.signum >= 0) {
          lazy val oneOverRoot2 = Number(Rational.half, SquareRoot)
          lazy val rootThreeQuarters = Number(Rational(3, 4), SquareRoot)
          lazy val rootSix = Number(6, SquareRoot)
          val z = x.scale(Radian)
          z.doMultiply(12).toInt match {
            case Some(3) | Some(9) =>
              oneOverRoot2  // pi/4 and 3pi/4
            case Some(4) | Some(8) =>
              rootThreeQuarters // pi/3 and 2pi/3
            case Some(1) | Some(11) =>
              rootSix doSubtract root2 doDivide 4 // pi/12 and 11pi/12 would be nice for this to be an Expression
            case Some(5) | Some(7) =>
              rootSix doAdd root2 doDivide 4 // 5pi/12 and 7pi/12 ditto // TESTME
            case _ =>
              prepareWithSpecialize(z.transformMonadic(PureNumber)(MonadicOperationSin)) // this takes proper care of 0, 2, 6, 10, 12.
          }
        } else negate(sin(negate(x)))
      case None =>
        throw NumberException(s"Number.sin: logic error")
    }

  /**
    * Calculates the arctangent of the angle specified by the ratio of y to x.
    * The result is an angle expressed in radians.
    * CONSIDER checking here for x being zero.
    *
    * @param x the horizontal coordinate of the point
    * @param y the vertical coordinate of the point
    * @return the arctangent of the angle, in radians
    */
  def atan(x: Number, y: Number): Number =
    doAtan(y doDivide x, x.signum)

  /**
    * Yield the natural log of x.
    * If the factor is NatLog, then we force the factor to be PureNumber and simplify.
    * Otherwise, if the factor is PureNumber, we first convert it to a NatLog number, then call log recursively.
    * Otherwise, we convert to a PureNumber number and call log recursively.
    *
    * CONSIDER using x.factor.log(x.nominalValue)
    *
    * @param x a Number.
    * @return the natural log of x.
    */
  def log(x: Number): Field = x.factor match {
    case NatLog =>
      Real(x.make(PureNumber).simplify)
    case Euler =>
      ComplexPolar(Number.one, x.make(Radian).simplify)
    case PureNumber =>
      log(x.scale(NatLog))
    case SquareRoot if x.signum < 0 =>
      ComplexPolar(x.make(PureNumber).makeNegative, piBy2, 2).ln
    case NthRoot(r) if x.signum > 0 =>
      Real(log(x.make(PureNumber)) divide Real(r))
    case _ =>
      log(x.scale(PureNumber))
  }

  /**
    * Yield the exponential of x i.e., e to the power of x.
    * If the factor is `PureNumber`, then we force the factor to be `NatLog` and simplify.
    * Otherwise, we convert to a `PureNumber` number and call `exp` recursively.
    *
    * @param x a (real) Number whose factor is NatLog.
    * @return the value of e raised to the power of x.
    */
  @tailrec
  def exp(x: Number): Number = x.factor match {
    case PureNumber =>
      x.make(NatLog).simplify
    case _ =>
      exp(x.scale(PureNumber))
  }

  /**
    * Method to yield the square root of a Number.
    *
    * @param x the Number whose square root we need.
    * @return the square root of x.
    */
  def sqrt(x: Number): Number = x.factor match {
    case PureNumber =>
      x.make(SquareRoot).simplify
    case _ =>
      x.power(Number.half)
  }

  /**
    * This method returns a Number equivalent to x but with the value in an explicit factor-dependent range.
    * Only Radian is currently fixed within a range (-1 -> 1).
    *
    * @param x the Number to operate on.
    * @return either x or a number equivalent to x with value in defined range.
    */
  def modulate(x: Number): Number = x.factor match {
    case Radian | Euler =>
      // CONSIDER using f.modulate(...)
      prepare(x.transformMonadic(x.factor)(MonadicOperationModulate(-1, 1, circular = false)))
    case _ =>
      x
  }

  /**
    * Prepares a valid Number based on the provided optional input.
    *
    * @param no An optional Number that may or may not be defined.
    * @return The provided Number if it exists, otherwise a default Number instance.
    */
  def prepare(no: Option[Number]): Number =
    no.getOrElse(Number())

  /**
    * Calculates the arctangent (atan) of the given number with a specified sign.
    * This method performs recursive computation based on the factor type of the input number.
    *
    * @param number The input number for which to calculate the arctangent.
    * @param sign   The sign modifier that affects the outcome of the arctangent calculation.
    * @return A `Number` representing the computed arctangent in radians. If the input number's factor is not supported or valid, the method may throw an exception.
    */
  @tailrec
  private def doAtan(number: Number, sign: Int): Number = number.factor match {
    case PureNumber =>
      prepareWithSpecialize(number.transformMonadic(Radian)(MonadicOperationAtan(sign))).modulate
    case SquareRoot =>
      val ry = number.toNominalRational map (_.abs) match {
        case Some(Rational(Rational.bigThree, Rational.bigOne)) => //
          Success(Rational(1, 3))
        case Some(Rational(Rational.bigOne, Rational.bigThree)) =>
          Success(Rational(1, 6))
        case None =>
          Failure(NumberException("input to atan is not rational"))
        case _ =>
          Failure(NumberException("rational is not matched"))
      }
      (for (flip <- number.toNominalRational map (_.signum < 0); z <- MonadicOperationAtan(sign).modulateAngle(ry, flip).toOption) yield z) match {
        case Some(r) =>
          Number(r, Radian)
        case None =>
          doAtan(number.scale(PureNumber), sign)
      }
    case _ =>
      throw NumberException("number.factor is not matched")
  }

  /**
    * Computes the square root of the given integer value.
    *
    * @param x The integer value for which the square root is to be computed.
    * @return A Number representing the square root of the input value.
    */
  def √(x: Int): Number = x.sqrt

  /**
    * Following are the definitions required by Numeric[Number]
    */
  trait NumberIsNumeric extends Numeric[Number] with NumberIsOrdering {
    /**
      * Adds two `Number` instances and returns their sum.
      *
      * @param x the first `Number` to be added
      * @param y the second `Number` to be added
      * @return the result of adding `x` and `y` as a `Number`
      */
    def plus(x: Number, y: Number): Number =
      GeneralNumber.plus(x, y)

    /**
      * Subtracts the second number from the first number.
      *
      * @param x the number from which the second number will be subtracted
      * @param y the number to be subtracted from the first number
      * @return the result of subtracting y from x
      */
    def minus(x: Number, y: Number): Number =
      GeneralNumber.plus(x, negate(y))

    /**
      * Negates the given `Number` instance.
      *
      * @param x the `Number` to be negated
      * @return a new `Number` that is the negation of the input `Number`
      */
    def negate(x: Number): Number =
      Number.negate(x)

    /**
      * Multiplies two `Number` values and returns the result.
      *
      * @param x the first `Number` operand
      * @param y the second `Number` operand
      * @return the product of the two given `Number` values
      */
    def times(x: Number, y: Number): Number =
      GeneralNumber.times(x, y)

    /**
      * Creates a `Number` instance from the given integer value.
      *
      * @param x the integer value to be converted to a `Number`
      * @return a `Number` representing the given integer value
      */
    def fromInt(x: Int): Number = Number(x)

    /**
      * Parses the given string representation of a number and attempts to convert it into an Option of `Number`.
      *
      * @param str the string to be parsed as a potential representation of a `Number`
      * @return an `Option` containing the parsed `Number` if conversion is successful, or `None` if parsing fails
      */
    def parseString(str: String): Option[Number] =
      parse(str).toOption

    /**
      * Converts a given `Number` instance to an `Int`.
      *
      * @param x the `Number` to be converted to an `Int`
      * @return the integer representation of the given `Number`
      */
    def toInt(x: Number): Int =
      toLong(x).toInt

    /**
      * Converts the given `Number` instance to a `Long` representation.
      * If the number is a `GeneralNumber` and contains a rational value, the rational is converted to a `Long`.
      * If the rational is absent but a double value is present, it rounds the double to the closest `Long`.
      * Throws an exception if neither a rational nor a valid double representation can be derived.
      *
      * @param x the number to be converted to a `Long`
      * @return the `Long` representation of the given number
      * @throws NumberException if the conversion is not possible
      */
    def toLong(x: Number): Long = x match {
      case z: GeneralNumber =>
        z.maybeNominalRational match {
          case Some(r) =>
            r.toLong
          case None =>
            x.maybeNominalDouble match {
              case Some(z) =>
                Math.round(z)
              case None =>
                throw NumberException("toLong: this is invalid")
            }
        }
    }

    /**
      * Converts the given `Number` instance to a `Double`.
      *
      * @param x the `Number` instance to be converted to a `Double`
      * @return the `Double` representation of the input `Number`
      * @throws NumberException if the `Number` instance cannot be converted to a `Double`
      */
    def toDouble(x: Number): Double = x.maybeNominalDouble match {
      case Some(y) =>
        y
      case None =>
        throw NumberException("toDouble: this is invalid")
    }

    /**
      * Converts a given Number to a Float value by first converting it to a Double and then to a Float.
      *
      * @param x the Number to be converted to a Float
      * @return the Float representation of the given Number
      */
    def toFloat(x: Number): Float = toDouble(x).toFloat
  }

  /**
    * Following are the definitions required by Fractional[Number]
    */
  trait NumberIsFractional extends Fractional[Number] with NumberIsNumeric {
    /**
      * Divides one Number by another Number.
      *
      * @param x the numerator, represented as a Number
      * @param y the denominator, represented as a Number, which must be invertible
      * @return the result of dividing x by y, represented as a Number
      */
    def div(x: Number, y: Number): Number =
      GeneralNumber.times(x, y getInverse)
  }

  /**
    * Converts a scalar number to a root factor representation by applying a power function
    * and associating it with a given factor.
    *
    * @param n      the scalar number to be converted
    * @param factor the factor to associate with the result
    * @param f      the exponent to be applied during conversion
    */
  private def convertScalarToRoot(n: Number, factor: Factor, f: Double) =
    n.doPower(ExactNumber(Value.fromDouble(Some(f)), PureNumber)).make(factor)

  /**
    * Converts the root representation of a number into its natural logarithm, scaled by a given factor.
    *
    * @param n      the number to be converted, containing a possible double value
    * @param factor a factor used during the conversion process
    * @param f      a scaling factor for adjusting the natural logarithm
    */
  private def convertRootToNatLog(n: Number, factor: Factor, f: Double) = {
    val yo = for (x <- n.maybeNominalDouble; z = math.log(x)) yield z / f
    n.make(Value.fromDouble(yo), factor).specialize
  }
}

case class NumberException(str: String) extends Exception(str)

case class NumberExceptionWithCause(str: String, e: Throwable) extends Exception(str, e)
