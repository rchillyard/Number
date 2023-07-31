/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.{optional, toTry}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{inverse, negate}
import com.phasmidsoftware.number.core.Value.{fromDouble, fromInt, fromRational}
import com.phasmidsoftware.number.parse.NumberParser
import com.phasmidsoftware.number.parse.RationalParser.parseComponents
import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.util._

/**
  * Trait to model numbers as a sub-class of Field and such that we can order Numbers.
  * That's to say that Numbers have linear domain and all belong, directly or indirectly, to the set R (real numbers).
  *
  * CONSIDER eliminate extending Field
  *
  * Every number has three properties:
  * * value: Value
  * * factor: Factor
  * * fuzz: (from extending Fuzz[Double]).
  */
trait Number extends Fuzz[Double] with Ordered[Number] with Numerical {

  /**
    * The value of this Number.
    *
    * @return the value.
    */
  def value: Value

  /**
    * The factor of this Number.
    * Ordinary numbers are of Scalar factor, angles have factor Radian, and natural logs have factor NatLog.
    *
    * @return the factor.
    */
  def factor: Factor

  /**
    * Method to determine if this is a valid Number.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  def isValid: Boolean

  /**
    * Method to determine if this is an imaginary Number,
    * that's to say a number with negative value and Root2 as its factor.
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
    * Method to apply a function to this Number.
    *
    * @param f      a function Double=>Double.
    * @param dfByDx the first derivative of f.
    * @return a Try[Number] which is the result of applying f to this Number.
    */
  def applyFunc(f: Double => Double, dfByDx: Double => Double): Try[Number]

  /**
    * Method to get the value of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */
  def toDouble: Option[Double]

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  def maybeDouble: Option[Double]

  /**
    * Method to determine if this Number is actually represented as an Integer.
    *
    * @return true if exact and rational.
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
  def toRational: Option[Rational]

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int]

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Long. If this Number cannot be converted to a Long, then None will be returned.
    */
  def toLong: Option[Long] = toRational map (_.toLong)

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
  def doSubtract(n: Number): Number = doAdd(negate(n))

  /**
    * Multiply this Number by n.
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number

  /**
    * Perform an exact scalar multiplication of this Number by the scale factor z.
    *
    * @param r a Rational.
    * @return a new Number which is this Number scaled by z.
    */
  def doMultiple(r: Rational): Number = GeneralNumber.times(this, r)

  /**
    * Perform an exact scalar multiplication of this Number by the scale factor z.
    *
    * @param x an Int.
    * @return a new Number which is this Number scaled by z.
    */
  def doMultiple(x: Int): Number = doMultiple(Rational(x))

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e. this/n.
    */
  def doDivide(n: Number): Number

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    *
    * CONSIDER allowing logarithmic numbers to be inverted simply by changing the sign of the value.
    */
  def doInvert: Number = Number.inverse(convertToNumber(normalize))

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number

  // NOTE Following are methods defined in Field.

  /**
    * Add x to this Number and return the result.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = x match {
    case Real(n) if n.isImaginary => ComplexCartesian.fromImaginary(n) doAdd Complex(this)
    case Real(n) => Real(doAdd(n))
    case c@BaseComplex(_, _) => c.add(this.asComplex)
  }

  /**
    * Multiply this Number by x and return the result.
    *
    * @param x the multiplicand.
    * @return the product.
    */
  def multiply(x: Field): Field = (this, x) match {
    case (Number.zero, _) | (_, Constants.zero) => Constants.zero
    case (Number.one, _) => x
    case (_, Constants.one) => Real(this)
    case (Number.i, Constants.pi) | (Number.pi, Constants.i) => Constants.iPi
    case (n, Constants.i) => n.asComplex.rotate
    case (Number.i, _) => x.multiply(ComplexCartesian(0, 1))
    case (_, Real(n)) => doMultiply(n).normalize
    case (_, c@BaseComplex(_, _)) => c.multiply(this.asComplex)
  }

  /**
    * Divide this Number by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = x match {
    case Real(n) => Real(doDivide(n))
    case c@BaseComplex(_, _) => c.divide(x)
  }

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field = Real(makeNegative)

  def power(p: Number): Number = p match {
    case Number.zero => Number.one
    case Number.one => this
    case Number.negOne => inverse(this)
    case Number.two => this doMultiply this
    case _ => doPower(p)
  }

  /**
    * Raise this Number to the power p.
    *
    * @param p a Field.
    * @return this Number raised to power p.
    */
  def power(p: Field): Field = p match {
    case Real(n) => Real(doPower(n))
    case ComplexCartesian(x, y) => ComplexPolar(doPower(x), y) // CONSIDER is this correct?
    case _ => throw NumberException("logic error: power not supported for non-Number powers")
  }

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    *
    * CONSIDER allowing logarithmic numbers to be inverted simply by changing the sign of the value.
    */
  def invert: Field = Real(doInvert)

  /**
    * Yields the square root of this Number.
    * If possible, the result will be exact.
    */
  def sqrt: Number

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the sine of this.
    */
  def sin: Number

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the cosine.
    */
  def cos: Number

  /**
    * Method to determine the tangent of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the tangent
    */
  def tan: Number = (value, factor) match { // CONSIDER modulating first.
    case (Left(Right(r)), Radian) => r match {
      case Rational(Rational.bigOne, Rational.bigFour) | Rational(Rational.bigFive, Rational.bigFour) => Number.one
      case Rational(Rational.bigThree, Rational.bigFour) | Rational(Rational.bigSeven, Rational.bigFour) => negate(Number.one)
      case _ => sin doDivide cos
    }
    case _ => sin doDivide cos
  }

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the natural log of this.
    */
  def log: Number

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
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired.
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int

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
    * TESTME
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
    * TESTME
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
  def isProbablyZero(p: Double): Boolean
}

object Number {

  /**
    * NOTE: this unapply method does not match on the fuzz of a Number.
    *
    * @param arg a Number to be unapplied.
    * @return optional value and factor.
    */
  def unapply(arg: Number): Option[(Value, Factor)] = Some(arg.value, arg.factor)

  /**
    * Exact value of 0
    */
  val zero: Number = 0
  /**
    * Exact value of -0
    */
  val negZero: Number = Rational.negZero
  /**
    * Exact value of 1
    */
  val one: Number = 1
  /**
    * Exact value of -1
    */
  val negOne: Number = -1
  /**
    * Exact value of 2
    */
  val two: Number = 2
  /**
    * Exact value of 1/2
    */
  val half: Number = Rational.half
  /**
    * Exact value of 10
    */
  val ten: Number = Rational.ten
  /**
    * Exact value of pi
    */
  val pi: Number = Number(1, Radian)
  /**
    * Exact value of 𝛑
    */
  //noinspection NonAsciiCharacters
  val `𝛑`: Number = pi
  /**
    * Exact value of 2 pi
    */
  val twoPi: Number = Number(2, Radian)
  /**
    * Exact value of pi/2
    */
  val piBy2: Number = Number(Rational.half, Radian)
  /**
    * Exact value of -pi
    */
  val minusPi: Number = negate(pi)
  /**
    * Exact value of zero radians
    */
  val zeroR: Number = Number(0, Radian)
  /**
    * Exact value of e
    */
  val e: Number = ExactNumber(1, NatLog)
  /**
    * Exact value of i
    */
  val i: Number = ExactNumber(-1, Root2)
  /**
    * Exact value of the Number √2 (not Complex)
    */
  val root2: Number = Number(2, Root2)
  /**
    * Exact value of √3
    */
  val root3: Number = Number(3, Root2)
  /**
    * Exact value of √5
    */
  val root5: Number = Number(5, Root2)

  /**
    * Implicit converter from Expression to Number.
    *
    * @param x the Expression to be converted.
    * @return the equivalent Number.
    * @throws ExpressionException if x cannot be converted to a Number.
    */
  //noinspection Annotator
  implicit def convertExpression(x: Expression): Number = x.materialize.asNumber match {
    case Some(n) => n
    case None => throw ExpressionException(s"Expression $x cannot be converted implicitly to a Number")
  }

  /**
    * Implicit converter from Int to Number.
    *
    * @param x the Int to be converted.
    * @return the equivalent Number.
    */
  implicit def convertInt(x: Int): Number = Number(x)

//  implicit def convertToReal(x: Number): Real = Real(x)

//  implicit def convertToField(x: Number): Field = Real(x)

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
    def ~(y: Int): Number = if (y >= 10 && y < 100) {
      val p = y * math.pow(10.0, -BigDecimal(x).scale)
      Number(x, Scalar, Some(AbsoluteFuzz(implicitly[Valuable[Double]].fromDouble(p), Gaussian)))
    }
    else
      throw NumberException(s"The ~ operator for defining fuzz for numbers must be followed by two digits: " + y)
  }

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
        x <- parse(w) // We don't strictly need this now we also have components
        components <- parseComponents(w)
        f <- toTry(components._3, Failure(NumberException(s"no fractional part: " + w)))
        exp = components._4.getOrElse("0")
        e <- toTry(implicitly[Numeric[Int]].parseString(exp), Failure(NumberException(s"Logic error: " + exp)))
        y <- toTry(optional[Int](x => x >= 10 && x < 100)(n), Failure(NumberException(s"The ~ operator for defining fuzz for numbers must be followed by two digits: " + n)))
        p = y * math.pow(10, e - f.length)
      } yield x.make(Some(AbsoluteFuzz(implicitly[Valuable[Double]].fromDouble(p), Gaussian)))
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
    def +(y: Number): Number = Number(x) doAdd y

    /**
      * Multiply x by y (a Number) and yield a Number.
      *
      * @param y the multiplicand, a Number.
      * @return a Number whose value is x * y.
      */
    def *(y: Number): Number = Number(x) doMultiply y

    /**
      * Divide x by y (a Number) and yield a Number.
      *
      * @param y the divisor, a Number.
      * @return a Number whose value is x / y.
      */
    def /(y: Number): Number = convertToNumber(Number(x) multiply y.invert)

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
    def ^(y: Int): Number = x ^ y

    /**
      * Raise x to the power of y (an Rational) and yield a Number.
      *
      * @param y the exponent, a Rational.
      * @return a Number whose value is x / y.
      */
    def ^(y: Rational): Number = x ^ y
  }

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * CONSIDER modulate the result so that, in the case of a multiple of Radian, we restrict the range to 0 to 2pi immediately.
    * However, note that this will change the behavior such that it is no longer possible to have the constant 2pi.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: Scalar, Radian, and NatLog.
    * @param fuzz   the fuzziness of this Number, wrapped in Option.
    * @return a Number.
    */
  def create(value: Value, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = (fuzz match {
    case None => ExactNumber(value, factor)
    case _ => FuzzyNumber(value, factor, fuzz)
  }).specialize

  /**
    * CONSIDER why do we need this method?
    *
    * NOTE not all double values should be given fuzz.
    *
    * @param x      a Double.
    * @param factor a Factor.
    * @return a Number formed from x and factor using standard double precision fuzziness.
    */
  def createFromDouble(x: Double, factor: Factor): Number = apply(x, factor, Some(Fuzziness.doublePrecision))

  def createFromDouble(x: Double): Number = createFromDouble(x, Scalar)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: Scalar, Radian, and NatLog.
    * @return a Number.
    */
  def create(value: Value, factor: Factor): Number = create(value, factor, None)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value      the value of the Number, expressed as a nested Either type.
    * @param actualFuzz the fuzziness of this Number.
    * @return a Number.
    */
  def create(value: Value, actualFuzz: Fuzziness[Double]): Number = create(value, Scalar, Some(actualFuzz))

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value the value of the Number, expressed as a nested Either type.
    * @return a Number.
    */
  def create(value: Value): Number = create(value, Scalar)

  /**
    * Method to construct a Number from a String.
    * This is by far the best way of creating the number that you really want.
    *
    * @param x the String representation of the value.
    * @return a Number based on x.
    */
  def apply(x: String, factor: Factor): Number = parse(x) match {
    // CONSIDER we should perhaps process n (e.g. to modulate a Radian value)
    case Success(n) => n.make(factor)
    case Failure(e) => throw NumberExceptionWithCause(s"apply(String, Factor): unable to parse $x", e)
  }

  /**
    * Method to construct a Number from a String.
    * This is by far the best way of creating the number that you really want.
    *
    * @param x the String representation of the value.
    * @return a Number based on x.
    */
  def apply(x: String): Number = parse(x) match {
    // CONSIDER we should perhaps process n (e.g. to modulate a Radian value)
    case Success(n) => n
    case Failure(e) => throw NumberExceptionWithCause(s"apply(String, Factor): unable to parse $x", e)
  }

  /**
    * Method to construct a Number from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Int, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = create(fromInt(x), factor, fuzz)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigInt, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = apply(Rational(x), factor, fuzz)

  /**
    * Method to construct a Number from a Rational.
    * NOTE: this method is invoked indirectly by parse(String).
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Rational, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = create(fromRational(x), factor, fuzz)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigDecimal, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = Number(Rational(x), factor, fuzz)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double], factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = create(fromDouble(xo), factor, fuzz)

  /**
    * Method to construct a Number from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Double, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = x match {
    case Double.NaN => Number(None, factor, fuzz)
    case _ => Number(Some(x), factor, fuzz)
  }

  /**
    * Method to construct a Number from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Int, factor: Factor): Number = Number(x, factor, None)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigInt, factor: Factor): Number = Number(x, factor, None)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Rational, factor: Factor): Number = Number(x, factor, None)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigDecimal, factor: Factor): Number = Number(x, factor, None)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double], factor: Factor): Number = Number(xo, factor, None)

  /**
    * Method to construct a Number from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Double, factor: Factor): Number = Number(x, factor, None)

  /**
    * Method to construct a unit Number with explicit factor.
    *
    * @param factor the appropriate factor
    * @return a unit Number with the given factor.
    */
  def apply(factor: Factor): Number = Number(1, factor)

  /**
    * Method to construct a Number from an Int.
    *
    * @param x the Int value.
    * @return a Number based on x.
    */
  def apply(x: Int): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x a BigInt value.
    * @return a Number based on x.
    */
  def apply(x: BigInt): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x a Rational value.
    * @return a Number based on x.
    */
  def apply(x: Rational): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x the BigDecimal value.
    * @return a Number based on x.
    */
  def apply(x: BigDecimal): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo an optional Double.
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double]): Number = Number(xo, Scalar)

  /**
    * Method to construct a Number from a Double.
    *
    * @param x the Double value.
    * @return a Number based on x.
    */
  def apply(x: Double): Number = Number(x, Scalar)

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
      * When we do a compare on NatLog numbers, they are in the same order as Scalar numbers (i.e. monotonically increasing).
      * It's not necessary to convert exact numbers to fuzzy numbers for this purpose, we simply
      * pretend that the NatLog numbers are Scalar numbers.
      *
      * @param x the first Number.
      * @param y the second Number.
      * @return an Int representing the order.
      */
    def compare(x: Number, y: Number): Int = {
      if (x == Number.NaN && y == Number.NaN) 0
      else if (x == Number.NaN || y == Number.NaN) throw NumberException("cannot compare NaN with non-NaN")
      else if (x.factor == NatLog && y.factor == NatLog)
        compare(x.make(Scalar), y.make(Scalar)) // TESTME why do we need to convert to Scalar?
      else {
        // CONSIDER invoking the compare method in GeneralNumber.
        GeneralNumber.plus(x, Number.negate(y)).signum
      }
    }
  }

  implicit object NumberIsOrdering extends NumberIsOrdering

  /**
    * Following are the definitions required by Numeric[Number]
    */
  trait NumberIsNumeric extends Numeric[Number] with NumberIsOrdering {
    def plus(x: Number, y: Number): Number = GeneralNumber.plus(x, y)

    def minus(x: Number, y: Number): Number = GeneralNumber.plus(x, negate(y))

    def times(x: Number, y: Number): Number = GeneralNumber.times(x, y)

    def negate(x: Number): Number = Number.negate(x)

    def fromInt(x: Int): Number = Number(x)

    def parseString(str: String): Option[Number] = parse(str).toOption

    def toInt(x: Number): Int = toLong(x).toInt

    def toLong(x: Number): Long = x match {
      case z: GeneralNumber => z.maybeRational match {
        case Some(r) => r.toLong
        case None => x.maybeDouble match {
          case Some(z) => Math.round(z)
          case None => throw NumberException("toLong: this is invalid")
        }
      }
    }

    def toDouble(x: Number): Double = x.maybeDouble match {
      case Some(y) => y
      case None => throw NumberException("toDouble: this is invalid")
    }

    def toFloat(x: Number): Float = toDouble(x).toFloat
  }

  /**
    * CONSIDER inlining this method or making it private.
    *
    * @param x the first number.
    * @param y the second number.
    * @return the order.
    */
  def doCompare(x: Number, y: Number): Int = NumberIsOrdering.compare(x, y)

  /**
    * Following are the definitions required by Fractional[Number]
    */
  trait NumberIsFractional extends Fractional[Number] with NumberIsNumeric {
    def div(x: Number, y: Number): Number = GeneralNumber.times(x, y doInvert)
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  // CONSIDER some of the following should probably be moved to GeneralNumber

  def prepare(no: Option[Number]): Number = no.getOrElse(Number())

  def prepareWithSpecialize(no: Option[Number]): Number = prepare(no).specialize

  /**
    * Method to deal with a Scale factor change.
    *
    * TODO: this will work for FuzzyNumber but only if the fuzz is relative, and even then perhaps not for NatLog conversions.
    *
    * @param n      the Number to be scaled.
    * @param factor the factor to which it should be converted.
    * @return the resulting Number (equivalent in value, but with a potentially different scale factor).
    */
  def scale(n: Number, factor: Factor): Number = (n.factor, factor) match {
    case (a, b) if a == b => n
    case (NatLog, Scalar) => prepare(n.transformMonadic(factor)(MonadicOperationExp))
    case (Scalar, NatLog) => prepare(n.transformMonadic(factor)(MonadicOperationLog))
    case (Root(_), Scalar) if Value.signum(n.value) < 0 => Number.NaN
    case (Root2, Scalar) => prepare(n.transformMonadic(factor)(MonadicOperationSqrt))
    case (NatLog, PureNumber(_)) | (PureNumber(_), NatLog) | (Logarithmic(_), Root(_)) => scale(scale(n, Scalar), factor)
    case (Scalar, Logarithmic(_)) => scale(scale(n, NatLog), factor)
    case (Scalar, Root(f)) => convertScalarToRoot(n, factor, f)
    case (Root(f), NatLog) => convertRootToNatLog(n, factor, f)
    case (PureNumber(_), PureNumber(_)) => prepare(n.factor.convert(n.value, factor) map (v => n.make(v, factor)))
    case (Logarithmic(_), Logarithmic(_)) => prepare(n.factor.convert(n.value, factor) map (v => n.make(v, factor)))
    case (Root(_), Root(_)) => prepare(n.factor.convert(n.value, factor) map (v => n.make(v, factor)))
    case (Logarithmic(_), PureNumber(_)) | (Root(_), Logarithmic(_)) | (Root(_), PureNumber(_)) => scale(scale(n, NatLog), factor)
    case _ => throw NumberException(s"Number.scale: scaling between ${n.factor} and $factor factors is not supported")
  }

  /**
    * Method to change the sign of this Number.
    * The meaning of "change the sign" is in terms of pure numbers (Scalar, Radian).
    * For any other factor, we convert <code>x</code> into Scalar form (which will most likely introduce fuzziness).
    * Imaginary numbers cannot be negated--they must first be converted to Complex form and then negated.
    *
    * @param x a Number to be negated.
    * @return <code>-x</code> unless the negative cannot be represented, in which case a NumberException will be thrown.
    */
  @tailrec
  def negate(x: Number): Number = x.factor match {
    case p@PureNumber(_) => prepare(x.transformMonadic(p)(MonadicOperationNegate))
    case Root(_) if Value.signum(x.value) < 0 => throw NumberException(s"cannot negate imaginary number: $x")
    case _ => negate(x.scale(Scalar))
  }

  def inverse(x: Number): Number = x.factor match {
    case Scalar => prepare(x.transformMonadic(Scalar)(MonadicOperationInvert))
    case f@Root(_) => prepare(x.transformMonadic(f)(MonadicOperationInvert))
    case _ => negate(x.scale(Scalar))
  }

  /**
    * CONSIDER move this to GeneralNumber as an instance method.
    *
    * @param x a Number.
    * @return -1, 0, or 1 according to its sign.
    */
  def signum(x: Number): Int = x match {
    case z: GeneralNumber => z.query(QueryOperationSignum, 0)
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
    * @return a Scalar Number which represents the sine of x.
    */
  def sin(x: Number): Number =
  // TODO much of the logic here is a repeat of what's in transformMonadic.
    x.scale(Radian).transformMonadic(Radian)(MonadicOperationModulate(-1, 1, circular = true)) match {
      case Some(z) =>
        if (z.signum >= 0) {
          lazy val oneOverRoot2 = Number(Rational.half, Root2)
          lazy val rootThreeQuarters = Number(Rational(3, 4), Root2)
          lazy val rootSix = Number(6, Root2)
          val z = x.scale(Radian)
          z.doMultiply(12).toInt match {
            case Some(3) | Some(9) => oneOverRoot2  // pi/4 and 3pi/4
            case Some(4) | Some(8) => rootThreeQuarters // pi/3 and 2pi/3
            case Some(1) | Some(11) => rootSix doSubtract root2 doDivide 4 // pi/12 and 11pi/12 would be nice for this to be an Expression
            case Some(5) | Some(7) => rootSix doAdd root2 doDivide 4 // 5pi/12 and 7pi/12 ditto
            case _ => prepareWithSpecialize(z.transformMonadic(Scalar)(MonadicOperationSin)) // this takes proper care of 0, 2, 6, 10, 12.
          }
        } else negate(sin(negate(x)))

      case None => throw NumberException(s"Number.sin: logic error")
    }

  // CONSIDER checking here for x being zero
  def atan(x: Number, y: Number): Number = doAtan(y doDivide x, x.signum)

  /**
    * Yield the natural log of x.
    * If the factor is NatLog, then we force the factor to be Scalar and simplify.
    * Otherwise, if the factor is Scalar, we first convert it to a NatLog number, then call log recursively.
    * Otherwise, we convert to a Scalar number and call log recursively.
    *
    * @param x a Number.
    * @return the natural log of x.
    */
  @tailrec
  def log(x: Number): Number = x.factor match {
    case NatLog => x.make(Scalar).simplify
    case Scalar => log(x.scale(NatLog))
    case _ => log(x.scale(Scalar))
  }

  /**
    * Yield the exponential of x i.e. e to the power of x.
    * If the factor is Scalar, then we force the factor to be NatLog and simplify.
    * Otherwise, we convert to a Scalar number and call exp recursively.
    *
    * @param x a Number whose factor is NatLog.
    * @return the value of e raised to the power of x.
    */
  @tailrec
  def exp(x: Number): Number = x.factor match {
    case Scalar => x.make(NatLog).simplify
    case _ => exp(x.scale(Scalar))
  }

  /**
    * Method to yield the square root of a Number.
    *
    * @param x the Number whose square root we need.
    * @return the square root of x.
    */
  def sqrt(x: Number): Number = x.factor match {
    case Scalar => x.make(Root2).simplify
    case _ => x.power(Number.half)
  }

  def √(x: Int): Number = x.sqrt

  /**
    * This method returns a Number equivalent to x but with the value in an explicit factor-dependent range.
    * Only Radian is currently fixed within a range (-1 -> 1).
    *
    * @param x the Number to operate on.
    * @return either x or a number equivalent to x with value in defined range.
    */
  def modulate(x: Number): Number = x.factor match {
    case f@Radian => prepare(x.transformMonadic(f)(MonadicOperationModulate(-1, 1, circular = true)))
    case _ => x
  }

  @tailrec
  private def doAtan(number: Number, sign: Int): Number = number.factor match {
    case Scalar => prepareWithSpecialize(number.transformMonadic(Radian)(MonadicOperationAtan(sign))).modulate
    case Root(2) =>
      val ro = number.toRational
      val ry: Try[Rational] = ro map (_.abs) match {
        case Some(Rational(Rational.bigThree, Rational.bigOne)) => Success(Rational(1, 3))
        case Some(Rational(Rational.bigOne, Rational.bigThree)) => Success(Rational(1, 6))
        case None => Failure(NumberException("input to atan is not rational"))
        case _ => Failure(NumberException("rational is not matched"))
      }
      (for (flip <- ro map (_.signum < 0); z <- MonadicOperationAtan(sign).modulateAngle(ry, flip).toOption) yield z) match {
        case Some(r) => Number(r, Radian)
        case None => doAtan(number.scale(Scalar), sign)
      }
    case _ => throw NumberException("number.factor is not matched")
  }

  private def convertScalarToRoot(n: Number, factor: Factor, f: Double) =
    n.doPower(ExactNumber(Value.fromDouble(Some(f)), Scalar)).make(factor)

  private def convertRootToNatLog(n: Number, factor: Factor, f: Double) = {
    val yo = for (x <- n.maybeDouble; z = math.log(x)) yield z / f
    n.make(Value.fromDouble(yo), factor).specialize
  }
}

case class NumberException(str: String) extends Exception(str)

case class NumberExceptionWithCause(str: String, e: Throwable) extends Exception(str, e)
