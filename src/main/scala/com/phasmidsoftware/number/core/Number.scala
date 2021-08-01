package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP.{identityTry, tryF}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.FuzzyNumber.NumberIsFuzzy
import com.phasmidsoftware.number.core.Rational.toInts
import com.phasmidsoftware.number.core.Render.renderValue
import com.phasmidsoftware.number.core.Value.{fromDouble, fromInt, fromRational}
import com.phasmidsoftware.number.parse.NumberParser

import scala.annotation.tailrec
import scala.math.BigInt
import scala.util._

/**
  * Trait to model numbers as a sub-class of Field and such that we can order Numbers.
  *
  * Every number has three properties:
  * * value: Value
  * * factor: Factor
  * * fuzz: (from extending Fuzz[Double]).
  */
trait Number extends Fuzz[Double] with Field with Ordered[Number] {

  /**
    * The value of this Number.
    *
    * @return the value.
    */
  def value: Value

  /**
    * The factor of this Number.
    * Ordinary numbers are of Scalar factor, angles have factor Pi, and natural logs have factor E.
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
    * Method to get the value of this Number as a Rational.
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
    * Multiply this Number by n.
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  def doMultiply(n: Number): Number

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e. this/n.
    */
  def doDivide(n: Number): Number

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  def doPower(p: Number): Number

  // NOTE Following are methods defined in Field.

  /**
    * @return true if this Number is equal to zero.
    */
  def isInfinite: Boolean = Number.isInfinite(this)

  /**
    * @return true if this Number is equal to zero.
    */
  def isZero: Boolean = Number.isZero(this)

  /**
    * @return true if there is no fuzz.
    */
  def isExact: Boolean = fuzz.isEmpty

  /**
    * Add x to this Number and return the result.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = x match {
    case n@Number(_, _) => doAdd(n)
    case c@Complex(_, _) => c.add(x)
  }

  /**
    * Multiply this Number by x and return the result.
    *
    * @param x the multiplicand.
    * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case n@Number(_, _) => doMultiply(n)
    case c@Complex(_, _) => c.multiply(x)
  }

  /**
    * Divide this Number by x and return the result.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = x match {
    case n@Number(_, _) => doDivide(n)
    case c@Complex(_, _) => c.divide(x)
  }

  /**
    * Change the sign of this Number.
    */
  def unary_- : Field = makeNegative

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Number): Field = p match {
    case n@Number(_, _) => doPower(n)
    case _ => throw NumberException("logic error: power not supported for non-Number powers")
  }

  /**
    * Raise this Number to the power p.
    *
    * @param p an Int.
    * @return this Number raised to power p.
    */
  def power(p: Int): Field = power(Number(p))

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: Number = Number.inverse(normalize)

  /**
    * Yields the square root of this Number.
    * If possible, the result will be exact.
    */
  def sqrt: Number

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
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
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the natural log of this.
    */
  def log: Number

  /**
    * Method to raise E to the power of this number.
    * The result will be a Number with E factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
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
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    * In other words,  in the case where f is not factor, the numerical value of the result's value will be different
    * from this value.
    *
    * NOTE that the value 0 (which represents 1 times the double-precision tolerance) is a guess.
    * It may not be appropriate for all invocations.
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
    * Be careful when implementing this method that you do not invoke a method recursively.
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return a Number which is the the result, possibly fuzzy, of invoking f on this.
    */
  protected def makeFuzzyIfAppropriate(f: Number => Number, relativePrecision: Int): Number

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
    * TEST me
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
    * NOTE that the value 0 (which represents 1 times the double-precision tolerance) is a guess.
    * It may not be appropriate for all invocations.
    *
    * @param v the value (a Double).
    * @param f Factor.
    * @return either a Number.
    */
  def make(v: Double, f: Factor): Number

  /**
    * Make a copy of this Number, with the same value and factor but with a different value of fuzziness.
    *
    * TEST me
    *
    * @param fo the (optional) fuzziness.
    * @return a Number.
    */
  def make(fo: Option[Fuzziness[Double]]): Number

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  def normalize: Number

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
    * In particular, Pi=based numbers are modulated to the range 0..2
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
  def unapply(arg: Number): Option[(Value, Factor)] = Some(arg.value, arg.factor)

  /**
    * Exact value of 0
    */
  val zero: Number = ExactNumber(Right(0), Scalar)
  /**
    * Exact value of -0
    */
  val negZero: Number = ExactNumber(Left(Right(Rational(0, -1))), Scalar)
  /**
    * Exact value of 1
    */
  val one: Number = ExactNumber(Right(1), Scalar)
  /**
    * Exact value of 1
    */
  val two: Number = ExactNumber(Right(2), Scalar)
  /**
    * Exact value of pi
    */
  val pi: Number = ExactNumber(Right(1), Pi)

  /**
    * Exact value of e
    */
  val e: Number = ExactNumber(Right(1), E)

  implicit class NumberOps(x: Int) {

    /**
      * Add this x (a Number) and yield a Number.
      *
      * @param y the addend, a Number.
      * @return a Number whose value is x + y.
      */
    def +(y: Number): Number = Number(x) doAdd y //.materialize

    /**
      * Multiply x by y (a Number) and yield a Number.
      *
      * @param y the multiplicand, a Number.
      * @return a Number whose value is x * y.
      */
    def *(y: Number): Number = Number(x) doMultiply y //.materialize

    /**
      * Divide x by y (a Number) and yield a Number.
      *
      * @param y the divisor, a Number.
      * @return a Number whose value is x / y.
      */
    def /(y: Number): Number = Number(x) doMultiply convertToNumber(y.invert)

    /**
      * Divide x by y (a Number) and yield a Number.
      * NOTE: the colon is necessary in order to coerce the left hand operand to be a Number.
      *
      * @param y the divisor, an Int.
      * @return a Number whose value is x / y.
      */
    def :/(y: Int): Number = /(Number(y))
  }

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * CONSIDER modulate the result so that, in the case of a multiple of Pi, we restrict the range to 0 to 2pi immediately.
    * However, note that this will change the behavior such that it is no longer possible to have the constant 2pi.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
    * @param fuzz   the fuzziness of this Number, wrapped in Option.
    * @return a Number.
    */
  def create(value: Value, factor: Factor, fuzz: Option[Fuzziness[Double]]): Number = (fuzz match {
    case None => ExactNumber(value, factor)
    case _ => FuzzyNumber(value, factor, fuzz)
  }).specialize

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
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
  def apply(x: String): Number = parse(x) match {
    // CONSIDER we should perhaps process n (e.g. to modulate a Pi value)
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

  private val numberParser = new NumberParser()

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
      * When we do a compare on E numbers, they are in the same order as Scalar numbers (i.e. monotonically increasing).
      * It's not necessary to convert exact numbers to fuzzy numbers for this purpose, we simply
      * pretend that the E numbers are Scalar numbers.
      *
      * @param x the first Number.
      * @param y the second Number.
      * @return an Int representing the order.
      */
    def compare(x: Number, y: Number): Int =
      if (x.factor == E && y.factor == E)
        compare(x.make(Scalar), y.make(Scalar))
      else
        GeneralNumber.plus(x, Number.negate(y)).signum
  }

  implicit object NumberIsOrdering extends NumberIsOrdering

  /**
    * Following are the definitions required by Numeric[Number]
    */
  trait NumberIsNumeric extends Numeric[Number] {
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
    * For fuzzy numbers, it's appropriate to use the the normal mechanism for compare, even for E numbers.
    *
    * NOTE, we first invoke same(p)(x, y) to determine if the Numbers are the same in a canonical manner.
    * However, we could actually skip this step and always just invoke the else part of the expression.
    *
    * @param x the first number.
    * @param y the second number.
    * @param p the probability criterion.
    * @return an Int representing the order.
    */
  def fuzzyCompare(x: Number, y: Number, p: Double): Int =
    if (implicitly[Fuzzy[Number]].same(p)(x, y)) 0
    else GeneralNumber.plus(x, Number.negate(y)).signum(p)

  /**
    * Following are the definitions required by Fractional[Number]
    */
  trait NumberIsFractional extends Fractional[Number] {
    def div(x: Number, y: Number): Number = GeneralNumber.times(x, Number.inverse(y))
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  // CONSIDER some of the following should probably be moved to GeneralNumber

  def prepare(no: Option[Number]): Number = no.getOrElse(Number())

  def prepareWithSpecialize(no: Option[Number]): Number = prepare(no).specialize

  private def sqrt(n: Number): Number = prepareWithSpecialize(n.scale(Scalar).transformMonadic(Scalar)(MonadicOperationSqrt))

  def power(x: Number, y: Number): Number =
    y.scale(Scalar).toRational match {
      case Some(r) => power(x, r)
      case None =>
        // NOTE this is not used, but it doesn't seem to handle fuzziness properly either.
        val zo = for (p <- x.toDouble; q <- y.toDouble) yield Number(math.pow(p, q))
        prepareWithSpecialize(zo)
    }

  @tailrec
  private def power(x: Number, r: Rational): Number =
    x.factor match {
      case E =>
        val vo: Option[Value] = Operations.doTransformValueMonadic(x.value)(MonadicOperationScale(r).functions)
        vo match {
          case Some(v) => x.make(v)
          case None => throw NumberException("power: logic error")
        }

      case Pi =>
        power(x.scale(Scalar), r)

      case Scalar =>
        toInts(r) match {
          case Some((n, d)) =>
            root(power(x, n), d) match {
              case Some(q) => q
              case None => Number(r.toDouble)
            }
          case _ =>
            throw NumberException("rational power cannot be represented as two Ints")
        }
    }

  private def power(n: Number, i: Int) = i match {
    case x if x > 0 => LazyList.continually(n).take(x).product
    case x => LazyList.continually(Number.inverse(n)).take(-x).product
  }

  /**
    * Method to take the ith root of n.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * CONSIDER a special factor which is basically a root.
    *
    * @param n the Number whose root is required.
    * @param i the ordinal of the root (2: square root, etc.).
    * @return the root.
    */
  private def root(n: Number, i: Int): Option[Number] = i match {
    case 0 => throw NumberException(s"root: logic error: cannot take ${i}th root")
    case 1 => Some(n)
    case 2 => Some(n.makeFuzzyIfAppropriate(Number.sqrt, 3))
    case _ => None
  }

  /**
    * Method to deal with a Scale factor change.
    *
    * CONSIDER: re-implementing the Pi/Scalar and Scalar/Pi cases using MonadicOperationScale.
    * TODO: this will work for FuzzyNumber but only if the fuzz is relative, and even then not for E conversions.
    *
    * @param n      the Number to be scaled.
    * @param factor the factor to which it should be converted.
    * @return the resulting Number (equivalent in value, but with a potentially different scale factor).
    */
  def scale(n: Number, factor: Factor): Number = (n.factor, factor) match {
    case (a, b) if a == b => n
    case (Pi, Scalar) | (Scalar, Pi) => prepare(n.maybeDouble.map(x => n.make(scaleDouble(x, n.factor, factor), factor)))
    case (E, Scalar) => prepare(n.transformMonadic(factor)(MonadicOperationExp))
    case (Scalar, E) => prepare(n.transformMonadic(factor)(MonadicOperationLog))
    case (Pi, E) => scale(scale(n, Scalar), E)
    case (E, Pi) => scale(scale(n, Scalar), E)
    case _ => throw NumberException("scaling between e and Pi factors is not supported")
  }

  def negate(x: Number): Number = prepare(x.transformMonadic(x.factor)(MonadicOperationNegate))

  def inverse(x: Number): Number = prepare(x.transformMonadic(x.factor)(MonadicOperationInvert))

  def isZero(x: Number): Boolean = x.query(QueryOperationIsZero, false)

  def isInfinite(x: Number): Boolean = x.query(QueryOperationIsInfinite, false)

  /**
    * TODO move this to GeneralNumber as an instance method.
    *
    * @param x a Number.
    * @return -1, 0, or 1 according to its sign.
    */
  def signum(x: Number): Int = x match {
    // CONSIDER this should be more like a query than a monadic function.
    case z: GeneralNumber =>
      z.doTransformMonadic(x.factor)(identityTry, tryF(x => x.signum), tryF(math.signum)).flatMap(_.toInt).getOrElse(0)
  }

  def sin(x: Number): Number = prepareWithSpecialize(x.scale(Pi).transformMonadic(Scalar)(MonadicOperationSin))

  def atan(x: Number, y: Number): Number = prepareWithSpecialize((y doDivide x).transformMonadic(Pi)(MonadicOperationAtan(x.signum))).modulate

  def log(x: Number): Number = x.scale(E).make(Scalar)

  def exp(x: Number): Number = x.scale(Scalar).make(E)

  private def scaleDouble(x: Double, fThis: Factor, fResult: Factor) = x * fThis.value / fResult.value

  /**
    * This method returns a Number equivalent to x but with the value in an explicit factor-dependent range.
    * Only Pi is currently fixed within a range (0 -> 2).
    *
    * @param x the Number to operate on.
    * @return either x or a number equivalent to x with value in defined range.
    */
  def modulate(x: Number): Number = x.factor match {
    case f@Pi => prepare(x.transformMonadic(f)(MonadicOperationModulate))
    case _ => x
  }

  def valueToString(v: Value): String = renderValue(v) match {
    case (x, true) => x
    case (x, false) => x + "..."
  }

  private def incrementUnicode(str: String, index: Int, x: Int): String = {
    val chars: Array[Char] = str.toArray
    chars.update(index, (chars(index) + x).toChar)
    new String(chars)
  }

  def asPowerOfE(v: Value): String = v match {
    case Right(1) => Factor.sE
    case Right(2) => Factor.sE + "\u00B2"
    case Right(3) => Factor.sE + "\u00B3"
    case Right(x) if x > 3 & x < 10 => Factor.sE + incrementUnicode("\u2070", 0, x)
    case Left(Right(r)) if r * 2 == Rational.one => "\u221A" + Factor.sE
    case Left(Right(r)) if r * 3 == Rational.one => "\u221B" + Factor.sE
    case Left(Right(r)) if r * 4 == Rational.one => "\u221C" + Factor.sE
    case _ => Factor.sE + "^" + valueToString(v)
  }
}

case class NumberException(str: String) extends Exception(str)

case class NumberExceptionWithCause(str: String, e: Throwable) extends Exception(str, e)
