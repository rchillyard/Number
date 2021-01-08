package com.phasmidsoftware.number.core

import java.util.NoSuchElementException

import com.phasmidsoftware.number.core.FP.{identityTry, optionMap, tryF, tryMap}
import com.phasmidsoftware.number.core.Number.{bigIntToInt, negate}
import com.phasmidsoftware.number.core.Rational.RationalHelper
import com.phasmidsoftware.number.core.Render.renderValue
import com.phasmidsoftware.number.core.Value._
import com.phasmidsoftware.number.parse.NumberParser

import scala.language.implicitConversions
import scala.math.BigInt
import scala.util._

/**
  * This class is designed to model an exact Numeral.
  * See Number for more details on the actual representation.
  *
  * TODO implement scientific notation by having factors such os 10^3, 10^6, etc. (alternatively, add a separate parameter)
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  */
case class ExactNumber(override val value: Value, override val factor: Factor) extends Number(value, factor) {

  /**
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new Number.
    */
  def this(v: Value) = this(v, Scalar)

  /**
    * Method to get this fuzziness of this Number.
    *
    * @return None
    */
  def fuzz: Option[Fuzz[Double]] = None

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return an ExactNumber.
    */
  def makeNumber(v: Value, f: Factor): Number = ExactNumber(v, f)

  /**
    * If the result of invoking f on this is a Double, then there will inevitably be some loss of precision.
    *
    * CONSIDER passing in the fuzziness value as it may differ for different functions.
    *
    * @return a Number which is the square toot of this, possibly fuzzy, Number.
    */
  def makeFuzzyIfAppropriate(f: Number => Number): Number = {
    val z: Number = f(this)
    z.value match {
      case v@Left(Left(Left(Some(_)))) => FuzzyNumber(v, z.factor, Some(RelativeFuzz(DoublePrecisionTolerance, Box)))
      case v => z.makeNumber(v)
    }
  }

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired (ignored).
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * Action to render this ExactNumber as a String.
    *
    * @return a String.
    */
  def render: String = toString

  /**
    * Action to materialize this Expression.
    *
    * @return this ExactNumber.
    */
  def materialize: Number = this
}

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * CONSIDER including the fuzziness in Number and simply having ExactNumber always have fuzz of None.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  */
abstract class Number(val value: Value, val factor: Factor) extends Expression with Ordered[Number] {

  self =>

  /**
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new Number.
    */
  def this(v: Value) = this(v, Scalar)

  /**
    * Numbers cannot (for now) be simplified.
    *
    * @return this.
    */
  def simplify: Expression = this

  /**
    * Method to determine if this is a valid Number.
    * An invalid number is of the has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  def isValid: Boolean = maybeDouble.isDefined

  /**
    * Method to get this fuzziness of this Number.
    *
    * @return for an Exact number, this will be None, otherwise the actual fuzz.
    */
  def fuzz: Option[Fuzz[Double]]

  /**
    * Method to get the value of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */

  def toDouble: Option[Double] = maybeDouble

  /**
    * Method to get the value of this Number as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.doubleToRational(x).
    *
    * @return an Option of Rational.
    */
  def toRational: Option[Rational] = maybeRational

  /**
    * Method to get the value of this Number as a BigInt.
    *
    * @return an Option of BigInt. If this Number cannot be converted to a BigInt, then None will be returned.
    */
  def toBigInt: Option[BigInt] = maybeBigInt

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int] = maybeInt

  /**
    * Add x to this Number and return the result.
    * See Number.plus for more detail.
    * CONSIDER inlining this method.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Number): Number = Number.plus(this, x)

  /**
    * Subtract x from this Number and return the result.
    * See + and unary_ for more detail.
    * CONSIDER inlining this method.
    *
    * @param x the subtrahend.
    * @return the difference.
    */
  def subtract(x: Number): Number = this add -x

  /**
    * Change the sign of this Number.
    */
  lazy val unary_- : Number = Number.negate(this)

  /**
    * Multiply this Number by x and return the result.
    * See Number.times for more detail.
    * CONSIDER inlining this method.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Number): Number = Number.times(this, x)

  /**
    * Eagerly multiply this Number by an Int.
    * CONSIDER inlining this method.
    *
    * @param x the scale factor (an Int).
    * @return this * x.
    */
  def multiply(x: Int): Number = Number.scale(this, x)

  /**
    * Divide this Number by x and return the result.
    * See * and invert for more detail.
    * CONSIDER inlining this method.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Number): Number = this multiply x.invert

  /**
    * Divide this Number by a scalar x and return the result.
    * CONSIDER inlining this method.
    *
    * @param x the divisor (an Int).
    * @return the quotient.
    */
  def divide(x: Int): Number = this divide Number(x)

  /**
    * Raise this Number to the power p.
    * CONSIDER inlining this method.
    *
    * @param p a Number.
    * @return this Number raised to power p.
    */
  def power(p: Number): Number = Number.power(this, p)

  /**
    * Raise this Number to the power p.
    * CONSIDER inlining this method.
    *
    * @param p an integer.
    * @return this Number raised to power p.
    */
  def power(p: Int): Number = Number.power(this, p)

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
  def sqrt: Number = Number.power(this, Number(r"1/2"))

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the sine of this.
    */
  def sin: Number = makeFuzzyIfAppropriate(Number.sin)

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the cosine.
    */
  def cos: Number = negate(scale(Pi) subtract Number(Rational.half, Pi)).sin

  /**
    * The tangent of this Number.
    *
    * @return the tangent.
    */
  def tan: Number = sin divide cos

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number = makeFuzzyIfAppropriate(x => Number.atan(x, y))

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int = Number.signum(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired.
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int

  /**
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    * In other words,  in the case where f is not factor, the numerical value of the result's value will be different
    * from this value.
    *
    * @param f the new factor for the result.
    * @return a Number based on this and factor.
    */
  def scale(f: Factor): Number = makeFuzzyIfAppropriate(x => Number.scale(x, f))

  /**
    * @return true if this Number is equal to zero.
    */
  def isZero: Boolean = Number.isZero(this)

  /**
    * @return true if this Number is equal to zero.
    */
  def isInfinite: Boolean = Number.isInfinite(this)

  /**
    * Method to compare this with another Number.
    * The difference between this method and that of ExactNumber is that the signum method is implemented differently.
    *
    * @param other the other Number.
    * @return -1, 0, or 1 according to whether x is <, =, or > y.
    */
  override def compare(other: Number): Int = Number.compare(this, other)

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * @param other the Number to be compared with.
    * @param p     the confidence expressed as a fraction of 1 (0.5 would be a typical value).
    * @return -1, 0, 1 as usual.
    */
  def fuzzyCompare(other: Number, p: Double): Int = Number.fuzzyCompare(this, other, p)

  /**
    * Be careful when implementing this method that you do not invoke a method recursively.
    *
    * @return a Number which is the the result, possibly fuzzy, of invoking f on this.
    */
  def makeFuzzyIfAppropriate(f: Number => Number): Number

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadic(other: Number, f: Factor)(op: DyadicOperation): Option[Number] = doComposeDyadic(other, f)(op.getFunctions)

  /**
    * Evaluate a monadic operator on this, using either negate or... according to the value of op.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def composeMonadic(f: Factor)(op: MonadicOperation): Option[Number] = doComposeMonadic(f)(op.getFunctions)

  /**
    * Render this Number in String form, including the factor.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    sb.append(valueToString)
    sb.append(factor.toString)
    sb.toString
  }

  protected def valueToString: String = renderValue(value) match {
    case (x, true) => x
    case (x, false) => x + "..."
  }

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Value, f: Factor): Number

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the factor will change.
    * This method does not need to be followed by a call to specialize.
    *
    * @param f the factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(f: Factor): Number = makeNumber(value, f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  def makeNumber(v: Value): Number = makeNumber(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Int, f: Factor): Number = makeNumber(Value.fromInt(v), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Int): Number = makeNumber(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: BigInt, f: Factor): Number = makeNumber(Value.fromBigInt(v), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: BigInt): Number = makeNumber(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param r a Rational.
    * @param f Factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(r: Rational, f: Factor): Number = makeNumber(Value.fromRational(r), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Rational): Number = makeNumber(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value (a Double).
    * @param f Factor.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Double, f: Factor): Number = makeFuzzyIfAppropriate(x => x.makeNumber(Value.fromDouble(Some(v)), f))

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value (a Double).
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Double): Number = makeNumber(v, factor)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  def normalize: Number = factor match {
    case Scalar => this
    case Pi | E => (maybeDouble map (x => self.makeNumber(x * factor.value).specialize.makeNumber(Scalar))).getOrElse(Number())
  }

  /**
    * Return a Number which uses the most restricted type possible.
    * A Number based on a Double will yield a Number based on a Rational (if the conversion is exact).
    * A Number based on a Rational will yield a Number based on a BigInt (if there is a unit denominator).
    * A Number based on a BigInt will yield a Number based on a Int (if it is sufficiently small).
    *
    * @return a Number with the same magnitude as this.
    */
  //protected
  lazy val specialize: Number = value match {
    // Int case
    case Right(_) => this
    // BigInt case
    case Left(Right(b)) =>
      val intValue = b.intValue
      if (BigInt(intValue) == b) makeNumber(intValue) else this
    // Rational case
    case Left(Left(Right(r))) =>
      Try(r.toBigInt) match {
        case Success(b) => makeNumber(b).specialize
        case _ => this
      }
    // Double case
    case d@Left(Left(Left(Some(x)))) =>
      // NOTE: here we attempt to deal with Doubles.
      // If a double can be represented by a BigDecimal with scale 0, 1, or 2 then we treat it as exact.
      // Otherwise, we will give it appropriate fuzziness.
      // In general, if you wish to have more control over this, then define your input using a String.
      // CONSIDER will this handle numbers correctly which are not close to 1?
      val r = Rational(x)
      r.toBigDecimal.scale match {
        case 0 | 1 | 2 => makeNumber(r).specialize
          // CONSIDER in following line adding fuzz only if this Number is exact.
        case n => FuzzyNumber(d, factor, fuzz).addFuzz(AbsoluteFuzz(Fuzz.toDecimalPower(5, -n), Box))
      }
    // Invalid case
    case _ => this
  }

  /**
    * Method to align the factors of this and x such that the resulting Numbers (in the tuple) each have the same factor.
    *
    * @param x the Number to be aligned with this.
    * @return a tuple of two Numbers with the same factor.
    */
  //protected
  def alignFactors(x: Number): (Number, Number) = factor match {
    case Scalar => (this, x.scale(factor))
    case _ => (scale(x.factor), x)
  }

  /**
    * Method to align the types of this and x such that the resulting Numbers (in the tuple) each have the same structure.
    *
    * @param x the Number to be aligned with this.
    * @return a tuple of two Numbers, the first of which will be the more general type:
    *         (Invalid vs. Double, Double vs. Rational, Rational vs. BigInt, BigInt vs. Int).
    */
  //protected
  def alignTypes(x: Number): (Number, Number) = value match {
    // this is an invalid Number: return a pair of invalid numbers
    case Left(Left(Left(None))) => (this, this)
    // this value is a real Number: convert x to a Number based on real.
    case Left(Left(Left(Some(_)))) => x.value match {
      // x's value is invalid: swap the order so the the first element is invalid
      case Left(Left(Left(None))) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a Double
      case _ => (this, x.maybeDouble.map(y => makeNumber(y, x.factor).specialize).getOrElse(Number()))
    }
    // this value is a Rational:
    case Left(Left(Right(_))) => x.value match {
      // x's value is a real Number: swap the order so that the first element is the real number
      case Left(Left(Left(_))) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a Rational
      case _ => (this, x.makeNumber(x.maybeRational.getOrElse(Rational.NaN), x.factor).specialize)
    }
    // this value is a BigInt:
    case Left(Right(_)) => x.value match {
      // x's value is a Rational or real Number: swap the order so that the first element is the Rational/real number
      case Left(Left(_)) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a BigInt
      case _ => (this, x.makeNumber(x.maybeBigInt.getOrElse(BigInt(0)), x.factor).specialize)
    }
    // this value is an Int:
    case Right(_) => x.value match {
      // x's value is a BigInt, Rational or real Number: swap the order so that the first element is the BigInt/Rational/real number
      case Left(_) => x.alignTypes(this)
      // otherwise: return this and x re-cast as an Int
      case _ => (this, x.makeNumber(x.maybeInt.getOrElse(0), x.factor).specialize)
    }
  }

  /**
    * Method to ensure that the value is within some factor-specific range.
    * In particular, Pi=based numbers are modulated to the range 0..2
    *
    * @return this or an equivalent Number.
    */
  def modulate: Number = Number.modulate(this)

  /**
    * Evaluate a dyadic operator on this and other, using the various functions passed in.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other     the other operand, a Number.
    * @param f         the factor to apply to the result.
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  private def doComposeDyadic(other: Number, f: Factor)(functions: DyadicFunctions): Option[Number] = {
    val (fInt, fBigInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Number] = {
      case Some(n) => fDouble(n, other.maybeDouble.get) map (x => makeNumber(x, f))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => tryMap(y)(x => fRational(x, other.maybeRational.get) map (makeNumber(_, f)), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => tryMap(x)(x => fBigInt(x, other.toBigInt.get) map (makeNumber(_, f)), xToZy1)
    tryMap(value)(x => fInt(x, other.maybeInt.get) map (makeNumber(_, f)), xToZy2).toOption
  }

  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    *
    * @param f         the factor to be used for the result.
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  private def doComposeMonadic(f: Factor)(functions: MonadicFunctions): Option[Number] = {
    val (fInt, fBigInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Number] = {
      case Some(n) => fDouble(n) map (makeNumber(_, f))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => tryMap(y)(x => fRational(x) map (makeNumber(_, f)), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => tryMap(x)(x => fBigInt(x) map (makeNumber(_, f)), xToZy1)
    tryMap(value)(x => fInt(x) map (y => makeNumber(y, f)), xToZy2).toOption
  }

  /**
    * Evaluate a query operator on this, using the various functions passed in.
    *
    * @param f         the factor to be used for the result.
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  private def doQuery(f: Factor)(functions: QueryFunctions): Option[Boolean] = {
    val (fInt, fBigInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Boolean] = {
      case Some(n) => fDouble(n)
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Boolean] = y => tryMap(y)(x => fRational(x), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Boolean] = x => tryMap(x)(x => fBigInt(x), xToZy1)
    tryMap(value)(x => fInt(x), xToZy2).toOption
  }

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using MonadicTransformations
    */
  private lazy val maybeRational: Option[Rational] = {
    import Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x =>
      tryMap(x)(tryF(Rational.apply), x =>
        tryMap(x)(identityTry, _ =>
          Failure(new NoSuchElementException()))))
    ry.toOption
  }

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  private lazy val maybeDouble: Option[Double] = optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, x => optionMap(x)(_.toDouble, identity)))

  /**
    * An optional BigInt that corresponds to the value of this Number (but ignoring the factor).
    *
    * CONSIDER using MonadicTransformations
    */
  private lazy val maybeBigInt: Option[BigInt] = {
    val xToZy0: Option[Double] => Try[BigInt] = {
      case Some(n) if Math.round(n) == n => Try(BigInt(n.toInt))
      case Some(n) => Failure(NumberException(s"toBigInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[BigInt] = y => tryMap(y)(tryF(y => y.toBigInt), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[BigInt] = x => tryMap(x)(identityTry, xToZy1)
    tryMap(value)(tryF(BigInt.apply), xToZy2).toOption
  }

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    *
    * CONSIDER using MonadicTransformations
    */
  private lazy val maybeInt: Option[Int] = {
    val xToZy0: Option[Double] => Try[Int] = {
      case Some(n) if Math.round(n) == n => if (n <= Int.MaxValue && n >= Int.MinValue) Try(n.toInt)
      else Failure(NumberException(s"double $n cannot be represented as an Int"))
      case Some(n) => Failure(NumberException(s"toInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Int] = y => tryMap(y)(tryF(y => y.toInt), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Int] = x => tryMap(x)(bigIntToInt, xToZy1)
    tryMap(value)(identityTry, xToZy2).toOption
  }
}

object Number {
  /**
    * Exact value of 1
    */
  val zero: Number = ExactNumber(Right(0), Scalar)
  /**
    * Exact value of 1
    */
  val one: Number = ExactNumber(Right(1), Scalar)
  /**
    * Exact value of pi
    */
  val pi: Number = ExactNumber(Right(1), Pi)
  /**
    * Exact value of e
    */
  val e: Number = ExactNumber(Right(1), E)

  /**
    * Method to construct a new Number from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value  the value of the Number, expressed as a nested Either type.
    * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
    * @param fuzz   the fuzziness of this Number, wrapped in Option.
    * @return a Number.
    */
  def create(value: Value, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = (fuzz match {
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
  def create(value: Value, actualFuzz: Fuzz[Double]): Number = create(value, Scalar, Some(actualFuzz))

  /**
    * Method to construct a new Number from 1, factor and fuzz, according to whether there is any fuzziness.
    *
    * NOTE: not currently used.
    *
    * @param actualFuzz the fuzziness of this Number.
    * @return a Number.
    */
  def create(factor: Factor, actualFuzz: Fuzz[Double]): Number = create(Value.fromInt(1), factor, Some(actualFuzz))

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
  def apply(x: Int, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = create(fromInt(x), factor, fuzz)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigInt, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = create(fromBigInt(x), factor, fuzz)

  /**
    * Method to construct a Number from a Long with an explicit factor.
    *
    * NOTE: not currently used.
    *
    * @param x      the Long value.
    * @param factor the appropriate factor
    * @return a Number based on the value of x converted to BigInt.
    */
  def apply(x: Long, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = Number(BigInt(x), factor, fuzz)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Rational, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = create(fromRational(x), factor, fuzz)

  /**
    * Method to construct a Number from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: BigDecimal, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = Number(Rational(x), factor, fuzz)

  /**
    * Method to construct a Number from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a Number based on xo.
    */
  def apply(xo: Option[Double], factor: Factor, fuzz: Option[Fuzz[Double]]): Number = create(fromDouble(xo), factor, fuzz)

  /**
    * Method to construct a Number from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a Number based on x.
    */
  def apply(x: Double, factor: Factor, fuzz: Option[Fuzz[Double]]): Number = x match {
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
    * Method to construct a Number from a Long with an explicit factor.
    *
    * NOTE: not currently used.
    *
    * @param x      the Long value.
    * @param factor the appropriate factor
    * @return a Number based on the value of x converted to BigInt.
    */
  def apply(x: Long, factor: Factor): Number = Number(x, factor, None)

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
    * NOTE: not currently used.
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
    * Method to construct a Number from a Long.
    *
    * @param x the Long value.
    * @return a Number based on the value of x converted to BigInt.
    */
  def apply(x: Long): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from a BigInt.
    *
    * @param x the BigInt value.
    * @return a Number based on x.
    */
  def apply(x: BigInt): Number = Number(x, Scalar)

  /**
    * Method to construct a Number from a Rational.
    *
    * @param x the BigInt value.
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
    def compare(x: Number, y: Number): Int = plus(x, negate(y)).signum
  }

  implicit object NumberIsOrdering extends NumberIsOrdering

  /**
    * Following are the definitions required by Numeric[Number]
    */
  trait NumberIsNumeric extends Numeric[Number] {
    def plus(x: Number, y: Number): Number = Number.plus(x, y)

    def minus(x: Number, y: Number): Number = Number.plus(x, negate(y))

    def times(x: Number, y: Number): Number = Number.times(x, y)

    def negate(x: Number): Number = Number.negate(x)

    def fromInt(x: Int): Number = Number(x)

    def parseString(str: String): Option[Number] = parse(str).toOption

    def toInt(x: Number): Int = toLong(x).toInt

    def toLong(x: Number): Long = x.toBigInt match {
      case Some(y) => Rational(y).toLong
      case None => x.maybeRational match {
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

  def compare(x: Number, y: Number): Int = NumberIsOrdering.compare(x, y)

  def fuzzyCompare(x: Number, y: Number, p: Double): Int = Number.plus(x, Number.negate(y)).signum(p)

  /**
    * Following are the definitions required by Fractional[Number]
    */
  trait NumberIsFractional extends Fractional[Number] {
    def div(x: Number, y: Number): Number = Number.times(x, inverse(y))
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  private def plus(x: Number, y: Number): Number = y match {
    case n@FuzzyNumber(_, _, _) => n add x
    case _ =>
      val (a, b) = x.alignFactors(y)
      val (p, q) = a.alignTypes(b)
      p.composeDyadic(q, p.factor)(DyadicOperationPlus).getOrElse(Number()).specialize
  }

  private def times(x: Number, y: Number): Number =
    y match {
      case n@FuzzyNumber(_, _, _) => n multiply x
      case _ =>
        val (p, q) = x.alignTypes(y)
        val factor = p.factor + q.factor
        p.composeDyadic(q, factor)(DyadicOperationTimes).getOrElse(Number()).specialize
    }

  // TODO wrap this (and all the others)
  private def sqrt(n: Number): Number = n.scale(Scalar).composeMonadic(Scalar)(MonadicOperationSqrt).getOrElse(Number()).specialize

  // CONSIDER dealing with non-Scalar x values up-front
  private def power(x: Number, y: Number): Number = y.normalize.toInt match {
    case Some(i) => power(x, i)
    case _ => y.toRational match {
      // TODO this may lose precision
      case Some(Rational(n, d)) =>
        root(power(x, n.toInt), d.toInt) match {
          case Some(q) => q
          case None => y.toDouble map x.makeNumber getOrElse Number()
        }
      case None => y.toDouble match {
        case Some(d) => power(x, d)
        case _ => throw NumberException("invalid power")
      }
    }
  }

  private def power(n: Number, i: Int) = i match {
    case x if x > 0 => LazyList.continually(n).take(x).product
    case x => LazyList.continually(inverse(n)).take(-x).product
  }

  private def root(n: Number, i: Int): Option[Number] = i match {
    case 2 => Some(n.makeFuzzyIfAppropriate(Number.sqrt))
    case _ => None
  }

  private def power(n: Number, y: Double): Number = n.toDouble.map(x => math.pow(x, y)).map(n.makeNumber).getOrElse(Number())

  def scale(n: Number, f: Factor): Number = n.factor match {
    case `f` => n
    case _ => n.maybeDouble.map(x => n.makeNumber(scaleDouble(x, n.factor, f), f)).getOrElse(Number())
  }

  private def scaleDouble(x: Double, fThis: Factor, fResult: Factor) = x * fThis.value / fResult.value

  private def scale(x: Number, f: Int): Number = x.composeMonadic(x.factor)(MonadicOperationScale(f)).getOrElse(Number())

  def negate(x: Number): Number = x.composeMonadic(x.factor)(MonadicOperationNegate).getOrElse(Number())

  def inverse(x: Number): Number = {
    // CONSIDER use composeMonadic
    val maybeNumber = x.value match {
      // First we take care of the special cases
      case Right(1) => Some(x)
      case Left(Right(_)) if x.toBigInt.get == BigInt(1) => Some(x)
      case Left(Left(Left(_))) => x.maybeDouble.map(1 / _).map(x.makeNumber)
      case _ => x.maybeRational.map(_.invert).map(x.makeNumber)
    }
    maybeNumber.getOrElse(Number()).specialize
  }

  private def isZero(x: Number): Boolean = {
    // CONSIDER use composeMonadic
    val intToTriedBoolean = tryF[Int, Boolean](x => x == 0)
    val bigIntToTriedBoolean = tryF[BigInt, Boolean](x => x.sign == 0)
    val rationalToTriedBoolean = tryF[Rational, Boolean](x => x.signum == 0)
    val doubleToTriedBoolean = tryF[Double, Boolean](x => x.sign == 0 || x.sign == -0)
    x.doQuery(x.factor)(intToTriedBoolean, bigIntToTriedBoolean, rationalToTriedBoolean, doubleToTriedBoolean).get
  }

  private def isInfinite(x: Number): Boolean = {
    // CONSIDER use composeMonadic
    val intToTriedBoolean = tryF[Int, Boolean](_ => false)
    val bigIntToTriedBoolean = tryF[BigInt, Boolean](_ => false)
    val rationalToTriedBoolean = tryF[Rational, Boolean](x => x.isInfinity)
    val doubleToTriedBoolean = tryF[Double, Boolean](x => x == Double.PositiveInfinity || x == Double.NegativeInfinity)
    x.doQuery(x.factor)(intToTriedBoolean, bigIntToTriedBoolean, rationalToTriedBoolean, doubleToTriedBoolean).get
  }

  private def signum(x: Number): Int = x.doComposeMonadic(x.factor)(identityTry, tryF(x => x.signum), tryF(x => x.signum), tryF(math.signum)).flatMap(_.toInt).getOrElse(0)

  private def sin(x: Number): Number = x.scale(Pi).composeMonadic(Scalar)(MonadicOperationSin).getOrElse(Number()).specialize

  private def atan(x: Number, y: Number): Number = (y divide x).composeMonadic(Pi)(MonadicOperationAtan(x.signum)).getOrElse(Number()).specialize.modulate

  /**
    * This method returns a Number equivalent to x but with the value in an explicit factor-dependent range.
    * Only Pi is currently fixed within a range (0 -> 2).
    *
    * @param x the Number to operate on.
    * @return either x or a number equivalent to x with value in defined range.
    */
  private def modulate(x: Number): Number = x.factor match {
    case f@Pi => x.composeMonadic(f)(MonadicOperationModulate).getOrElse(Number())
    case _ => x
  }

  private def bigIntToInt(x: BigInt): Try[Int] = Rational.toInt(x)
}

case class NumberException(str: String) extends Exception(str)

case class NumberExceptionWithCause(str: String, e: Throwable) extends Exception(str, e)

