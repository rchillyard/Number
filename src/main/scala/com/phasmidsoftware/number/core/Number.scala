package com.phasmidsoftware.number.core

import java.util.NoSuchElementException

import com.phasmidsoftware.number.core.Number.{DyadicFunctions, MonadicFunctions}
import com.phasmidsoftware.number.core.Value._
import com.phasmidsoftware.number.parse.NumberParser

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
}

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  */
abstract class Number(val value: Value, val factor: Factor) {

  self =>

  /**
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new Number.
    */
  def this(v: Value) = this(v, Scalar)

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
    * Method to get the value of this Number as a Double.
    *
    * @return an Option of Double.
    */

  def toDouble: Option[Double] = normalize.maybeDouble

  /**
    * Method to get the value of this Number as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.doubleToRational(x).
    *
    * @return an Option of Rational.
    */
  def toRational: Option[Rational] = normalize.maybeRational

  /**
    * Method to get the value of this Number as a BigInt.
    *
    * @return an Option of BigInt. If this Number cannot be converted to a BigInt, then None will be returned.
    */
  def toBigInt: Option[BigInt] = normalize.maybeBigInt

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int] = normalize.maybeInt

  /**
    * Add x to this Number and return the result.
    * See Number.plus for more detail.
    *
    * @param x the addend.
    * @return the sum.
    */
  def +(x: Number): Number = Number.plus(this, x)

  /**
    * Subtract x from this Number and return the result.
    * See + and unary_ for more detail.
    *
    * @param x the subtrahend.
    * @return the difference.
    */
  def -(x: Number): Number = this + -x

  /**
    * Change the sign of this Number.
    */
  lazy val unary_- : Number = Number.negate(this)

  /**
    * Multiply this Number by x and return the result.
    * See Number.times for more detail.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def *(x: Number): Number = Number.times(this, x)

  /**
    * Divide this Number by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def /(x: Number): Number = this * x.invert

  /**
    * Yields the inverse of this Number.
    * This Number is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  lazy val invert: Number = Number.inverse(normalize)

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    */
  lazy val sin: Number = Number.sin(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  lazy val signum: Int = Number.signum(this)

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

  protected def valueToString: String =
    Number.optionMap(value)(_.toString, x => Number.optionMap(x)(_.toString, y => Number.optionMap(y)(_.toString, {
      case Some(n) => Some(n.toString)
      case None => None
    }))).getOrElse("<undefined>")

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
  protected def makeNumber(v: Value): Number = makeNumber(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Int): Number = makeNumber(Value.fromInt(v), factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: BigInt): Number = makeNumber(Value.fromBigInt(v), factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Rational): Number = makeNumber(Value.fromRational(v), factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Fuzzy or Exact Number.
    */
  protected def makeNumber(v: Double): Number = makeNumber(Value.fromDouble(Some(v)), factor)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  //protected
  def normalize: Number = factor match {
    case Scalar => this
    case Pi | E => (maybeDouble map (x => self.makeNumber(x * factor.value).specialize.makeNumber(Scalar))).getOrElse(Number())
  }

  /**
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    *
    * @param f the new factor for the result.
    * @return a Number based on this and factor.
    */
  //protected
  def scale(f: Factor): Number = factor match {
    case `f` => this
    case _ => Number(maybeDouble map (_ * factor.value / f.value), f)
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
    case Left(Left(Left(Some(x)))) =>
      // NOTE: if we can represent x as a Rational whose value is identical with x, then we use it.
      // Keep in mind that, when we specify a Double such as 3.1415927, its binary representation
      // may not be exactly what we expect. Such a number is best defined through parsing a String.
      val r = Rational(x)
      if (r.toDouble == x) makeNumber(r).specialize else this
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
      case _ => (this, x.maybeDouble.map(y => Number(y, x.factor)).getOrElse(Number()))
    }
    // this value is a Rational:
    case Left(Left(Right(_))) => x.value match {
      // x's value is a real Number: swap the order so that the first element is the real number
      case Left(Left(Left(_))) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a Rational
      case _ => (this, Number(x.maybeRational.getOrElse(Rational.NaN), x.factor))
    }
    // this value is a BigInt:
    case Left(Right(_)) => x.value match {
      // x's value is a Rational or real Number: swap the order so that the first element is the Rational/real number
      case Left(Left(_)) => x.alignTypes(this)
      // otherwise: return this and x re-cast as a BigInt
      case _ => (this, Number(x.maybeBigInt.getOrElse(BigInt(0)), x.factor)) // FIXME Need to fix this
    }
    // this value is an Int:
    case Right(_) => x.value match {
      // x's value is a BigInt, Rational or real Number: swap the order so that the first element is the BigInt/Rational/real number
      case Left(_) => x.alignTypes(this)
      // otherwise: return this and x re-cast as an Int
      case _ => (this, Number(x.maybeInt.getOrElse(0), x.factor)) // FIXME Need to fix this
    }
  }

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
      case Some(n) => Try(Number(fDouble(n, other.maybeDouble.get), f))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => Number.tryMap(y)(x => Number(fRational(x, other.maybeRational.get), f), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => Number.tryMap(x)(x => Number(fBigInt(x, other.toBigInt.get), f), xToZy1)
    Number.tryMap(value)(x => Number(fInt(x, other.maybeInt.get), f), xToZy2).toOption
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
      case Some(n) => Try(Number(fDouble(n), f))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => Number.tryMap(y)(x => Number(fRational(x), f), xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Number] = x => Number.tryMap(x)(x => Number(fBigInt(x), f), xToZy1)
    Number.tryMap(value)(x => Number(fInt(x), f), xToZy2).toOption
  }

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    */
  private lazy val maybeRational: Option[Rational] = {
    val ry = Number.tryMap(value)(Rational(_), x =>
      Number.tryMap(x)(Rational(_), y =>
        Number.tryMap(y)(identity, _ =>
          Failure(new NoSuchElementException()))))
    ry.toOption
  }

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  private lazy val maybeDouble: Option[Double] = Number.optionMap(value)(_.toDouble, x => Number.optionMap(x)(_.toDouble, y => Number.optionMap(y)(_.toDouble, identity)))

  /**
    * An optional BigInt that corresponds to the value of this Number (but ignoring the factor).
    */
  private lazy val maybeBigInt: Option[BigInt] = {
    val xToZy0: Option[Double] => Try[BigInt] = {
      case Some(n) if Math.round(n) == n => Try(BigInt(n.toInt))
      case Some(n) => Failure(NumberException(s"toBigInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[BigInt] = y => Number.tryMap(y)(x => x.toBigInt, xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[BigInt] = x => Number.tryMap(x)(identity, xToZy1)
    Number.tryMap(value)(BigInt(_), xToZy2).toOption
  }

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    */
  private lazy val maybeInt: Option[Int] = {
    val xToZy0: Option[Double] => Try[Int] = {
      case Some(n) if Math.round(n) == n => Try(n.toInt)
      case Some(n) => Failure(NumberException(s"toInt: $n is not integral"))
      case None => Failure(new NoSuchElementException())
    }
    val xToZy1: Either[Option[Double], Rational] => Try[Int] = y => Number.tryMap(y)(x => x.toInt, xToZy0)
    val xToZy2: Either[Either[Option[Double], Rational], BigInt] => Try[Int] = x => Number.tryFlatMap(x)(Number.bigIntToInt, xToZy1)
    Number.tryMap(value)(identity, xToZy2).toOption
  }
}

object Number {

  type DyadicFunctions = ((Int, Int) => Int, (BigInt, BigInt) => BigInt, (Rational, Rational) => Rational, (Double, Double) => Double)

  type MonadicFunctions = (Int => Int, BigInt => BigInt, Rational => Rational, Double => Double)

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
  def apply(x: String): Number = Number.parse(x) match {
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
    * Method to construct a Number from a Long.
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
    * Method to construct a Number from a Long.
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
    * Method to construct a unit Number.
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

  val numberParser = new NumberParser()

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

  /**
    * Following are the definitions required by Numeric[Number]
    */
  trait NumberIsNumeric extends Numeric[Number] {
    def plus(x: Number, y: Number): Number = Number.plus(x, y)

    def minus(x: Number, y: Number): Number = Number.plus(x, negate(y))

    def times(x: Number, y: Number): Number = Number.times(x, y)

    def negate(x: Number): Number = Number.negate(x)

    def fromInt(x: Int): Number = Number(x)

    def parseString(str: String): Option[Number] = Number.parse(str).toOption

    def toInt(x: Number): Int = toLong(x).toInt

    def toLong(x: Number): Long = x.toBigInt match {
      case Some(y) => y.toLong
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

  /**
    * Following are the definitions required by Fractional[Number]
    */
  trait NumberIsFractional extends Fractional[Number] {
    def div(x: Number, y: Number): Number = Number.times(x, inverse(y))
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  //noinspection ScalaUnusedSymbol
  private def optionToEither[X, Y](x: Option[X], y: => Y): Either[Y, X] = x.map(Right(_)).getOrElse(Left(y))

  private def optionMap[X, Y, Z](xYe: Either[X, Y])(yToZ: Y => Z, xToZy: X => Option[Z]): Option[Z] = xYe.toOption.map(yToZ) match {
    case Some(z) => Some(z)
    case None => xYe.left.toOption.flatMap(xToZy)
  }

  private def tryMap[X, Y, Z](xYe: Either[X, Y])(yToZ: Y => Z, xToZy: X => Try[Z]): Try[Z] =
    xYe.toOption.map(yToZ) match {
      case Some(z) => Success(z)
      case None => tryAltFunction(xYe, xToZy)
    }

  private def tryFlatMap[X, Y, Z](xYe: Either[X, Y])(yToZy: Y => Try[Z], xToZy: X => Try[Z]): Try[Z] =
    xYe.toOption.map(yToZy) match {
      case Some(z) => z
      case None => tryAltFunction(xYe, xToZy)
    }

  private def tryAltFunction[Z, Y, X](xYe: Either[X, Y], xToZy: X => Try[Z]): Try[Z] =
    xYe.left.toOption.map(xToZy) match {
      case Some(z) => z
      case None => Failure(new NoSuchElementException)
    }

  private def plus(x: Number, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    val (p, q) = a.alignTypes(b)
    p.composeDyadic(q, p.factor)(DyadicOperationPlus).getOrElse(Number()).specialize
  }

  private def times(x: Number, y: Number): Number = {
    val (p, q) = x.alignTypes(y)
    val factor = p.factor + q.factor
    p.composeDyadic(q, factor)(DyadicOperationTimes).getOrElse(Number()).specialize
  }

  private def negate(x: Number): Number = x.composeMonadic(x.factor)(MonadicOperationNegate).getOrElse(Number())

  private def inverse(x: Number): Number = {
    val maybeNumber = x.value match {
      // First we take care of the special cases
      case Right(1) => Some(x)
      case Left(Right(_)) if x.toBigInt.get == BigInt(1) => Some(x)
      case Left(Left(Left(_))) => x.maybeDouble.map(1 / _).map(Number(_))
      case _ => x.maybeRational.map(_.invert).map(Number(_))
    }
    maybeNumber.getOrElse(Number()).specialize
  }

  // NOTE: This may throw an exception
  private def signum(x: Number): Int = x.doComposeMonadic(x.factor)(identity, _.signum, _.signum, Math.signum).flatMap(_.toInt).get

  private def sin(n: Number): Number = n.scale(Pi).doComposeMonadic(Scalar)(_ => 0, _ => 0, sinRat, sinDouble).getOrElse(Number()).specialize

  private val sinRat: Rational => Rational = x =>
    if (!x.invert.isWhole) sinDouble(x.toDouble)
    else
      x.invert.toInt match {
        case 6 => Rational(1, 2)
        case 4 => Rational(Math.sqrt(2) / 2)
        case 2 => Rational(1)
        case _ => sinDouble(x.toDouble)
      }

  def sinDouble(x: Double): Double = Math.sin(x)

  private def bigIntToInt(x: BigInt): Try[Int] = {
    val range = (BigInt(Int.MinValue), BigInt(Int.MaxValue))
    if (range._1 <= x && x <= range._2) Success(x.toInt) else Failure(NumberException(s"bigIntToInt: $x is too large"))
  }
}

/**
  * This object provides methods related to the type Value.
  */
object Value {

  /**
    * Convert an Int to a Value.
    *
    * @param x an Int.
    * @return a Value.
    */
  def fromInt(x: Int): Value = Right(x)

  /**
    * Convert a BigInt to a Value.
    *
    * @param x a BigInt.
    * @return a Value.
    */
  def fromBigInt(x: BigInt): Value = Left(Right(x))

  /**
    * Convert a Rational to a Value.
    *
    * @param x a Rational.
    * @return a Value.
    */
  def fromRational(x: Rational): Value = Left(Left(Right(x)))

  /**
    * Convert an Option[Double] to a Value.
    *
    * @param xo a Double.
    * @return a Value.
    */
  def fromDouble(xo: Option[Double]): Value = Left(Left(Left(xo)))

  /**
    * Convert nothing to an invalid Value.
    *
    * @return a Value.
    */
  def fromNothing(): Value = Left(Left(Left(None)))
}

sealed trait MonadicOperation {
  def getFunctions: MonadicFunctions
}

case object MonadicOperationNegate extends MonadicOperation {
  def getFunctions: MonadicFunctions = {
    val fInt: Int => Int = implicitly[Numeric[Int]].negate
    val fBigInt: BigInt => BigInt = implicitly[Numeric[BigInt]].negate
    val fRational: Rational => Rational = implicitly[Numeric[Rational]].negate
    val fDouble: Double => Double = implicitly[Numeric[Double]].negate
    (fInt, fBigInt, fRational, fDouble)
  }
}

sealed trait DyadicOperation {
  def getFunctions: DyadicFunctions
}

case object DyadicOperationPlus extends DyadicOperation {
  def getFunctions: DyadicFunctions = {
    val fInt: (Int, Int) => Int = implicitly[Numeric[Int]].plus
    val fBigInt: (BigInt, BigInt) => BigInt = implicitly[Numeric[BigInt]].plus
    val fRational: (Rational, Rational) => Rational = implicitly[Numeric[Rational]].plus
    val fDouble: (Double, Double) => Double = implicitly[Numeric[Double]].plus
    (fInt, fBigInt, fRational, fDouble)
  }
}

case object DyadicOperationTimes extends DyadicOperation {
  def getFunctions: DyadicFunctions = {
    val fInt: (Int, Int) => Int = implicitly[Numeric[Int]].times
    val fBigInt: (BigInt, BigInt) => BigInt = implicitly[Numeric[BigInt]].times
    val fRational: (Rational, Rational) => Rational = implicitly[Numeric[Rational]].times
    val fDouble: (Double, Double) => Double = implicitly[Numeric[Double]].times
    (fInt, fBigInt, fRational, fDouble)
  }
}

sealed trait Factor {
  def value: Double

  def +(other: Factor): Factor
}

sealed abstract class NonScalarFactor extends Factor {
  def +(other: Factor): Factor = other match {
    case Scalar => this
    case _ => throw NumberException("cannot add non-Scalar factors together")
  }
}

case object Scalar extends Factor {
  override def value: Double = 1

  override def toString: String = ""

  def +(other: Factor): Factor = other
}

case object Pi extends NonScalarFactor {
  override def value: Double = Math.PI

  override def toString: String = Factor.sPi
}

case object E extends Factor {
  override def value: Double = Math.E

  override def toString: String = "e"

  override def +(other: Factor): Factor = other match {
    case Scalar => this
    case _ => throw NumberException("cannot add non-Scalar factors together")
  }
}

object Factor {
  val sE = "\uD835\uDF00"
  val sPi = "\uD835\uDED1"
  val sPiAlt0 = "pi"
  val sPiAlt1 = "Pi"
  val sPiAlt2 = "PI"

  def apply(w: String): Factor = w match {
    case `sPi` | `sPiAlt0` | `sPiAlt1` | `sPiAlt2` => Pi
    case `sE` => E
    case _ => Scalar
  }
}

case class NumberException(str: String) extends Exception(str)

case class NumberExceptionWithCause(str: String, e: Throwable) extends Exception(str, e)

