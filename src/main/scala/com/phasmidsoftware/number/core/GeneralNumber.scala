package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Number.{negate, prepareWithSpecialize}

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.util._

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * TODO continue refactoring to merge similar methods, particularly in GeneralNumber and FuzzyNumber.
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Radian, and NatLog.
  * @param fuzz   the (optional) fuzziness of this Number.
  */
abstract class GeneralNumber(val value: Value, val factor: Factor, val fuzz: Option[Fuzziness[Double]]) extends Number with Fuzz[Double] {

  self =>

  /**
    * Action to materialize this Expression.
    *
    * @return this ExactNumber.
    */
  def materialize: Number = this

  /**
    * @return Some(factor).
    */
  def maybeFactor: Option[Factor] = Some(factor)

  /**
    * Method to determine if this is a valid Number.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  def isValid: Boolean = maybeDouble.isDefined

  /**
    * Method to get the value of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */

  def toDouble: Option[Double] = maybeDouble

  /**
    * Method to get the value of this Number as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.convertDouble(x).
    *
    * @return an Option of Rational.
    */
  def toRational: Option[Rational] = maybeRational

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int] = maybeInt

  /**
    * Method to determine if this Number is positive.
    * Use case: does the String representation not start with a "-"?
    *
    * CONSIDER evaluating toString instead.
    *
    * @return true if this Number is greater than or equal to 0.
    */
  def isPositive: Boolean = signum >= 0

  /**
    * Negative of this Number.
    */
  def makeNegative: Number = doMultiply(Number(-1))

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e. this/n.
    */
  def doDivide(n: Number): Number = doMultiply(Number.inverse(n))

  /**
    * Yields the square root of this Number.
    * If possible, the result will be exact.
    */
  def sqrt: Number = Number.power(this, Number(Rational.half))

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the sine of this.
    */
  def sin: Number = Number.sin(this)

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the cosine.
    */
  def cos: Number = Number.negate(negate(scale(Radian)) doAdd Number(Rational.half, Radian).makeNegative).sin

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: Number): Number = Number.atan(this, y)

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the natural log of this.
    */
  def log: Number = Number.log(this)

  /**
    * Method to raise NatLog to the power of this number.
    * The result will be a Number with NatLog factor.
    *
    * @return the e to the power of this.
    */
  def exp: Number = Number.exp(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int = Number.signum(this)

  /**
    * Method to yield the absolute value of this Number.
    *
    * @return this if its positive, else - this.
    */
  def abs: Number = if (signum >= 0) this else makeNegative

  /**
    * Method to create a new version of this, but with factor f.
    * NOTE: the result will have the same absolute magnitude as this.
    * In other words,  in the case where f is not factor, the numerical value of the result's value will be different
    * from this value.
    *
    * @param f the new factor for the result.
    * @return a Number based on this and factor.
    */
  def scale(f: Factor): Number = Number.scale(this, f)

  /**
    * Action to render this GeneralNumber as a String.
    *
    * @return a String.
    */
  def render: String = toString

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * @param other the Number to be compared with.
    * @param p     the confidence expressed as a fraction of 1 (0.5 would be a typical value).
    * @return -1, 0, 1 as usual.
    */
  def fuzzyCompare(other: Number, p: Double): Int = Number.fuzzyCompare(this, other, p)

  /**
    * Make a copy of this FuzzyNumber but with additional fuzz given by f.
    *
    * @param f the additional fuzz.
    * @return this but with fuzziness which is the convolution of fuzz and f.
    */
  def addFuzz(f: Fuzziness[Double]): Number = FuzzyNumber.addFuzz(this, f)
  //
  //  /**
  //    * Be careful when implementing this method that you do not invoke a method recursively.
  //    *
  //    * CONSIDER is there really any need for this to be different from ExactNumber?
  //    *
  //    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
  //    * @return a Number which is the the result, possibly fuzzy, of invoking f on this.
  //    */
  //  protected def makeFuzzyIfAppropriate(f: Number => Number, relativePrecision: Int): Number = f(this) match {
  //    case x: GeneralNumber => x.addFuzz(Fuzziness.createFuzz(relativePrecision))
  //  }

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadic(other: Number, f: Factor)(op: DyadicOperation): Option[Number] = doComposeDyadic(other, f)(op.functions)

  /**
    * Evaluate a monadic operator on this.
    *
    * CONSIDER the exact cases don't change the fuzziness at all.
    * But, if there is already fuzziness, then the monadic function should change it.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def transformMonadic(f: Factor)(op: MonadicOperation): Option[Number] =
    Operations.doTransformValueMonadic(value)(op.functions) flatMap {
      case v@Right(x)
        if op.isExact(x) => Some(make(v, f))
      case v@Left(Right(x))
        if op.isExact(x) => Some(make(v, f))
      case v =>
        make(v, f) match {
          case n: GeneralNumber => for (t <- toDouble; x <- n.toDouble) yield n.make(monadicFuzziness(op, t, x))
        }
    }

  private def monadicFuzziness(op: MonadicOperation, t: Double, x: Double): Option[Fuzziness[Double]] = {
    val functionalFuzz = Fuzziness.map(t, x, !op.absolute, op.derivative, fuzz)
    Fuzziness.combine(t, t, relative = true, independent = true)((functionalFuzz, Some(Fuzziness.createFuzz(op.fuzz))))
  }

  /**
    * Evaluate a query operator on this.
    *
    * @param op the appropriate QueryOperation.
    * @return a Boolean.
    */
  def query[T](op: QueryOperation[T], defaultVal: => T): T = Operations.doQuery(value, op.getFunctions).getOrElse(defaultVal)

  /**
    * Render this Number in String form, including the factor.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    factor match {
      case Logarithmic(_) =>
        sb.append(factor.render(value))
      case PureNumber(_) =>
        sb.append(Value.valueToString(value))
        sb.append(factor.toString)
      case Root(_) =>
        sb.append(factor.render(value))
    }
    sb.toString
  }

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the factor will change.
    * This method does not need to be followed by a call to specialize.
    *
    * @param f the factor.
    * @return a Number.
    */
  def make(f: Factor): Number = make(value, f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return a Number.
    */
  def make(v: Value): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return a Number.
    */
  def make(v: Int, f: Factor): Number = make(Value.fromInt(v), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return a Number.
    */
  def make(v: Int): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * NOTE that this method is primarily used by doComposeDyadic
    *
    * @param r a Rational.
    * @param f Factor.
    * @return a Number.
    */
  def make(r: Rational, f: Factor): Number = make(Value.fromRational(r), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param r the value.
    * @return a Number.
    */
  def make(r: Rational): Number = make(r, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param x the value (a Double).
    * @param f Factor.
    * @return a Number.
    */
  def make(x: Double, f: Factor): Number = make(Value.fromDouble(Some(x)), f)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  def normalize: Number = scale(Scalar)

  /**
    * Return a Number which uses the most restricted type possible.
    * A Number based on a Double will yield a Number based on a Rational (if the conversion is exact).
    * A Number based on a Rational will yield a Number based on a BigInt (if there is a unit denominator).
    * A Number based on a BigInt will yield a Number based on a Int (if it is sufficiently small).
    *
    * @return a Number with the same magnitude as this.
    */
  //protected
  def specialize: Number = value match {
    // XXX Int case
    case Right(_) => this
    // XXX Rational case
    case Left(Right(r)) =>
      Try(r.toInt) match {
        case Success(b) => make(b).specialize
        case _ => this
      }
    // XXX Double case
    case d@Left(Left(Some(x))) =>
      // NOTE: here we attempt to deal with Doubles.
      // If a double can be represented by a BigDecimal with scale 0, 1, or 2 then we treat it as exact.
      // Otherwise, we will give it appropriate fuzziness.
      // In general, if you wish to have more control over this, then define your input using a String.
      // CONSIDER will this handle numbers correctly which are not close to 1?
      val r = Rational(x)
      r.toBigDecimal.scale match {
        case 0 | 1 | 2 => make(r).specialize
        // CONSIDER in following line adding fuzz only if this Number is exact.
        case n => FuzzyNumber(d, factor, fuzz).addFuzz(AbsoluteFuzz(Fuzziness.toDecimalPower(5, -n), Box))
      }
    // XXX Invalid case
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
    * CONSIDER renaming this alignValueTypes
    *
    * @param q the Number to be aligned with this.
    * @return a tuple of two Numbers, the first of which will be the more general type:
    *         (Invalid vs. Double, Double vs. Rational, Rational vs. Int).
    */
  //protected
  def alignTypes(q: Number): (Number, Number) = q match {
    case x: GeneralNumber =>
      value match {
        // XXX this is an invalid Number: return a pair of invalid numbers
        case Left(Left(None)) => (this, this)
        // XXX this value is a real Number: convert x to a Number based on real.
        case Left(Left(Some(_))) => x.value match {
          // XXX x's value is invalid: swap the order so the the first element is invalid
          case Left(Left(None)) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as a Double
          case _ => (this, Number.prepare(x.maybeDouble.map(y => make(y, x.factor, x.fuzz).specialize)))
        }
        // XXX this value is a Rational:
        case Left(Right(_)) => x.value match {
          // XXX x's value is a real Number: swap the order so that the first element is the real number
          case Left(Left(_)) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as a Rational
          case _ => (this, x.make(x.maybeRational.getOrElse(Rational.NaN)).specialize)
        }
        // XXX this value is an Int:
        case Right(_) => x.value match {
          // XXX x's value is a BigInt, Rational or real Number: swap the order so that the first element is the BigInt/Rational/real number
          case Left(_) => x.alignTypes(this)
          // XXX otherwise: return this and x re-cast as an Int
          case _ => (this, x.make(x.maybeInt.getOrElse(0), x.factor).specialize)
        }
      }
  }

  /**
    * Method to ensure that the value is within some factor-specific range.
    * In particular, Radian=based numbers are modulated to the range 0..2
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
    val (fInt, fRational, fDouble) = functions

    def tryDouble(xo: Option[Double]): Try[Number] = xo match {
      case Some(n) => toTryWithThrowable(for (y <- other.maybeDouble) yield fDouble(n, y) map (x => make(x, f)), NumberException("other is not a Double")).flatten
      case None => Failure(NumberException("number is invalid")) // NOTE this case is not observed in practice
    }

    def tryConvert[X](x: X, msg: String)(extract: Number => Option[X], func: (X, X) => Try[X], g: (X, Factor) => Number): Try[Number] =
      toTryWithThrowable(for (y <- extract(other)) yield func(x, y) map (g(_, f)), NumberException(s"other is not a $msg")).flatten

    def tryRational(x: Rational): Try[Number] = tryConvert(x, "Rational")({ case n: GeneralNumber => n.maybeRational }, fRational, make)

    def tryInt(x: Int): Try[Number] = tryConvert(x, "Int")({ case n: GeneralNumber => n.maybeInt }, fInt, make)

    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => tryMap(y)(tryRational, tryDouble)

    tryMap(value)(tryInt, xToZy1).toOption
  }

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using query
    */
  lazy val maybeRational: Option[Rational] = {
    import Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  def maybeDouble: Option[Double] = Value.maybeDouble(value)

  /**
    * An optional Int that corresponds to the value of this Number (but ignoring the factor).
    *
    * CONSIDER using query
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
    tryMap(value)(identityTry, xToZy1).toOption
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[GeneralNumber]

  override def equals(other: Any): Boolean = other match {
    case that: GeneralNumber =>
      (that canEqual this) &&
        value == that.value &&
        factor == that.factor &&
        fuzz == that.fuzz
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value, factor, fuzz)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object GeneralNumber {
  def plus(x: Number, y: Number): Number = x match {
    case z: GeneralNumber =>
      val (a, b) = z.alignFactors(y)
      a.factor match {
        case NatLog => plusAligned(a.scale(Scalar), b.scale(Scalar))
        case _ => plusAligned(a, b)
      }
  }

  @tailrec
  def times(x: Number, y: Number): Number = x match {
    case a: GeneralNumber =>
      y match {
        case n@FuzzyNumber(_, _, _) => n doMultiply x
        case z: GeneralNumber =>
          val (p, q) = a.alignTypes(z)
          p.factor + q.factor match {
            case Some(NatLog) => prepareWithSpecialize(p.composeDyadic(q, NatLog)(DyadicOperationPlus))
            case Some(f) => prepareWithSpecialize(p.composeDyadic(q, f)(DyadicOperationTimes))
            case None => times(x.scale(Scalar), y.scale(Scalar))
          }
      }
  }

  private def plusAligned(x: Number, y: Number): Number = (x, y) match {
    case (a: GeneralNumber, b: GeneralNumber) =>
      y match {
        case n@FuzzyNumber(_, _, _) => n doAdd x
        case _ =>
          val (p, q) = a.alignTypes(b)
          prepareWithSpecialize(p.composeDyadic(q, p.factor)(DyadicOperationPlus))
      }
  }
}