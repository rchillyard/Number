package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.core.Number.prepareWithSpecialize
import com.phasmidsoftware.number.core.Rational.RationalHelper

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.util._

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * TODO try to refactor in such a way that we reduce the number of methods defined here,
  * especially those which are not implementations of an abstract method.
  *
  * CONSIDER including the fuzziness in Number and simply having ExactNumber always have fuzz of None.
  *
  * CONSIDER implementing equals. However, be careful!
  *
  * @param value  the value of the Number, expressed as a nested Either type.
  * @param factor the scale factor of the Number: valid scales are: Scalar, Pi, and E.
  * @param fuzz   the (optional) fuzziness of this Number.
  */
abstract class GeneralNumber(val value: Value, val factor: Factor, val fuzz: Option[Fuzziness[Double]]) extends AtomicExpression with Fuzz[Double] with Number {

  self =>

  /**
    * Auxiliary constructor for the usual situation with the default factor and no fuzziness.
    *
    * @param v the value for the new Number.
    */
  def this(v: Value) = this(v, Scalar, None)

  /**
    * Action to materialize this Expression.
    *
    * @return this ExactNumber.
    */
  def materialize: Number = this

  def maybeFactor: Option[Factor] = Some(factor)

  /**
    * Method to determine if this is a valid Number.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid Number
    */
  override def isValid: Boolean = maybeDouble.isDefined

  /**
    * Method to get the value of this Number as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */

  override def toDouble: Option[Double] = maybeDouble

  /**
    * Method to get the value of this Number as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.convertDouble(x).
    *
    * @return an Option of Rational.
    */
  override def toRational: Option[Rational] = maybeRational

  /**
    * Method to get the value of this Number as an Int.
    *
    * @return an Option of Int. If this Number cannot be converted to an Int, then None will be returned.
    */
  override def toInt: Option[Int] = maybeInt

  /**
    * Method to determine if this Number is positive.
    * Use case: does the String representation not start with a "-"?
    *
    * CONSIDER evaluating toString instead.
    *
    * @return true if this Number is greater than or equal to 0.
    */
  override def isPositive: Boolean = signum >= 0
  /**
    * Negative of this Number.
    */
  override def makeNegative: Number = doMultiply(Number(-1))

  /**
    * Add this Number to n.
    *
    * @param n another Number.
    * @return the sum of this and n.
    */
  override def doAdd(n: Number): Number = this match {
    case x: GeneralNumber => GeneralNumber.plus(x, n)
  }

  /**
    * Multiply this Number by n.
    *
    * @param n another Number.
    * @return the product of this and n.
    */
  override def doMultiply(n: Number): Number = GeneralNumber.times(this, n)

  /**
    * Divide this Number by n.
    *
    * @param n another Number.
    * @return this quotient of this and n, i.e. this/n.
    */
  override def doDivide(n: Number): Number = doMultiply(Number.inverse(n))

  /**
    * Raise this Number to the power p.
    *
    * @param p a Number.
    * @return this Number raised to the power of p.
    */
  override def doPower(p: Number): Number = Number.power(this, p)

  /**
    * Yields the square root of this Number.
    * If possible, the result will be exact.
    */
  override def sqrt: Number = Number.power(this, Number(r"1/2"))

  /**
    * Method to determine the sine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the sine of this.
    */
  override def sin: Number = makeFuzzyIfAppropriate(Number.sin, 3)

  /**
    * Method to determine the cosine of this Number.
    * The result will be a Number with Scalar factor.
    *
    * @return the cosine.
    */
  override def cos: Number = Number.negate(scale(Pi) doAdd Number(Rational.half, Pi).makeNegative).sin

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  override def atan(y: Number): Number = makeFuzzyIfAppropriate(x => Number.atan(x, y), 3)

  /**
    * Method to determine the natural log of this Number.
    * The result will be a Number with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the natural log of this.
    */
  override def log: Number = makeFuzzyIfAppropriate(Number.log, 3)

  /**
    * Method to raise E to the power of this number.
    * The result will be a Number with E factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the e to the power of this.
    */
  override def exp: Number = makeFuzzyIfAppropriate(Number.exp, 3)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  override def signum: Int = Number.signum(this)

  /**
    * Method to yield the absolute value of this Number.
    *
    * @return this if its positive, else - this.
    */
  override def abs: Number = if (signum >= 0) this else makeNegative

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
  override def scale(f: Factor): Number = makeFuzzyIfAppropriate(x => Number.scale(x, f), 0)

  /**
    * Evaluate the magnitude squared of this Complex number.
    *
    * @return the magnitude squared.
    */
  def magnitudeSquared: Expression = this * this

  def compare(other: Number): Int = Number.doCompare(this, other)

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * CONSIDER do we really need this to be different?
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

  /**
    * Be careful when implementing this method that you do not invoke a method recursively.
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return a Number which is the the result, possibly fuzzy, of invoking f on this.
    */
  override protected def makeFuzzyIfAppropriate(f: Number => Number, relativePrecision: Int): Number = {
    val number = f(this).asInstanceOf[GeneralNumber]
    number.asFuzzyNumber.addFuzz(Fuzziness.createFuzz(relativePrecision))
  }

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a Number.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operands this and other.
    */
  override def composeDyadic(other: Number, f: Factor)(op: DyadicOperation): Option[Number] = doComposeDyadic(other, f)(op.functions)

  /**
    * Evaluate a monadic operator on this.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  override def transformMonadic(f: Factor)(op: MonadicOperation): Option[Number] = doTransformMonadic(f)(op.functions)

  /**
    * Evaluate a query operator on this.
    *
    * @param op the appropriate QueryOperation.
    * @return a Boolean.
    */
  override def query(op: QueryOperation): Boolean = doQuery(op.getFunctions).getOrElse(false)

  /**
    * Render this Number in String form, including the factor.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    factor match {
      case E =>
        sb.append(Number.asPowerOfE(value))
      case f =>
        sb.append(Number.valueToString(value))
        sb.append(f.toString)
    }
    sb.toString
  }

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
  override protected def make(f: Factor): Number = make(value, f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Number.
    */
  override def make(v: Value): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return either a Number.
    */
  override protected def make(v: Int, f: Factor): Number = make(Value.fromInt(v), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a Number.
    */
  override protected def make(v: Int): Number = make(v, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * NOTE that this method is primarily used by doComposeDyadic
    *
    * @param r a Rational.
    * @param f Factor.
    * @return either a Number.
    */
  override protected def make(r: Rational, f: Factor): Number = make(Value.fromRational(r), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param r the value.
    * @return either a Number.
    */
  override protected def make(r: Rational): Number = make(r, factor)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param x the value (a Double).
    * @param f Factor.
    * @return either a Number.
    */
  override protected def make(x: Double, f: Factor): Number = make(Value.fromDouble(Some(x)), f)

  /**
    * Make a copy of this Number, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * TEST me
    *
    * @param x the value (a Double).
    * @return either a Number.
    */
  override protected def make(x: Double): Number = make(x, factor)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new Number with factor of Scalar but with the same magnitude as this.
    */
  override def normalize: Number = factor match {
    case Scalar => this
    // TEST me
    case Pi | E => Number.prepare(maybeDouble map (x => self.make(x * factor.value).specialize.asInstanceOf[GeneralNumber].make(Scalar)))
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
  def alignTypes(q: Number): (Number, Number) = {
    val x = q.asInstanceOf[GeneralNumber]
    value match {
      // XXX this is an invalid Number: return a pair of invalid numbers
      case Left(Left(None)) => (this, this)
      // XXX this value is a real Number: convert x to a Number based on real.
      case Left(Left(Some(_))) => x.value match {
        // XXX x's value is invalid: swap the order so the the first element is invalid
        case Left(Left(None)) => x.alignTypes(this)
        // XXX otherwise: return this and x re-cast as a Double
        case _ => (this, Number.prepare(x.maybeDouble.map(y => make(y, x.factor).specialize)))
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
    * In particular, Pi=based numbers are modulated to the range 0..2
    *
    * @return this or an equivalent Number.
    */
  override def modulate: Number = Number.modulate(this)

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
      case None => Failure(NumberException("number is invalid"))
    }

    def tryConvert[X](x: X, msg: String)(extract: Number => Option[X], func: (X, X) => Try[X], g: (X, Factor) => Number): Try[Number] =
      toTryWithThrowable(for (y <- extract(other)) yield func(x, y) map (g(_, f)), NumberException(s"other is not a $msg")).flatten

    def tryRational(x: Rational): Try[Number] = tryConvert(x, "Rational")(n => n.asInstanceOf[GeneralNumber].maybeRational, fRational, make)

    def tryInt(x: Int): Try[Number] = tryConvert(x, "Int")(n => n.asInstanceOf[GeneralNumber].maybeInt, fInt, make)

    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[Number] = y => tryMap(y)(tryRational, tryDouble)

    tryMap(value)(tryInt, xToZy1).toOption
  }

  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    *
    * @param f         the factor to be used for the result.
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  def doTransformMonadic(f: Factor)(functions: MonadicFunctions): Option[Number] =
    Operations.doTransformValueMonadic(value)(functions) map (make(_, f))

  /**
    * Evaluate a query operator on this, using the various functions passed in.
    *
    * @param functions the tuple of four conversion functions.
    * @return a new Number which is result of applying the appropriate function to the operand this.
    */
  private def doQuery(functions: QueryFunctions): Option[Boolean] = Operations.doQuery(value, functions)

  /**
    * An optional Rational that corresponds to the value of this Number (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using MonadicTransformations
    */
  lazy val maybeRational: Option[Rational] = {
    import Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

  /**
    * An optional Double that corresponds to the value of this Number (but ignoring the factor).
    */
  def maybeDouble: Option[Double] = optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, identity))

  /**
    * Ensure that this Number is actually a FuzzyNumber.
    *
    * TODO make this return a Number (or Number).
    *
    * @return a FuzzyNumber which is the same as this Number.
    */
  def asFuzzyNumber: FuzzyNumber

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
    tryMap(value)(identityTry, xToZy1).toOption
  }
}

object GeneralNumber {
  def plus(x: GeneralNumber, y: Number): Number = {
    val (a, b) = x.alignFactors(y)
    a.factor match {
      case E => plusAligned(a.scale(Scalar), b.scale(Scalar))
      case _ => plusAligned(a, b)
    }
  }

  @tailrec
  def times(x: GeneralNumber, y: Number): Number =
    y match {
      case n@FuzzyNumber(_, _, _) => recover((n multiply x).materialize.asNumber, NumberException("logic error: plusAligned"))
      case _ =>
        val (p, q) = x.alignTypes(y.asInstanceOf[GeneralNumber])
        p.factor + q.factor match {
          case Some(E) => prepareWithSpecialize(p.composeDyadic(q, E)(DyadicOperationPlus))
          case Some(f) => prepareWithSpecialize(p.composeDyadic(q, f)(DyadicOperationTimes))
          case None => times(x.scale(Scalar).asInstanceOf[GeneralNumber], y.scale(Scalar))
        }
    }

  private def plusAligned(x: Number, y: Number): Number =
    y match {
      case n@FuzzyNumber(_, _, _) => recover((n plus x).materialize.asNumber, NumberException("logic error: plusAligned"))
      case _ =>
        val (p, q) = x.asInstanceOf[GeneralNumber].alignTypes(y.asInstanceOf[GeneralNumber])
        prepareWithSpecialize(p.composeDyadic(q, p.factor)(DyadicOperationPlus))
    }

}