package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.FP._
import com.phasmidsoftware.number.core.Field.recover
import com.phasmidsoftware.number.core.FuzzyNumber.NumberIsFuzzy
import com.phasmidsoftware.number.core.GeneralNumber.{negate, prepare}
import com.phasmidsoftware.number.core.Rational.{RationalHelper, toInts}
import com.phasmidsoftware.number.core.Render.renderValue
import com.phasmidsoftware.number.core.Value._
import com.phasmidsoftware.number.parse.NumberParser

import java.util.NoSuchElementException
import scala.annotation.tailrec
import scala.math.BigInt
import scala.util._

/**
  * This class is designed to model an exact Numeral.
  * See GeneralNumber for more details on the actual representation.
  *
  * TODO implement scientific notation by having factors such os 10^3, 10^6, etc. (alternatively, add a separate parameter)
  *
  * @param value  the value of the GeneralNumber, expressed as a nested Either type.
  * @param factor the scale factor of the GeneralNumber: valid scales are: Scalar, Pi, and E.
  */
case class ExactNumber(override val value: Value, override val factor: Factor) extends GeneralNumber(value, factor) {

  /**
    * @return true.
    */
  def isExact: Boolean = true

  /**
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new GeneralNumber.
    */
  def this(v: Value) = this(v, Scalar)

  /**
    * Method to get this fuzziness of this GeneralNumber.
    *
    * @return None
    */
  def fuzz: Option[Fuzziness[Double]] = None

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return an ExactNumber.
    */
  def make(v: Value, f: Factor): GeneralNumber = ExactNumber(v, f)

  /**
    * If the result of invoking f on this is a Double, then there will inevitably be some loss of precision.
    *
    * CONSIDER rewriting this so that we don't have to override the method. But be careful!
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return a GeneralNumber which is the square toot of this, possibly fuzzy, GeneralNumber.
    */
  override protected def makeFuzzyIfAppropriate(f: GeneralNumber => GeneralNumber, relativePrecision: Int): GeneralNumber = {
    val z = f(this)
    z.value match {
      case v@Left(Left(Some(_))) => FuzzyNumber(v, z.factor, Some(Fuzziness.createFuzz(relativePrecision)))
      case v => z.make(v) // NOTE: do not attempt to convert this to a FuzzyNumber because it will cause a stack overflow
    }
  }

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    * the result will be zero.
    *
    * TEST me
    *
    * @param p the confidence desired (ignored).
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int = signum

  /**
    * Action to render this ExactNumber as a String.
    *
    * TEST me
    *
    * @return a String.
    */
  def render: String = toString

  /**
    * Action to materialize this Expression.
    *
    * @return this ExactNumber.
    */
  def materialize: GeneralNumber = this

  def asFuzzyNumber: FuzzyNumber = FuzzyNumber(value, factor, None)
}

/**
  * This class is designed to model a Numerical value of various possible different types.
  * These types are: Int, BigInt, Rational, Double.
  *
  * TODO try to refactor in such a way that we reduce the number of methods defined here,
  * especially those which are not implementations of an abstract method.
  *
  * CONSIDER including the fuzziness in GeneralNumber and simply having ExactNumber always have fuzz of None.
  *
  * CONSIDER implementing equals. However, be careful!
  *
  * @param value  the value of the GeneralNumber, expressed as a nested Either type.
  * @param factor the scale factor of the GeneralNumber: valid scales are: Scalar, Pi, and E.
  */
abstract class GeneralNumber(val value: Value, val factor: Factor) extends AtomicExpression with Ordered[GeneralNumber] with Field {

  self =>

  /**
    * Auxiliary constructor for the usual situation with the default factor.
    *
    * @param v the value for the new GeneralNumber.
    */
  def this(v: Value) = this(v, Scalar)

  // CONSIDER implementing equals (but be careful!)
  //  override def equals(obj: Any): Boolean = obj match {
  //    case x: GeneralNumber => this.compare(x) == 0
  //    case _ => false
  //  }

  def maybeFactor: Option[Factor] = Some(factor)

  /**
    * Method to determine if this is a valid GeneralNumber.
    * An invalid number has a value of form Left(Left(Left(None)))
    *
    * @return true if this is a valid GeneralNumber
    */
  def isValid: Boolean = maybeDouble.isDefined

  /**
    * Method to get this fuzziness of this GeneralNumber.
    *
    * @return for an Exact number, this will be None, otherwise the actual fuzz.
    */
  def fuzz: Option[Fuzziness[Double]]

  /**
    * Method to get the value of this GeneralNumber as an optional Double.
    *
    * @return an Some(Double) which is the closest possible value to the nominal value, otherwise None if this is invalid.
    */

  def toDouble: Option[Double] = maybeDouble

  /**
    * Method to get the value of this GeneralNumber as a Rational.
    * If this is actually a Double, it will be converted to a Rational according to the implicit conversion from Double to Rational.
    * See Rational.convertDouble(x).
    *
    * @return an Option of Rational.
    */
  def toRational: Option[Rational] = maybeRational

  /**
    * Method to get the value of this GeneralNumber as an Int.
    *
    * @return an Option of Int. If this GeneralNumber cannot be converted to an Int, then None will be returned.
    */
  def toInt: Option[Int] = maybeInt

  /**
    * Method to determine if this GeneralNumber is positive.
    * Use case: does the String representation not start with a "-"?
    *
    * CONSIDER evaluating toString instead.
    *
    * @return true if this GeneralNumber is greater than or equal to 0.
    */
  def isPositive: Boolean = signum >= 0

  /**
    * Add x to this GeneralNumber and return the result.
    * See GeneralNumber.plus for more detail.
    * CONSIDER inlining this method.
    *
    * @param x the addend.
    * @return the sum.
    */
  def add(x: Field): Field = x match {
    case n@GeneralNumber(_, _) => doAdd(n)
    case c@Complex(_, _) => c.add(x)
  }

  /**
    * Multiply this GeneralNumber by x and return the result.
    * See GeneralNumber.times for more detail.
    *
    * * @param x the multiplicand.
    * * @return the product.
    */
  def multiply(x: Field): Field = x match {
    case n@GeneralNumber(_, _) => doMultiply(n)
    case c@Complex(_, _) => c.multiply(x)
  }

  /**
    * Divide this GeneralNumber by x and return the result.
    * See * and invert for more detail.
    *
    * @param x the divisor.
    * @return the quotient.
    */
  def divide(x: Field): Field = x match {
    case n@GeneralNumber(_, _) => doDivide(n)
    case c@Complex(_, _) => c.divide(x)
  }

  /**
    * Change the sign of this GeneralNumber.
    */
  def unary_- : Field = makeNegative

  /**
    * Raise this GeneralNumber to the power p.
    *
    * @param p a GeneralNumber.
    * @return this GeneralNumber raised to power p.
    */
  def power(p: GeneralNumber): Field = p match {
    case n@GeneralNumber(_, _) => doPower(n)
    case _ => throw NumberException("logic error: power not supported for non-GeneralNumber powers")
  }

  def power(p: Int): Field = power(GeneralNumber(p))

  /**
    * Negative of this GeneralNumber.
    */
  def makeNegative: GeneralNumber = doMultiply(GeneralNumber(-1))

  /**
    * Add this GeneralNumber to n.
    *
    * @param n another GeneralNumber.
    * @return the sum of this and n.
    */
  def doAdd(n: GeneralNumber): GeneralNumber = GeneralNumber.plus(this, n)

  /**
    * Multiply this GeneralNumber by n.
    *
    * @param n another GeneralNumber.
    * @return the product of this and n.
    */
  def doMultiply(n: GeneralNumber): GeneralNumber = GeneralNumber.times(this, n)

  /**
    * Divide this GeneralNumber by n.
    *
    * @param n another GeneralNumber.
    * @return this quotient of this and n, i.e. this/n.
    */
  def doDivide(n: GeneralNumber): GeneralNumber = doMultiply(n.invert)

  /**
    * Raise this GeneralNumber to the power p.
    *
    * @param p a GeneralNumber.
    * @return this GeneralNumber raised to the power of p.
    */
  def doPower(p: GeneralNumber): GeneralNumber = GeneralNumber.power(this, p)

  /**
    * Yields the inverse of this GeneralNumber.
    * This GeneralNumber is first normalized so that its factor is Scalar, since we cannot directly invert Numbers with other
    * factors.
    */
  def invert: GeneralNumber = GeneralNumber.inverse(normalize)

  /**
    * Yields the square root of this GeneralNumber.
    * If possible, the result will be exact.
    */
  def sqrt: GeneralNumber = GeneralNumber.power(this, GeneralNumber(r"1/2"))

  /**
    * Method to determine the sine of this GeneralNumber.
    * The result will be a GeneralNumber with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the sine of this.
    */
  def sin: GeneralNumber = makeFuzzyIfAppropriate(GeneralNumber.sin, 3)

  /**
    * Method to determine the cosine of this GeneralNumber.
    * The result will be a GeneralNumber with Scalar factor.
    *
    * @return the cosine.
    */
  def cos: GeneralNumber = negate(scale(Pi) doAdd GeneralNumber(Rational.half, Pi).makeNegative).sin

  /**
    * Calculate the angle whose opposite length is y and whose adjacent length is this.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @param y the opposite length
    * @return the angle defined by x = this, y = y
    */
  def atan(y: GeneralNumber): GeneralNumber = makeFuzzyIfAppropriate(x => GeneralNumber.atan(x, y), 3)

  /**
    * Method to determine the natural log of this GeneralNumber.
    * The result will be a GeneralNumber with Scalar factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the natural log of this.
    */
  def log: GeneralNumber = makeFuzzyIfAppropriate(GeneralNumber.log, 3)

  /**
    * Method to raise E to the power of this number.
    * The result will be a GeneralNumber with E factor.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @return the e to the power of this.
    */
  def exp: GeneralNumber = makeFuzzyIfAppropriate(GeneralNumber.exp, 3)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    *
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum: Int = GeneralNumber.signum(this)

  /**
    * Method to determine the sense of this number: negative, zero, or positive.
    * If this FuzzyNumber cannot be distinguished from zero with p confidence, then
    *
    * @param p the confidence desired.
    * @return an Int which is negative, zero, or positive according to the magnitude of this.
    */
  def signum(p: Double): Int

  /**
    * Method to yield the absolute value of this GeneralNumber.
    *
    * @return this if its positive, else - this.
    */
  def abs: GeneralNumber = if (signum >= 0) this else makeNegative

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
    * @return a GeneralNumber based on this and factor.
    */
  def scale(f: Factor): GeneralNumber = makeFuzzyIfAppropriate(x => GeneralNumber.scale(x, f), 0)

  /**
    * @return true if this GeneralNumber is equal to zero.
    */
  def isZero: Boolean = GeneralNumber.isZero(this)

  /**
    * @return true if this GeneralNumber is equal to zero.
    */
  def isInfinite: Boolean = GeneralNumber.isInfinite(this)

  /**
    * Evaluate the magnitude squared of this Complex number.
    *
    * @return the magnitude squared.
    */
  def magnitudeSquared: Expression = this * this

  /**
    * Method to compare this with another GeneralNumber.
    * The difference between this method and that of ExactNumber is that the signum method is implemented differently.
    *
    * @param other the other GeneralNumber.
    * @return -1, 0, or 1 according to whether x is <, =, or > y.
    */
  override def compare(other: GeneralNumber): Int = GeneralNumber.doCompare(this, other)

  /**
    * Perform a fuzzy comparison where we only require p confidence to know that this and other are effectively the same.
    *
    * CONSIDER do we really need this?
    *
    * @param other the GeneralNumber to be compared with.
    * @param p     the confidence expressed as a fraction of 1 (0.5 would be a typical value).
    * @return -1, 0, 1 as usual.
    */
  def fuzzyCompare(other: GeneralNumber, p: Double): Int = GeneralNumber.fuzzyCompare(this, other, p)

  /**
    * Be careful when implementing this method that you do not invoke a method recursively.
    *
    * @param relativePrecision the approximate number of bits of additional imprecision caused by evaluating a function.
    * @return a GeneralNumber which is the the result, possibly fuzzy, of invoking f on this.
    */
  protected def makeFuzzyIfAppropriate(f: GeneralNumber => GeneralNumber, relativePrecision: Int): GeneralNumber =
    f(this).asFuzzyNumber.addFuzz(Fuzziness.createFuzz(relativePrecision))

  /**
    * Evaluate a dyadic operator on this and other, using either plus, times, ... according to the value of op.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other the other operand, a GeneralNumber.
    * @param f     the factor to apply to the result.
    * @param op    the appropriate DyadicOperation.
    * @return a new GeneralNumber which is result of applying the appropriate function to the operands this and other.
    */
  def composeDyadic(other: GeneralNumber, f: Factor)(op: DyadicOperation): Option[GeneralNumber] = doComposeDyadic(other, f)(op.functions)

  /**
    * Evaluate a monadic operator on this.
    *
    * @param f  the factor to apply to the result.
    * @param op the appropriate MonadicOperation.
    * @return a new GeneralNumber which is result of applying the appropriate function to the operand this.
    */
  def transformMonadic(f: Factor)(op: MonadicOperation): Option[GeneralNumber] = doTransformMonadic(f)(op.functions)

  /**
    * Evaluate a query operator on this.
    *
    * @param op the appropriate QueryOperation.
    * @return a Boolean.
    */
  def query(op: QueryOperation): Boolean = doQuery(op.getFunctions).getOrElse(false)

  /**
    * Render this GeneralNumber in String form, including the factor.
    *
    * @return
    */
  override def toString: String = {
    val sb = new StringBuilder()
    factor match {
      case E =>
        sb.append(GeneralNumber.asPowerOfE(value))
      case f =>
        sb.append(GeneralNumber.valueToString(value))
        sb.append(f.toString)
    }
    sb.toString
  }

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Both the value and the factor will be changed.
    *
    * @param v the value.
    * @param f the factor.
    * @return either a GeneralNumber.
    */
  protected def make(v: Value, f: Factor): GeneralNumber

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the factor will change.
    * This method does not need to be followed by a call to specialize.
    *
    * TEST me
    *
    * @param f the factor.
    * @return either a GeneralNumber.
    */
  protected def make(f: Factor): GeneralNumber = make(value, f)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a GeneralNumber.
    */
  def make(v: Value): GeneralNumber = make(v, factor)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @param f Factor.
    * @return either a GeneralNumber.
    */
  protected def make(v: Int, f: Factor): GeneralNumber = make(Value.fromInt(v), f)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a GeneralNumber.
    */
  protected def make(v: Int): GeneralNumber = make(v, factor)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value and the factor will change.
    * This method should be followed by a call to specialize.
    *
    * @param r a Rational.
    * @param f Factor.
    * @return either a GeneralNumber.
    */
  protected def make(r: Rational, f: Factor): GeneralNumber = make(Value.fromRational(r), f)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * @param v the value.
    * @return either a GeneralNumber.
    */
  protected def make(v: Rational): GeneralNumber = make(v, factor)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value and factor will change.
    * This method should be followed by a call to specialize.
    *
    * NOTE that the value 0 (which represents 1 times the double-precision tolerance) is a guess.
    * It may not be appropriate for all invocations.
    *
    * @param v the value (a Double).
    * @param f Factor.
    * @return either a GeneralNumber.
    */
  protected def make(v: Double, f: Factor): GeneralNumber = makeFuzzyIfAppropriate(x => x.make(Value.fromDouble(Some(v)), f), 0)

  /**
    * Make a copy of this GeneralNumber, given the same degree of fuzziness as the original.
    * Only the value will change.
    * This method should be followed by a call to specialize.
    *
    * TEST me
    *
    * @param v the value (a Double).
    * @return either a GeneralNumber.
    */
  protected def make(v: Double): GeneralNumber = make(v, factor)

  /**
    * Method to "normalize" a number, that's to say make it a Scalar.
    *
    * @return a new GeneralNumber with factor of Scalar but with the same magnitude as this.
    */
  def normalize: GeneralNumber = factor match {
    case Scalar => this
    // TEST me
    case Pi | E => prepare(maybeDouble map (x => self.make(x * factor.value).specialize.make(Scalar)))
  }

  /**
    * Return a GeneralNumber which uses the most restricted type possible.
    * A GeneralNumber based on a Double will yield a GeneralNumber based on a Rational (if the conversion is exact).
    * A GeneralNumber based on a Rational will yield a GeneralNumber based on a BigInt (if there is a unit denominator).
    * A GeneralNumber based on a BigInt will yield a GeneralNumber based on a Int (if it is sufficiently small).
    *
    * @return a GeneralNumber with the same magnitude as this.
    */
  //protected
  lazy val specialize: GeneralNumber = value match {
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
        // CONSIDER in following line adding fuzz only if this GeneralNumber is exact.
        case n => FuzzyNumber(d, factor, fuzz).addFuzz(AbsoluteFuzz(Fuzziness.toDecimalPower(5, -n), Box))
      }
    // XXX Invalid case
    case _ => this
  }

  /**
    * Method to align the factors of this and x such that the resulting Numbers (in the tuple) each have the same factor.
    *
    * @param x the GeneralNumber to be aligned with this.
    * @return a tuple of two Numbers with the same factor.
    */
  //protected
  def alignFactors(x: GeneralNumber): (GeneralNumber, GeneralNumber) = factor match {
    case Scalar => (this, x.scale(factor))
    case _ => (scale(x.factor), x)
  }

  /**
    * Method to align the types of this and x such that the resulting Numbers (in the tuple) each have the same structure.
    *
    * CONSIDER renaming this alignValueTypes
    *
    * @param x the GeneralNumber to be aligned with this.
    * @return a tuple of two Numbers, the first of which will be the more general type:
    *         (Invalid vs. Double, Double vs. Rational, Rational vs. Int).
    */
  //protected
  def alignTypes(x: GeneralNumber): (GeneralNumber, GeneralNumber) = value match {
    // XXX this is an invalid GeneralNumber: return a pair of invalid numbers
    case Left(Left(None)) => (this, this)
    // XXX this value is a real GeneralNumber: convert x to a GeneralNumber based on real.
    case Left(Left(Some(_))) => x.value match {
      // XXX x's value is invalid: swap the order so the the first element is invalid
      case Left(Left(None)) => x.alignTypes(this)
      // XXX otherwise: return this and x re-cast as a Double
      case _ => (this, prepare(x.maybeDouble.map(y => make(y, x.factor).specialize)))
    }
    // XXX this value is a Rational:
    case Left(Right(_)) => x.value match {
      // XXX x's value is a real GeneralNumber: swap the order so that the first element is the real number
      case Left(Left(_)) => x.alignTypes(this)
      // XXX otherwise: return this and x re-cast as a Rational
      case _ => (this, x.make(x.maybeRational.getOrElse(Rational.NaN), x.factor).specialize)
    }
    // XXX this value is an Int:
    case Right(_) => x.value match {
      // XXX x's value is a BigInt, Rational or real GeneralNumber: swap the order so that the first element is the BigInt/Rational/real number
      case Left(_) => x.alignTypes(this)
      // XXX otherwise: return this and x re-cast as an Int
      case _ => (this, x.make(x.maybeInt.getOrElse(0), x.factor).specialize)
    }
  }

  /**
    * Method to ensure that the value is within some factor-specific range.
    * In particular, Pi=based numbers are modulated to the range 0..2
    *
    * @return this or an equivalent GeneralNumber.
    */
  def modulate: GeneralNumber = GeneralNumber.modulate(this)

  /**
    * Evaluate a dyadic operator on this and other, using the various functions passed in.
    * NOTE: this and other must have been aligned by type so that they have the same structure.
    *
    * @param other     the other operand, a GeneralNumber.
    * @param f         the factor to apply to the result.
    * @param functions the tuple of four conversion functions.
    * @return a new GeneralNumber which is result of applying the appropriate function to the operands this and other.
    */
  private def doComposeDyadic(other: GeneralNumber, f: Factor)(functions: DyadicFunctions): Option[GeneralNumber] = {
    val (fInt, fRational, fDouble) = functions

    def tryDouble(xo: Option[Double]): Try[GeneralNumber] = xo match {
      case Some(n) => toTryWithThrowable(for (y <- other.maybeDouble) yield fDouble(n, y) map (x => make(x, f)), NumberException("other is not a Double")).flatten
      case None => Failure(NumberException("number is invalid"))
    }

    def tryConvert[X](x: X, msg: String)(extract: GeneralNumber => Option[X], func: (X, X) => Try[X], g: (X, Factor) => GeneralNumber): Try[GeneralNumber] =
      toTryWithThrowable(for (y <- extract(other)) yield func(x, y) map (g(_, f)), NumberException(s"other is not a $msg")).flatten

    def tryRational(x: Rational): Try[GeneralNumber] = tryConvert(x, "Rational")(n => n.maybeRational, fRational, make)

    def tryInt(x: Int): Try[GeneralNumber] = tryConvert(x, "Int")(n => n.maybeInt, fInt, make)

    import Converters._
    val xToZy1: Either[Option[Double], Rational] => Try[GeneralNumber] = y => tryMap(y)(tryRational, tryDouble)

    tryMap(value)(tryInt, xToZy1).toOption
  }

  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    *
    * @param f         the factor to be used for the result.
    * @param functions the tuple of four conversion functions.
    * @return a new GeneralNumber which is result of applying the appropriate function to the operand this.
    */
  private def doTransformMonadic(f: Factor)(functions: MonadicFunctions): Option[GeneralNumber] =
    Operations.doTransformValueMonadic(value)(functions) map (make(_, f))

  /**
    * Evaluate a query operator on this, using the various functions passed in.
    *
    * @param functions the tuple of four conversion functions.
    * @return a new GeneralNumber which is result of applying the appropriate function to the operand this.
    */
  private def doQuery(functions: QueryFunctions): Option[Boolean] = Operations.doQuery(value, functions)

  /**
    * An optional Rational that corresponds to the value of this GeneralNumber (but ignoring the factor).
    * A Double value is not converted to a Rational since, if it could be done exactly, it already would have been.
    * CONSIDER using MonadicTransformations
    */
  private lazy val maybeRational: Option[Rational] = {
    import Converters._
    val ry = tryMap(value)(tryF(Rational.apply), x => tryMap(x)(identityTry, fail("no Double=>Rational conversion")))
    ry.toOption
  }

  /**
    * An optional Double that corresponds to the value of this GeneralNumber (but ignoring the factor).
    */
  private lazy val maybeDouble: Option[Double] = optionMap(value)(_.toDouble, x => optionMap(x)(_.toDouble, identity))

  /**
    * An optional Int that corresponds to the value of this GeneralNumber (but ignoring the factor).
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

  /**
    * Ensure that this GeneralNumber is actually a FuzzyNumber.
    *
    * @return a FuzzyNumber which is the same as this GeneralNumber.
    */
  def asFuzzyNumber: FuzzyNumber
}

object GeneralNumber {
  def unapply(arg: GeneralNumber): Option[(Value, Factor)] = Some(arg.value, arg.factor)

  /**
    * Exact value of 0
    */
  val zero: GeneralNumber = ExactNumber(Right(0), Scalar)
  /**
    * Exact value of -0
    */
  val negZero: GeneralNumber = ExactNumber(Left(Right(Rational(0, -1))), Scalar)
  /**
    * Exact value of 1
    */
  val one: GeneralNumber = ExactNumber(Right(1), Scalar)
  /**
    * Exact value of 1
    */
  val two: GeneralNumber = ExactNumber(Right(2), Scalar)
  /**
    * Exact value of pi
    */
  val pi: GeneralNumber = ExactNumber(Right(1), Pi)

  /**
    * Exact value of e
    */
  val e: GeneralNumber = ExactNumber(Right(1), E)

  implicit class NumberOps(x: Int) {

    /**
      * Add this x (a GeneralNumber) and yield a GeneralNumber.
      *
      * @param y the addend, a GeneralNumber.
      * @return a GeneralNumber whose value is x + y.
      */
    def +(y: GeneralNumber): GeneralNumber = GeneralNumber(x) doAdd y //.materialize

    /**
      * Multiply x by y (a GeneralNumber) and yield a GeneralNumber.
      *
      * @param y the multiplicand, a GeneralNumber.
      * @return a GeneralNumber whose value is x * y.
      */
    def *(y: GeneralNumber): GeneralNumber = GeneralNumber(x) doMultiply y //.materialize

    /**
      * Divide x by y (a GeneralNumber) and yield a GeneralNumber.
      *
      * @param y the divisor, a GeneralNumber.
      * @return a GeneralNumber whose value is x / y.
      */
    def /(y: GeneralNumber): GeneralNumber = GeneralNumber(x) doMultiply y.invert //.materialize

    /**
      * Divide x by y (a GeneralNumber) and yield a GeneralNumber.
      * NOTE: the colon is necessary in order to coerce the left hand operand to be a GeneralNumber.
      *
      * @param y the divisor, an Int.
      * @return a GeneralNumber whose value is x / y.
      */
    def :/(y: Int): GeneralNumber = /(GeneralNumber(y))
  }

  /**
    * Method to construct a new GeneralNumber from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * CONSIDER modulate the result so that, in the case of a multiple of Pi, we restrict the range to 0 to 2pi immediately.
    * However, note that this will change the behavior such that it is no longer possible to have the constant 2pi.
    *
    * @param value  the value of the GeneralNumber, expressed as a nested Either type.
    * @param factor the scale factor of the GeneralNumber: valid scales are: Scalar, Pi, and E.
    * @param fuzz   the fuzziness of this GeneralNumber, wrapped in Option.
    * @return a GeneralNumber.
    */
  def create(value: Value, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = (fuzz match {
    case None => ExactNumber(value, factor)
    case _ => FuzzyNumber(value, factor, fuzz)
  }).specialize

  /**
    * Method to construct a new GeneralNumber from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value  the value of the GeneralNumber, expressed as a nested Either type.
    * @param factor the scale factor of the GeneralNumber: valid scales are: Scalar, Pi, and E.
    * @return a GeneralNumber.
    */
  def create(value: Value, factor: Factor): GeneralNumber = create(value, factor, None)

  /**
    * Method to construct a new GeneralNumber from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value      the value of the GeneralNumber, expressed as a nested Either type.
    * @param actualFuzz the fuzziness of this GeneralNumber.
    * @return a GeneralNumber.
    */
  def create(value: Value, actualFuzz: Fuzziness[Double]): GeneralNumber = create(value, Scalar, Some(actualFuzz))

  /**
    * Method to construct a new GeneralNumber from value, factor and fuzz, according to whether there is any fuzziness.
    *
    * @param value the value of the GeneralNumber, expressed as a nested Either type.
    * @return a GeneralNumber.
    */
  def create(value: Value): GeneralNumber = create(value, Scalar)

  /**
    * Method to construct a GeneralNumber from a String.
    * This is by far the best way of creating the number that you really want.
    *
    * @param x the String representation of the value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: String): GeneralNumber = parse(x) match {
    // CONSIDER we should perhaps process n (e.g. to modulate a Pi value)
    case Success(n) => n
    case Failure(e) => throw NumberExceptionWithCause(s"apply(String, Factor): unable to parse $x", e)
  }

  /**
    * Method to construct a GeneralNumber from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Int, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = create(fromInt(x), factor, fuzz)

  /**
    * Method to construct a GeneralNumber from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigInt, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = apply(Rational(x), factor, fuzz)

  /**
    * Method to construct a GeneralNumber from a Rational.
    * NOTE: this method is invoked indirectly by parse(String).
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Rational, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = create(fromRational(x), factor, fuzz)

  /**
    * Method to construct a GeneralNumber from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigDecimal, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = GeneralNumber(Rational(x), factor, fuzz)

  /**
    * Method to construct a GeneralNumber from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on xo.
    */
  def apply(xo: Option[Double], factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = create(fromDouble(xo), factor, fuzz)

  /**
    * Method to construct a GeneralNumber from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Double, factor: Factor, fuzz: Option[Fuzziness[Double]]): GeneralNumber = x match {
    case Double.NaN => GeneralNumber(None, factor, fuzz)
    case _ => GeneralNumber(Some(x), factor, fuzz)
  }

  /**
    * Method to construct a GeneralNumber from an Int.
    *
    * @param x      the Int value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Int, factor: Factor): GeneralNumber = GeneralNumber(x, factor, None)

  /**
    * Method to construct a GeneralNumber from a BigInt.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigInt, factor: Factor): GeneralNumber = GeneralNumber(x, factor, None)

  /**
    * Method to construct a GeneralNumber from a Rational.
    *
    * @param x      the BigInt value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Rational, factor: Factor): GeneralNumber = GeneralNumber(x, factor, None)

  /**
    * Method to construct a GeneralNumber from a BigDecimal.
    *
    * @param x      the BigDecimal value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigDecimal, factor: Factor): GeneralNumber = GeneralNumber(x, factor, None)

  /**
    * Method to construct a GeneralNumber from an optional Double.
    *
    * @param xo     an optional Double.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on xo.
    */
  def apply(xo: Option[Double], factor: Factor): GeneralNumber = GeneralNumber(xo, factor, None)

  /**
    * Method to construct a GeneralNumber from a Double.
    *
    * @param x      the Double value.
    * @param factor the appropriate factor
    * @return a GeneralNumber based on x.
    */
  def apply(x: Double, factor: Factor): GeneralNumber = GeneralNumber(x, factor, None)

  /**
    * Method to construct a unit GeneralNumber with explicit factor.
    *
    * @param factor the appropriate factor
    * @return a unit GeneralNumber with the given factor.
    */
  def apply(factor: Factor): GeneralNumber = GeneralNumber(1, factor)

  /**
    * Method to construct a GeneralNumber from an Int.
    *
    * @param x the Int value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: Int): GeneralNumber = GeneralNumber(x, Scalar)

  /**
    * Method to construct a GeneralNumber from a BigInt.
    *
    * @param x a BigInt value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigInt): GeneralNumber = GeneralNumber(x, Scalar)

  /**
    * Method to construct a GeneralNumber from a Rational.
    *
    * @param x a Rational value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: Rational): GeneralNumber = GeneralNumber(x, Scalar)

  /**
    * Method to construct a GeneralNumber from a BigDecimal.
    *
    * @param x the BigDecimal value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: BigDecimal): GeneralNumber = GeneralNumber(x, Scalar)

  /**
    * Method to construct a GeneralNumber from an optional Double.
    *
    * @param xo an optional Double.
    * @return a GeneralNumber based on xo.
    */
  def apply(xo: Option[Double]): GeneralNumber = GeneralNumber(xo, Scalar)

  /**
    * Method to construct a GeneralNumber from a Double.
    *
    * @param x the Double value.
    * @return a GeneralNumber based on x.
    */
  def apply(x: Double): GeneralNumber = GeneralNumber(x, Scalar)

  /**
    * Method to construct an invalid GeneralNumber.
    *
    * @return a invalid GeneralNumber.
    */
  def apply(): GeneralNumber = NaN

  /**
    * Invalid number.
    */
  val NaN: GeneralNumber = GeneralNumber(None)

  private val numberParser = new NumberParser()

  /**
    * Method to parse a String and yield a Try[GeneralNumber].
    *
    * NOTE: this method indirectly invokes apply(Rational, Factor, Option of Fuzz[Double] )
    *
    * @param w the String to be parsed.
    * @return a GeneralNumber.
    */
  def parse(w: String): Try[GeneralNumber] = {
    val ny: Try[GeneralNumber] = numberParser.parseNumber(w) map (_.specialize)
    ny flatMap (n => if (n.isValid) Success(n) else Failure(NumberException(s"parse: cannot parse $w as a GeneralNumber")))
  }

  /**
    * Following are the definitions required by Ordering[GeneralNumber]
    */
  trait NumberIsOrdering extends Ordering[GeneralNumber] {
    /**
      * When we do a compare on E numbers, they are in the same order as Scalar numbers (i.e. monotonically increasing).
      * It's not necessary to convert exact numbers to fuzzy numbers for this purpose, we simply
      * pretend that the E numbers are Scalar numbers.
      *
      * @param x the first GeneralNumber.
      * @param y the second GeneralNumber.
      * @return an Int representing the order.
      */
    def compare(x: GeneralNumber, y: GeneralNumber): Int =
      if (x.factor == E && y.factor == E)
        compare(x.make(Scalar), y.make(Scalar))
      else
        plus(x, negate(y)).signum
  }

  implicit object NumberIsOrdering extends NumberIsOrdering

  /**
    * Following are the definitions required by Numeric[GeneralNumber]
    */
  trait NumberIsNumeric extends Numeric[GeneralNumber] {
    def plus(x: GeneralNumber, y: GeneralNumber): GeneralNumber = GeneralNumber.plus(x, y)

    def minus(x: GeneralNumber, y: GeneralNumber): GeneralNumber = GeneralNumber.plus(x, negate(y))

    def times(x: GeneralNumber, y: GeneralNumber): GeneralNumber = GeneralNumber.times(x, y)

    def negate(x: GeneralNumber): GeneralNumber = GeneralNumber.negate(x)

    def fromInt(x: Int): GeneralNumber = GeneralNumber(x)

    def parseString(str: String): Option[GeneralNumber] = parse(str).toOption

    def toInt(x: GeneralNumber): Int = toLong(x).toInt

    def toLong(x: GeneralNumber): Long = x.maybeRational match {
      case Some(r) => r.toLong
      case None => x.maybeDouble match {
        case Some(z) => Math.round(z)
        case None => throw NumberException("toLong: this is invalid")
      }
    }

    def toDouble(x: GeneralNumber): Double = x.maybeDouble match {
      case Some(y) => y
      case None => throw NumberException("toDouble: this is invalid")
    }

    def toFloat(x: GeneralNumber): Float = toDouble(x).toFloat
  }

  /**
    * CONSIDER inlining this method or making it private.
    *
    * @param x the first number.
    * @param y the second number.
    * @return the order.
    */
  def doCompare(x: GeneralNumber, y: GeneralNumber): Int = NumberIsOrdering.compare(x, y)

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
  def fuzzyCompare(x: GeneralNumber, y: GeneralNumber, p: Double): Int =
    if (implicitly[Fuzzy[GeneralNumber]].same(p)(x, y)) 0
    else GeneralNumber.plus(x, GeneralNumber.negate(y)).signum(p)

  /**
    * Following are the definitions required by Fractional[GeneralNumber]
    */
  trait NumberIsFractional extends Fractional[GeneralNumber] {
    def div(x: GeneralNumber, y: GeneralNumber): GeneralNumber = GeneralNumber.times(x, inverse(y))
  }

  implicit object NumberIsFractional extends NumberIsFractional with NumberIsNumeric with NumberIsOrdering

  private def plus(x: GeneralNumber, y: GeneralNumber): GeneralNumber = {
    val (a, b) = x.alignFactors(y)
    a.factor match {
      case E => plusAligned(a.scale(Scalar), b.scale(Scalar))
      case _ => plusAligned(a, b)
    }
  }

  private def plusAligned(x: GeneralNumber, y: GeneralNumber): GeneralNumber =
    y match {
      case n@FuzzyNumber(_, _, _) => recover((n plus x).materialize.asNumber, NumberException("logic error: plusAligned"))
      case _ =>
        val (p, q) = x.alignTypes(y)
        prepareWithSpecialize(p.composeDyadic(q, p.factor)(DyadicOperationPlus))
    }

  @tailrec
  private def times(x: GeneralNumber, y: GeneralNumber): GeneralNumber =
    y match {
      case n@FuzzyNumber(_, _, _) => recover((n multiply x).materialize.asNumber, NumberException("logic error: plusAligned"))
      case _ =>
        val (p, q) = x.alignTypes(y)
        p.factor + q.factor match {
          case Some(E) => prepareWithSpecialize(p.composeDyadic(q, E)(DyadicOperationPlus))
          case Some(f) => prepareWithSpecialize(p.composeDyadic(q, f)(DyadicOperationTimes))
          case None => times(x.scale(Scalar), y.scale(Scalar))
        }
    }

  def prepare(no: Option[GeneralNumber]): GeneralNumber = no.getOrElse(GeneralNumber())

  def prepareWithSpecialize(no: Option[GeneralNumber]): GeneralNumber = prepare(no).specialize

  private def sqrt(n: GeneralNumber): GeneralNumber = prepareWithSpecialize(n.scale(Scalar).transformMonadic(Scalar)(MonadicOperationSqrt))

  private def power(x: GeneralNumber, y: GeneralNumber): GeneralNumber =
    y.scale(Scalar).toRational match {
      case Some(r) => power(x, r)
      case None =>
        val zo = for (p <- x.toDouble; q <- y.toDouble) yield GeneralNumber(math.pow(p, q))
        prepareWithSpecialize(zo)
    }

  @tailrec
  private def power(x: GeneralNumber, r: Rational): GeneralNumber =
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
              case None => GeneralNumber(r.toDouble)
            }
          case _ =>
            throw NumberException("rational power cannot be represented as two Ints")
        }
    }

  private def power(n: GeneralNumber, i: Int) = i match {
    case x if x > 0 => LazyList.continually(n).take(x).product
    case x => LazyList.continually(inverse(n)).take(-x).product
  }

  /**
    * Method to take the ith root of n.
    *
    * NOTE that the value 3 (which represents 8 times the double-precision tolerance) is a guess.
    *
    * @param n the GeneralNumber whose root is required.
    * @param i the ordinal of the root (2: square root, etc.).
    * @return the root.
    */
  private def root(n: GeneralNumber, i: Int): Option[GeneralNumber] = i match {
    case 0 => throw NumberException(s"root: logic error: cannot take ${i}th root")
    case 1 => Some(n)
    case 2 => Some(n.makeFuzzyIfAppropriate(GeneralNumber.sqrt, 3))
    case _ => None
  }

  /**
    * Method to deal with a Scale factor change.
    *
    * CONSIDER: re-implementing the Pi/Scalar and Scalar/Pi cases using MonadicOperationScale.
    * TODO: this will work for FuzzyNumber but only if the fuzz is relative, and even then not for E conversions.
    *
    * @param n      the GeneralNumber to be scaled.
    * @param factor the factor to which it should be converted.
    * @return the resulting GeneralNumber (equivalent in value, but with a potentially different scale factor).
    */
  def scale(n: GeneralNumber, factor: Factor): GeneralNumber = (n.factor, factor) match {
    case (a, b) if a == b => n
    case (Pi, Scalar) | (Scalar, Pi) => prepare(n.maybeDouble.map(x => n.make(scaleDouble(x, n.factor, factor), factor)))
    case (E, Scalar) => prepare(n.transformMonadic(factor)(MonadicOperationExp))
    case (Scalar, E) => prepare(n.transformMonadic(factor)(MonadicOperationLog))
    case (Pi, E) => scale(scale(n, Scalar), E)
    case (E, Pi) => scale(scale(n, Scalar), E)
    case _ => throw NumberException("scaling between e and Pi factors is not supported")
  }

  def negate(x: GeneralNumber): GeneralNumber = prepare(x.transformMonadic(x.factor)(MonadicOperationNegate))

  def inverse(x: GeneralNumber): GeneralNumber = prepare(x.transformMonadic(x.factor)(MonadicOperationInvert))

  private def isZero(x: GeneralNumber): Boolean = x.query(QueryOperationIsZero)

  private def isInfinite(x: GeneralNumber): Boolean = x.query(QueryOperationIsInfinite)

  private def signum(x: GeneralNumber): Int = x.doTransformMonadic(x.factor)(identityTry, tryF(x => x.signum), tryF(math.signum)).flatMap(_.toInt).getOrElse(0)

  private def sin(x: GeneralNumber): GeneralNumber = prepareWithSpecialize(x.scale(Pi).transformMonadic(Scalar)(MonadicOperationSin))

  private def atan(x: GeneralNumber, y: GeneralNumber): GeneralNumber = prepareWithSpecialize((y doDivide x).transformMonadic(Pi)(MonadicOperationAtan(x.signum))).modulate

  private def log(x: GeneralNumber): GeneralNumber = x.scale(E).make(Scalar)

  private def exp(x: GeneralNumber): GeneralNumber = x.scale(Scalar).make(E)

  private def scaleDouble(x: Double, fThis: Factor, fResult: Factor) = x * fThis.value / fResult.value

  /**
    * This method returns a GeneralNumber equivalent to x but with the value in an explicit factor-dependent range.
    * Only Pi is currently fixed within a range (0 -> 2).
    *
    * @param x the GeneralNumber to operate on.
    * @return either x or a number equivalent to x with value in defined range.
    */
  private def modulate(x: GeneralNumber): GeneralNumber = x.factor match {
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

  private def asPowerOfE(v: Value): String = v match {
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

