/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.FuzzyNumber.Ellipsis
import com.phasmidsoftware.number.core.inner.Rational.{MAX_PRIME_FACTORS, NaN, bigNegOne, bigOne, bigZero, half, minus, one, rootOfBigInt, times, toInts}
import com.phasmidsoftware.number.core.{BigNumber, Number, NumberLike, Prime}
import com.phasmidsoftware.number.misc.FP.{toTry, toTryWithRationalException}
import com.phasmidsoftware.number.misc.{ContinuedFraction, FP}
import com.phasmidsoftware.number.parse.RationalParser
import java.lang.Math._
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
 * `Rational`: a case class that represents rational numbers by a `BigInt` numerator and a `BigInt` denominator.
 * The numerator (`n`) and the denominator (`d`) may not share a common factor:
 * if you try to construct a `Rational` with `new` where there is a common factor,
 * then an exception will be thrown.
 * However, all the `apply` methods ensure valid `Rational` instances by factoring out any common factors.
 * Similarly, the denominator may not be negative: again, the apply methods will take care of this situation.
 * This is the reason that the constructor of `Rational` is marked package-private.
 *
 * The domain of `Rational` includes values with a zero value in the denominator and any numerator (either -ve or +ve infinity)
 * as well as the value with 0 as both the numerator and denominator (`NaN`).
 *
 * @author scalaprof
 */
case class Rational private[inner](n: BigInt, d: BigInt) extends NumberLike {

  // Pre-conditions

  // NOTE: ensure that the denominator is non-zero, or the numerator is zero.
  require(d.signum >= 0 || (n.signum == 0 && d == bigNegOne), s"Rational denominator is negative: $d")

  // NOTE: ensure that the numerator and denominator are relatively prime.
  require(n == bigZero && d == bigZero || n.gcd(d) == 1, s"Rational($n,$d): arguments have common factor: ${n.gcd(d)}")

  /**
    * The absolute value of `this Rational`.
    * If the sign of `this` is negative, the result will be the negation of `this`.
    * Otherwise, it will return the `Rational` itself.
    */
  lazy val abs: Rational =
    if (signum < 0) negate else this
  /**
    * The square root of `this` `Rational`.
    *
    * This is achieved by raising `this` to the power of 1/2.
    *
    * @return `Success` containing the square root if it can be calculated,
    *         otherwise `Failure` containing an explanation of why the calculation failed.
    */
  lazy val sqrt: Try[Rational] =
    power(half)
  /**
    * The value of `n.signum`.
    * NOTE that `d` is non-negative for all values in the domain of `Rational` except for negative zero.
    */
  lazy val signum: Int =
    n.signum

  lazy val halve: Rational = this * Rational.half

  /**
    * Adds a BigInt value to this Rational number.
    *
    * @param that The BigInt value to be added to this Rational.
    * @return A new Rational representing the sum of this Rational and the given BigInt.
    */
  def +(that: BigInt): Rational =
    this + Rational(that)

  /**
    * Adds this Rational number to another Rational number.
    *
    * @param that The Rational number to add to this Rational number.
    * @return A new Rational number representing the sum of the two Rational numbers.
    */// Operators
  def +(that: Rational): Rational =
    Rational.plus(this, that)

  /**
    * Negates the current Rational number.
    *
    * @return A new Rational instance representing the negation of this Rational number.
    */// NOTE this IS used, whatever the compiler thinks.
  def unary_- : Rational = negate

  /**
    * Adds a Long value to this Rational number and returns the resulting Rational.
    *
    * @param that the Long value to be added to this Rational number
    * @return a new Rational representing the sum of this Rational and the given Long value
    */
  def +(that: Long): Rational =
    this + Rational(that)

  /**
    * Subtracts a `BigInt` value from this `Rational` number and returns the resulting `Rational`.
    *
    * @param that the `BigInt` value to subtract from this `Rational`
    * @return the result of the subtraction as a `Rational`
    */
  def -(that: BigInt): Rational =
    this - Rational(that)

  /**
    * Subtracts the given Rational number from this Rational number and returns the result.
    *
    * @param that the Rational number to be subtracted from this Rational number
    * @return a new Rational representing the result of the subtraction
    */
  def -(that: Rational): Rational =
    minus(this, that)

  /**
    * Multiplies this Rational number by the given BigInt and returns the result as a new Rational.
    *
    * @param that the BigInt to multiply this Rational by
    * @return a new Rational resulting from the multiplication
    */
  def *(that: BigInt): Rational =
    this * Rational(that)

  /**
    * Multiplies this `Rational` instance by the provided `Short` value, producing a new `Rational` instance.
    *
    * @param that the `Short` value to multiply with this `Rational`
    * @return a new `Rational` instance representing the product of this `Rational` and the provided `Short`
    */
  def *(that: Short): Rational =
    this * Rational(that)

  /**
    * Divides the current `Rational` instance by the given `Long` value and
    * returns a new `Rational` instance representing the result.
    *
    * @param that the `Long` value to divide the current `Rational` instance by
    * @return a new `Rational` instance resulting from the division
    */
  def /(that: Long): Rational =
    this / Rational(that)

  /**
    * Divides this Rational number by another Rational number.
    *
    * @param that the Rational number to divide by
    * @return a new Rational number representing the result of the division
    */
  def /(that: Rational): Rational =
    this * that.invert

  /**
    * Multiplies this Rational number by another Rational number.
    *
    * @param that the Rational number to multiply with this Rational number
    * @return a new Rational number representing the product of this Rational number and the given Rational number
    */
  def *(that: Rational): Rational =
    times(this, that)

  /**
    * Inverts the numerator and the denominator of a Rational number.
    *
    * @return A new Rational instance with the numerator and denominator swapped.
    */
  def invert: Rational =
    Rational(d, n)

  /**
    * Raises this Rational number to the power of the given exponent.
    * WARNING: this is deprecated in favor of `^`(that: Int) because the precedence for this operator
    * is lower than for multiplication, addition, etc.
    *
    * @param that the exponent to which this Rational number will be raised
    * @return a new Rational representing the result of raising this Rational to the given exponent
    */
  @deprecated("use ∧ instead", "1.2.1")
  def ^(that: Int): Rational =
    power(that)

  /**
    * Raises this Rational number to the power of the given exponent.
    * NOTE: this is not the caret symbol (that operator is deprecated).
    *
    * @param that the exponent to which this Rational number will be raised
    * @return a new Rational representing the result of raising this Rational to the given exponent
    */
  def ∧(that: Int): Rational =
    power(that)

  /**
    * Method to calculate the value of this Rational raised to the power of `p`.
    *
    * @param p an Int which may be negative, zero, or positive.
    * @return this raised to the power of `p`.
    */
  def power(p: Int): Rational = {
    @tailrec def inner(r: Rational, x: Int): Rational =
      if (x == 0) r else inner(r * this, x - 1)

    if (p == 0) one
    else if (p == 1 || isUnity) this
    else {
      val rational = inner(one, math.abs(p))
      if (p > 0) rational
      else rational.invert
    }
  }

  /**
    * Checks if the number is equal to one and is a whole number.
    *
    * @return true if the number is exactly one and is a whole number, false otherwise
    */
  def isUnity: Boolean =
    n == bigOne && isWhole

  // Other methods appropriate to Rational

  /**
    * Raises this Rational number to the power of the given Rational number.
    *
    * @param that the Rational number representing the exponent
    * @return a Try containing the resulting Rational number if the operation is successful,
    *         or a Failure if an error occurs during the calculation
    */
  def ^(that: Rational): Try[Rational] =
    power(that)

  /**
   * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
   *
   * @return an optional `Factor`.
   */
  def maybeFactor: Option[Factor] =
    Some(PureNumber)

  /**
    * Determines if the value is negative.
    *
    * @return true if the value is negative, false otherwise.
    */
  def isNegative: Boolean =
    signum < 0

  /**
    * Determines if the number is zero and is a whole number.
    *
    * @return true if the number is zero and is a whole number, otherwise false
    */
  def isZero: Boolean =
    n == bigZero && isWhole

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    !(isInfinity || isNaN)

  /**
   * Determines if the Rational object represents "not a number" (NaN).
   *
   * @return true if the Rational is both zero and infinite, indicating an undefined result; false otherwise.
   */
  def isNaN: Boolean =
    n == bigZero && d == bigZero

  /**
    * Returns an optional integer representation of this `Rational`.
    * If the `Rational` is not a whole number or is too large for `Int`, `None` is returned.
    *
    * @return an `Option[Int]`.
    */
  def maybeInt: Option[Int] =
    if (isWhole) Rational.toInt(this).toOption else None

  /**
    * Determines if the number is a whole number.
    *
    * @return true if the number is equal to the mathematical representation of one; false otherwise.
    */
  def isWhole: Boolean =
    d == bigOne

  /**
    * Method to convert `this` `Rational` into an `Int`.
    * It is better to use `maybeInt`.
    *
    * NOTE this will throw an exception if `this` `Rational` is not whole or its numerator is too large for an `Int`.
    *
    * @return an `Int`.
    */
  def toInt: Int =
    Rational.toInt(this).get

  /**
   * Method to determine if this Rational can be represented exactly as a decimal string.
   *
   * NOTE: re: (fixed) Issue #85
   * It's inappropriate to look for prime factors of very large numbers just to determine
   * if we can represent this Rational as a decimal.
   *
   * Originally, the code here was as follows:
   * denominatorPrimeFactors.map(_.toBigInt).sorted.distinct.filterNot(x => x == bigTwo).forall(x => x == bigFive)
   *
   * @return true if the prime factors only include 2 and/or 5.
   */
  def isDecimal: Boolean =
    isWhole || {
      import com.phasmidsoftware.number.core.Divides.IntDivides
      (2 |> d) || (5 |> d)
    }

  /**
   * Method to convert `this` `Rational` into a `Long`.
   * It is better to use `Rational.toLong`.
   *
   * NOTE this will throw an exception if `this` `Rational` is not whole or its numerator is too large for a `Long`.
   *
   * @return a `Long`.
   */
  def toLong: Long =
    Rational.toLong(this).get

  /**
   * Method to convert `this` `Rational` into a `BigInt`.
   * It is better to use `Rational.toBigInt`.
   *
   * NOTE this will throw an exception if `this` `Rational` is not whole.
   *
   * @return a `BigInt`.
   */
  def toBigInt: BigInt =
    Rational.toBigInt(this).get

  /**
   * Converts this `Rational` instance to its `Float` representation.
   *
   * @return a `Float` value representing this `Rational`.
   */
  def toFloat: Float =
    Rational.toFloat(this)

  /**
   * Returns an optional `Double` representation of this `Rational`.
   * The method checks if the `Rational` can be exactly represented as a `Double`. If so, it converts the value to `Double` and returns it wrapped in `Some`.
   * Otherwise, it returns `None`.
   *
   * @return an `Option[Double]`, `Some(Double)` if the `Rational` can be exactly represented as a `Double`, otherwise `None`.
   */
  def maybeDouble: Option[Double] =
    if (isExactDouble)
      Some(toDouble)
    else
      None

  /**
    * Converts this `Rational` instance into a `Double` representation.
    *
    * @return the `Double` value of this `Rational`.
    */
  def toDouble: Double =
    Rational.toDouble(this)

  /**
    * Determines if this `Rational` can be exactly represented as a `Double`.
    *
    * @return `true` if the `Rational` can be precisely converted to a `Double`, otherwise `false`.
    */
  def isExactDouble: Boolean =
    toBigDecimal.exists(_.isExactDouble) // Only work with Scala 2.11 or above

  /**
    * Converts this `Rational` to an `Option[BigDecimal]` if it can be exactly represented as a decimal.
    * If the `Rational` can be precisely represented as a `BigDecimal`, it is returned wrapped in `Some`.
    * Otherwise, `None` is returned.
    *
    * @return an `Option[BigDecimal]`, where `Some(BigDecimal)` represents the exact conversion,
    *         or `None` if the `Rational` cannot be exactly represented.
    */
  def toBigDecimal: Option[BigDecimal] =
    if (isDecimal)
      Some(forceToBigDecimal)
    else
      None

  /**
   * Method to get the xth power of this `Rational`, exactly.
   *
   * @param x the power (any `Rational`) whose numerator and denominator can each be represented by an `Int`.
   * @return `Success(...)` if the result can be calculated exactly, else `Failure`.
   */
  def power(x: Rational): Try[Rational] = for {
    p <- toTryWithRationalException(Rational.toInt(x.n), s"power($x): numerator is not an Int")
    r <- toTryWithRationalException(Rational.toInt(x.d), s"power($x): denominator is not an Int")
    z <- toTryWithRationalException(power(p).root(r), s"power($x): cannot calculate result exactly")
  } yield z

  /**
    * Converts the Rational number to a BigDecimal representation.
    * If the Rational number represents infinity, an exception is thrown.
    *
    * @return A BigDecimal representing the Rational number.
    * @throws com.phasmidsoftware.number.core.inner.RationalException if the Rational number is infinity.
    */
  def forceToBigDecimal: BigDecimal =
    if (!isInfinity)
      BigDecimal(n) / BigDecimal(d)
    else
      throw RationalException(s"cannot convert infinity to BigDecimal: $this")

  /**
    * Method to determine if the given Rational represents an infinity.
    *
    * @return true if the denominator is zero; false otherwise.
    */
  def isInfinity: Boolean =
    d == bigZero && n != bigZero

  /**
   * Method to get the xth root of this Rational.
   * Note that it is not guaranteed to result in an exact value.
   *
   * @param x the root to be taken, for example, for the cube root, we set x = 3.
   * @param epsilon if defined, this represents an acceptable error in the calculation (defaults to None).
   * @return an (optional) Rational result which is the exact root.
   *         In the event that it's not possible to get the exact root, then None is returned.
   */
  def root(x: Int, epsilon: Option[Double] = None): Option[Rational] =
    if (x == 1 || isUnity)
      Some(this)
    else if (x <= 0)
      None
    else
      (n, d, x, epsilon) match {
        case (_, `bigOne`, 2, Some(e)) =>
          Rational.squareRoot(n, e)
        case _ =>
          for (a <- rootOfBigInt(n, x); b <- rootOfBigInt(d, x)) yield Rational(a, b)
    }

  /**
   * Compares this `Rational` instance with another `Rational` instance and determines their relative order.
   *
   * @param other the `Rational` instance to compare with this instance
   * @return an `Int` value:
   *         - Negative if this instance is less than the other instance,
   *         - Zero if they are equal,
   *         - Positive if this instance is greater than the other instance
   */
  def compare(other: Rational): Int =
    Rational.compare(this, other)

  /**
   * Renders the current object into its string representation.
   * The method conditionally determines the output based on the
   * result of an internal conditional computation.
   *
   * @return A string representation of the object. If a certain
   *         condition evaluates to true, a specific value is
   *         returned; otherwise, the object's `toString` is used.
   */
  def render: String = {
    val x -> y = renderConditional(true)
    if (y) x else toString
  }

  /**
    * Represents the fractional string format of the object as "numerator/denominator".
    * Converts the values of `n` (numerator) and `d` (denominator) to a string.
    * The result is a lazily evaluated string construction.
    */
  lazy val toRationalString = s"$n/$d"

  /**
   * Applies a sign to this `Rational` based on the given boolean value.
   * If the value is `true`, it negates the current `Rational`, otherwise, it returns the original instance.
   *
   * @param negative a boolean indicating whether the `Rational` should be negated (`true`) or left as is (`false`)
   * @return a `Rational` instance with the applied sign
   */
  def applySign(negative: Boolean): Rational =
    if (negative)
      negate
    else
      this

  /**
    * Computes the negation of this Rational number.
    *
    * @return A new Rational instance representing the additive inverse of this Rational.
    */
  def negate: Rational =
    Rational.negate(this)

  /**
   * Applies an integer exponent to this `Rational` instance.
   *
   * @param exponent the exponent to which this `Rational` will be raised. It can be positive, negative, or zero.
   * @return a new `Rational` that is the result of raising this `Rational` to the given `exponent`.
   */
  def applyExponent(exponent: Int): Rational =
    this * Rational.exponent(exponent)

  /**
   * Method to determine the Mediant of two rational numbers.
   * See Wikipedia: Mediant (Mathematics).
   * The result will always be intermediate in value to this and other.
   * The Mediant plays a role in the Farey Sequence (see Wikipedia) and thus can be used to approximate
   * an irrational number as a rational number.
   *
   * @param other the other number.
   * @return the mediant of this and other.
   */
  def mediant(other: Rational): Rational =
    if (signum >= 0 && other.signum >= 0 && !isInfinity && !other.isInfinity)
      Rational(n + other.n, d + other.d)
    else
      NaN

  /**
    * Converts the Rational object to its string representation.
    * Rational numbers are represented as "numerator/denominator" or "numerator" if the denominator is 1.
    *
    * @return A string representation of the Rational object.
    */
  override def toString: String = this match {
    case r: Rational if r == half =>
      "\u00BD"
    case Rational(n, `bigZero`) if n > 0 =>
      "∞"
    case Rational(top, Rational.bigOne) =>
      s"$top"
    case Rational(top, bottom) =>
      s"$top/$bottom"
  }

  /**
   * Renders a string representation of the object either exactly or approximately,
   * depending on the constraints of the provided parameters.
   *
   * @param maxLength   The maximum length of the resulting string representation.
   * @param maybePlaces An optional parameter specifying the number of decimal places to use for rendering.
   *                    If not provided, a default value is inferred based on the constraints.
   * @return A string representation of the object padded to the specified maximum length.
   *         Throws an exception if the representation cannot fit within the constraints.
   */
  def renderApproximate(maxLength: Int, maybePlaces: Option[Int] = None): String = {
    val w = renderExact
    if (w.length <= maxLength && maybePlaces.isEmpty) w.padTo(maxLength, ' ') else {
      val x = toDouble
      val places = maybePlaces getOrElse maxLength - x.toInt.toString.length - 1
      if (places < 0) throw RationalException(s"renderApproximate: $this cannot be rendered in $maxLength characters")
      val result = String.format(s"%$maxLength.${places}f", roundDouble(x, places))
      if (result.length > maxLength) throw RationalException(s"renderApproximate: $this cannot be rendered in $maxLength characters with $places places")
      if (result.length < maxLength) " " * (maxLength - result.length) + result else result
    }
  }

  /**
   * Rounds a given double value to the specified number of decimal places.
   *
   * @param x      The double value to be rounded.
   * @param places The number of decimal places to round to.
   * @return The rounded double value.
   */
  private def roundDouble(x: Double, places: Int) = {
    val factor: Double = math.pow(10, places)
    math.round(x * factor) / factor
  }

  /**
   * Renders a conditional output based on the provided boolean flag.
   * CONSIDER: why not return an Option[String] ?
    * NOTE the parameter exact is `always` true.
   *
   * @param exact A boolean flag indicating whether to render the exact output or a default value.
   * @return A tuple containing the rendered string and a boolean indicating the success of rendering.
   */
  def renderConditional(exact: Boolean): (String, Boolean) =
    if (exact)
      renderExact -> true
    else
      "" -> false // NOTE string is ignored if boolean is false

  /**
   * Method to render a Rational in a manner other than as a rational number (for that, use toString).
   *
   * @return an exact String representation of this Rational.
   */
  def renderExact: String = this match {
    case VulgarFraction(w) =>
      w
    case `half` =>
      "\u00BD"
    case _ if isNaN =>
      "NaN"
    case _ if isZero && d < 0 =>
      "-0"
    case _ if isInfinity =>
      (if (n > 0) "" else "-") + "∞"
    case _ if isWhole =>
      toBigInt.toString
    case _ if isExactDouble =>
      toDouble.toString
    case _ =>
      findRepeatingSequence match {
        case Success(w) =>
          w
        case _ =>
          toBigDecimal match {
            case Some(x) =>
              x.toString
            case None =>
              asString
      }
    }
  }

  /**
   * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
   *
   * CONSIDER whether this method should really be defined in NumberLike since it makes no sense here.
   *
   * @param context      the (optional) context in which we want to evaluate this Expression.
   *                    if factor is None then, the result will depend solely on whether this is exact.
   * @return true if this NumberLike object is exact in the context of factor, else false.
   */
  def isExactInContext(context: Context): Boolean = true

  /**
   * Method to determine if this Field is actually a real Number (i.e. not complex).
   * NOTE: to force this as a Number, use convertToNumber in the companion Object.
   *
   * @return a Some(x) if this is a Number; otherwise return None.
   */
  def asNumber: Option[Number] = Some(this)

  /**
   * Converts a number to a percentage representation with a specified number of decimal places.
   *
   * @param places The number of decimal places to include in the percentage representation.
   * @return A string representation of the number as a percentage with the specified number of decimal places.
   */
  def renderAsPercent(places: Int): String =
    (this * 100).renderApproximate(4 + places, Some(places)) + "%"

  /**
    * Multiplies this Rational number by a given Long value and returns the result as a new Rational number.
    *
    * @param that the Long value to multiply with this Rational number
    * @return a new Rational number representing the product of this Rational number and the given Long value
    */
  def *(that: Long): Rational =
    this * Rational(that)

  /**
   * Finds and returns the repeating sequence in the decimal representation of a fraction.
   * The method uses the numerator, denominator, and the prime factorization of the denominator
   * to compute the repeating sequence.
   *
   * @return a Try containing the repeating sequence as a String if successful, or a Failure if an error occurs
   */
  def findRepeatingSequence: Try[String] =
    Rational.findRepeatingSequence(n, d, denominatorPrimeFactors(MAX_PRIME_FACTORS))

  /**
   * Computes the prime factors of the denominator of a given value `d`.
   *
   * This method uses the `Prime.primeFactors` utility to determine the
   * prime factors of the denominator.
   *
   * @return A sequence of prime factors of the denominator.
   */
  private def denominatorPrimeFactors(max: Int) =
    Prime.primeFactors(d, Some(max * 3)).take(max)

  /**
   * Converts the current value to its string representation.
   *
   * The method provides a string representation depending on the value of `d`.
   * If `d` is less than or equal to 100,000, it uses a rational string representation.
   * For larger values, it converts the value to a BigDecimal and appends an ellipsis to indicate that the value cannot be exactly represented.
   *
   * @return A string representation of the value, either as a rational string or a BigDecimal string with an ellipsis.
   */
  private def asString: String = d match {
    case x if x <= 100000L => // XXX arbitrary limit of one hundred thousand.
      toRationalString
    case _ =>
      // NOTE this case represents a Rational that cannot easily be rendered in decimal form.
      // It is not fuzzy.
      forceToBigDecimal.toString + Ellipsis
  }
}

/**
 * Companion object to `Rational`.
 * Represents a Rational number, with various methods for conversion, approximation, and utility.
 * Provides a rich set of operations and utilities to handle rational numbers, convert data types,
 * and manage normalization. Rational numbers are represented as fractions with BigInt numerator
 * and denominator.
 *
 * Fields:
 * - bigZero: Commonly used representation for Rational zero.
 * - bigOne: Commonly used representation for Rational one.
 * - bigTwo: Commonly used representation for Rational two.
 * - bigThree: Commonly used representation for Rational three.
 * - bigFour: Commonly used representation for Rational four.
 * - bigFive: Commonly used representation for Rational five.
 * - bigSeven: Commonly used representation for Rational seven.
 * - bigNegOne: Commonly used representation for Rational negative one.
 * - bigTen: Commonly used representation for Rational ten.
 * - zero: Rational representation for zero.
 * - infinity: Positive infinity in Rational form.
 * - negInfinity: Negative infinity in Rational form.
 * - one: Rational representation for one.
 * - ten: Rational representation for ten.
 * - two: Rational representation for two.
 * - half: Rational representation for one-half.
 * - NaN: Represents "Not a Number" in Rational form.
 * - negZero: Represents negative zero in Rational form.
 * - negZeroDouble: Represents negative zero as a Double in Rational form.
 * - pi_5000: Predefined Rational representation of π/5000.
 * - squareRoots: A collection of predefined square root values in Rational form.
 */
object Rational {

  implicit def convertToNumber(x: Rational): Number =
    Number(x)

  /**
   * Implicit class RationalOps to allow an Int to be treated as a Rational for the purpose
   * of allowing operations with right-hand Rational arguments or for division by an Int.
   * A particular case is rational division where we must convert the numerator to Rational so that
   * we don't end up with 0 as the result of an Int division.
   *
   * @param x an Int
   */
  implicit class RationalOps(x: Int) {

    /**
     * Method to increment x by r.
     *
     * @param r the addend.
     * @return the result of x + r.
     */
    def +(r: Rational): Rational =
      Rational(x) + r

    /**
     * Method to multiply x by r.
     *
     * @param r the multiplicand.
     * @return the result of x * r.
     */
    def *(r: Rational): Rational =
      Rational(x) * r

    /**
      * Computes the multiplicative inverse of the Rational representation of the given integer.
      * The result is equivalent to 1 divided by the Rational value of the integer.
      *
      * @return the inverted Rational representation of the integer.
      */
    def invert: Rational =
      Rational(x).invert

    /**
     * Method to divide by Rational.
     * We cannot just use "/" as that will bind to Int and result in 0.
     * Therefore, we use the special ":/" operator which will work as long as RationalOps is imported.
     *
     * @param denominator the denominator of the resulting Rational. Any Int will be implicitly converted to a Long.
     * @return the Rational whose value is x/y
     */
    def :/(denominator: Long): Rational =
      Rational(x, denominator)
  }

  /**
   * Implicit class RationalHelper to allow definition of Rationals by Strings of the form r"n/d".
   *
   * @param sc a StringContext.
   */
  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): Rational = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next()
        if (s.isEmpty) {
          if (expressions.hasNext)
            sb.append(expressions.next())
          else
            throw RationalException("r: logic error: missing expression")
        }
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw RationalException(s"r: ignored: ${expressions.next()}")
      else
        Rational(sb.toString)
    }
  }

  /**
   * Method to approximate an irrational number as a Rational.
   * Application code should always call this method or, preferably, Rational.apply(Double).
   *
   * NOTE: this method is designed for true Doubles, not floating-point representations of decimal numbers.
   * Such decimal numbers should be converted to BigDecimal first using BigDecimal.valueOf(x).
   *
   * CONSIDER what we need here is to pass in a function and a desired Rational result,
   * and perhaps even the first derivative of the function, so that we can approximate the solution (root) of the function.
   *
   * @param x       the value to approximate.
   * @param epsilon (implicit) the tolerance.
   * @return a Rational such that the difference between the result and x is less than epsilon.
   */
  def approximateAny(x: Double)(implicit epsilon: Tolerance): Rational =
    if (Double.box(x).isNaN)
      NaN
    else if (x.isPosInfinity)
      infinity
    else if (x.isNegInfinity)
      infinity.negate
    else if (x > 0)
      approximatePositive(x)
    else
      approximatePositive(-x).negate

  /**
   * Method to take a Double in the range 0 thru 1 and approximate it by a Rational number
   * to within the given tolerance (epsilon).
   *
   * NOTE: this Farey sequence converges very slowly. In order to put an absolute limit on the recursion
   * (which otherwise might never terminate), we limit the size of the denominator of r1 to be 10E12.
   * It shouldn't matter much whether we test r1 or r2 in this context.
   *
   * @param x       the value to approximate (should be between 0 and 1).
   * @param epsilon (implicit) the tolerance.
   * @return a Rational such that the difference between the result and x is less than epsilon.
   * @throws java.lang.IllegalArgumentException if x is not between 0 and 1.
   */
  def approximate(x: Double)(implicit epsilon: Tolerance): Rational = {
    require(x >= 0 && x <= 1, "Call convertDouble instead of approximate")

    @scala.annotation.tailrec
    def inner(r1: Rational, r2: Rational): Rational =
      if (r1.d > epsilon.maxDenom || math.abs(r1.toDouble - x) < epsilon.x)
        r1
      else {
        val mediant: Rational = r1 mediant r2
        if (mediant.toDouble > x) inner(r1, mediant)
        else inner(mediant, r2)
      }

    inner(zero, one)
  }

  /**
    * A predefined instance of the `Rational` class representing the value zero.
    * This constant can be used as a default or as a comparison operand
    * in operations involving rational numbers.
    */
  val zero: Rational = Rational(0)
  /**
    * A constant value representing the rational number equivalent to 1.
    * It is defined as an instance of the `Rational` class initialized with
    * the value of `bigOne`.
    */
  val one: Rational = Rational(bigOne)
  /**
    * Represents the negation of the `Rational` value `one`.
    * The result is a `Rational` object equivalent to -1.
    */
  val negOne: Rational = one.negate
  /**
    * Represents a rational number with an infinite value, defined as the
    * inversion of the `zero` rational number.
    */
  val infinity: Rational = zero.invert
  /**
    * A Rational number representing the reciprocal of `two`.
    * The `invert` method on the `two` object is used to calculate this value.
    */
  val half: Rational = two.invert

  /**
    * Implicit converter from Double to Rational.
    *
    * @param x the value.
    * @return a Rational equal to or approximately equal to x.
    * @throws java.util.NoSuchElementException because we invoke get on an Option[Rational].
    */
  implicit def convertDouble(x: Double): Rational =
    createExact(x).get // NOTE using get

  /**
    * Implicit converter from Long to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def convertLong(x: Long): Rational =
    Rational(x)
  /**
    * Represents the rational number 10 as an instance of the `Rational` type.
    * Utilizes the predefined value `bigTen` to create the `Rational` instance.
    */
  val ten: Rational = Rational(bigTen)
  /**
    * Represents the rational number two as a constant of type Rational.
    * It is constructed using `Rational(bigTwo)`, where `bigTwo` presumably
    * signifies the numerical value 2 in a predefined or imported context.
    */
  lazy val two: Rational = Rational(bigTwo)
  /**
    * Represents a Not-a-Number (NaN) value as a rational number.
    * It is created by initializing a `Rational` object with both numerator
    * and denominator as zero. This is a special case and does not represent
    * a valid rational number or a real number.
    */
  val NaN = new Rational(0, 0)

  /**
    * A constant value representing zero as a BigInt.
    */
  private[inner] lazy val bigZero: BigInt = BigInt(0)
  /**
    * A constant value representing the big integer 1.
    */
  lazy val bigOne: BigInt = BigInt(1)
  /**
    * A constant value representing the number 2 as a BigInt instance.
    */
  private[inner] lazy val bigTwo: BigInt = BigInt(2)

  /**
    * Implicit converter from BigInt to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def convertBigInt(x: BigInt): Rational =
    Rational(x)
  /**
    * Represents the number four as a `BigInt` type.
    *
    * This value is immutable and can be used where a `BigInt` representation
    * of the integer 4 is required.
    */
  private[core] val bigFour: BigInt = BigInt(4)
  /**
    * A constant value representing the number five as a `BigInt`.
    */
  private[core] val bigFive: BigInt = BigInt(5)
  /**
    * Represents the integer value 6 as a `BigInt` constant.
    */
  private[core] val bigSix: BigInt = BigInt(6)
  /**
    * A constant value representing the number seven as a BigInt.
    */
  private[core] val bigSeven: BigInt = BigInt(7)
  /**
    * A constant value representing the integer -1 as a BigInt.
    * BigInt is used to hold arbitrarily large or small integer values.
    */
  private[core] lazy val bigNegOne: BigInt = BigInt(-1)
  /**
    * Represents the BigInt value of 10.
    * This can be used in calculations or as a constant reference for
    * operations requiring the integer value 10 in the BigInt context.
    */
  private[inner] lazy val bigTen: BigInt = BigInt(10)

  /**
   * Represents a Rational number with a numerator of `0` and a denominator of `-1`.
   * Although mathematically this simplifies to zero, the negative denominator is retained in this instance.
   * This is the only instance in the domain of Rational with a negative denominator.
   */
  private[core] lazy val negZero = new Rational(0, -1)
  private[core] val negInfinity: Rational = new Rational(-1, 0)
  private[inner] val negZeroDouble: Double = "-0.0".toDouble // negative zero as a Double
  private[inner] lazy val pi_5000: Rational = Rational(sPi_5000)
  private[inner] lazy val sPi_5000: String =
    """3.14159265358979323846264338327950288419716939937510582097494459230781640628
      |6208998628034825342117067982148086513282306647093844609550582231725359408128
      |4811174502841027019385211055596446229489549303819644288109756659334461284756
      |4823378678316527120190914564856692346034861045432664821339360726024914127372
      |4587006606315588174881520920962829254091715364367892590360011330530548820466
      |5213841469519415116094330572703657595919530921861173819326117931051185480744
      |6237996274956735188575272489122793818301194912983367336244065664308602139494
      |6395224737190702179860943702770539217176293176752384674818467669405132000568
      |1271452635608277857713427577896091736371787214684409012249534301465495853710
      |5079227968925892354201995611212902196086403441815981362977477130996051870721
      |1349999998372978049951059731732816096318595024459455346908302642522308253344
      |6850352619311881710100031378387528865875332083814206171776691473035982534904
      |2875546873115956286388235378759375195778185778053217122680661300192787661119
      |5909216420198938095257201065485863278865936153381827968230301952035301852968
      |9957736225994138912497217752834791315155748572424541506959508295331168617278
      |5588907509838175463746493931925506040092770167113900984882401285836160356370
      |7660104710181942955596198946767837449448255379774726847104047534646208046684
      |2590694912933136770289891521047521620569660240580381501935112533824300355876
      |4024749647326391419927260426992279678235478163600934172164121992458631503028
      |6182974555706749838505494588586926995690927210797509302955321165344987202755
      |9602364806654991198818347977535663698074265425278625518184175746728909777727
      |9380008164706001614524919217321721477235014144197356854816136115735255213347
      |5741849468438523323907394143334547762416862518983569485562099219222184272550
      |2542568876717904946016534668049886272327917860857843838279679766814541009538
      |8378636095068006422512520511739298489608412848862694560424196528502221066118
      |6306744278622039194945047123713786960956364371917287467764657573962413890865
      |8326459958133904780275900994657640789512694683983525957098258226205224894077
      |2671947826848260147699090264013639443745530506820349625245174939965143142980
      |9190659250937221696461515709858387410597885959772975498930161753928468138268
      |6838689427741559918559252459539594310499725246808459872736446958486538367362
      |2262609912460805124388439045124413654976278079771569143599770012961608944169
      |4868555848406353422072225828488648158456028506016842739452267467678895252138
      |5225499546667278239864565961163548862305774564980355936345681743241125150760
      |6947945109659609402522887971089314566913686722874894056010150330861792868092
      |0874760917824938589009714909675985261365549781893129784821682998948722658804
      |8575640142704775551323796414515237462343645428584447952658678210511413547357
      |3952311342716610213596953623144295248493718711014576540359027993440374200731
      |0578539062198387447808478489683321445713868751943506430218453191048481005370
      |6146806749192781911979399520614196634287544406437451237181921799983910159195
      |6181467514269123974894090718649423196156794520809514655022523160388193014209
      |3762137855956638937787083039069792077346722182562599661501421503068038447734
      |5492026054146659252014974428507325186660021324340881907104863317346496514539
      |0579626856100550810665879699816357473638405257145910289706414011097120628043
      |9039759515677157700420337869936007230558763176359421873125147120532928191826
      |1861258673215791984148488291644706095752706957220917567116722910981690915280
      |1735067127485832228718352093539657251210835791513698820914442100675103346711
      |0314126711136990865851639831501970165151168517143765761835155650884909989859
      |9823873455283316355076479185358932261854896321329330898570642046752590709154
      |8141654985946163718027098199430992448895757128289059232332609729971208443357
      |3265489382391193259746366730583604142813883032038249037589852437441702913276
      |5618093773444030707469211201913020330380197621101100449293215160842444859637
      |6698389522868478312355265821314495768572624334418930396864262434107732269780
      |2807318915441101044682325271620105265227211166039666557309254711055785376346
      |6820653109896526918620564769312570586356620185581007293606598764861179104533
      |4885034611365768675324944166803962657978771855608455296541266540853061434443
      |1858676975145661406800700237877659134401712749470420562230538994561314071127
      |0004078547332699390814546646458807972708266830634328587856983052358089330657
      |5740679545716377525420211495576158140025012622859413021647155097925923099079
      |6547376125517656751357517829666454779174501129961489030463994713296210734043
      |7518957359614589019389713111790429782856475032031986915140287080859904801094
      |1214722131794764777262241425485454033215718530614228813758504306332175182979
      |8662237172159160771669254748738986654949450114654062843366393790039769265672
      |1463853067360965712091807638327166416274888800786925602902284721040317211860
      |8204190004229661711963779213375751149595015660496318629472654736425230817703
      |6751590673502350728354056704038674351362222477158915049530984448933309634087
      |8076932599397805419341447377441842631298608099888687413260472""".stripMargin.replaceAll("""\n""", "")

  /**
    * Represents the integer value 3 as a `BigInt`.
    */
  private[core] lazy val bigThree: BigInt = BigInt(3)
  /**
    * Represents the rational number three as a constant of type Rational.
    * It is constructed using `Rational(bigThree)`.
    */
  val three: Rational = Rational(bigThree)
  /**
    * A Rational number representing the reciprocal of `three`.
    * The `invert` method on the `three` object is used to calculate this value.
    */
  val third: Rational = three.invert

  /**
    * A constant value representing the rational number 4.
    * This is created using the Rational class.
    */
  val four: Rational = Rational(4)

  /**
    * Represents a rational number instance initialized with the value 9.
    */
  val nine: Rational = Rational(9)

  /**
   * Method to construct a Rational from two BigInt values.
   * NOTE: the this method ensures that the numerator and denominator are normalized.
   *
   * @param n the numerator.
   * @param d the denominator.
   * @return a Rational with the same ratio as n/d.
   */
  def apply(n: BigInt, d: BigInt): Rational =
    normalize(n, d)

  /**
   * Method to construct a Rational from a BigInt numerator and a Long denominator.
   * NOTE: the this method ensures that the numerator and denominator are normalized.
   *
   * @param n the numerator.
   * @param d the denominator.
   * @return a Rational with the same ratio as n/d.
   */
  def apply(n: BigInt, d: Long): Rational =
    apply(n, BigInt(d))

  /**
   * Method to construct a Rational from a BigInt.
   *
   * @param n the value.
   * @return a Rational with the same value as n.
   */
  def apply(n: BigInt): Rational =
    apply(n, bigOne)

  /**
   * Method to construct a Rational from a BigInt with sign defined by "negative".
   *
   * @param n        the value.
   * @param negative if true then the sign of the result will be flipped.
   * @return a Rational with the same value as n.
   */
  def apply(n: BigInt, negative: Boolean): Rational =
    apply(n).applySign(negative)

  /**
   * Creates an exact `Rational` representation of the given `Double` value if possible.
   *
   * @param x the `Double` value to be converted to an exact `Rational`
   * @return a `Try` containing the exact `Rational` if the conversion is possible and exact,
   *         or a `Failure` containing an `ArithmeticException` if the conversion is not possible
   *         or loses precision
   */
  def createExact(x: Double): Try[Rational] =
    if (x.compare(negZeroDouble) == 0)
      Success(negZero)
    else {
      val bigDecimal = BigDecimal.valueOf(x)
      if (bigDecimal.isDecimalDouble) Try {
        val r = Rational(bigDecimal)
        if (r.toDouble == x)
          r
        else
          throw new ArithmeticException(s"Rational.createExact: $x is not exact")
      }
      else Failure(new ArithmeticException(s"Cannot create exact Rational from Double: $x"))
    }

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Long): Rational =
    apply(BigInt(n))

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Int): Rational =
    apply(BigInt(n))

  /**
   * Method to construct a Rational based on a BigDecimal.
   *
   * @param x the BigDecimal to convert.
   * @return a Rational which is equal to x.
   * @throws com.phasmidsoftware.number.core.inner.RationalException if x cannot be represented as a Rational.
   */
  def apply(x: BigDecimal): Rational =
    if (x.scale >= 0) {
      val e = BigDecimal.apply(10).pow(x.scale)
      (for (n <- (x * e).toBigIntExact; d <- e.toBigIntExact) yield Rational(n, d)) match {
        case Some(r) =>
          r
        case None =>
          throw RationalException(s"Rational.apply(BigDecimal): cannot represent $x as a Rational")
      }
    }
    else
      x.toBigIntExact match {
        case Some(b) =>
          Rational(b)
        case None =>
          throw RationalException(s"cannot get value from BigDecimal $x")
    }

  /**
   * Method to construct a Rational based on a String.
   * NOTE: this method is NOT safe. It is much better to invoke parse(w).
   *
   * @param w the String value.
   * @return a Rational corresponding to the value given by w.
   * @throws com.phasmidsoftware.number.parse.RationalParserException if w is malformed.
   */
  def apply(w: String): Rational =
    toTry(VulgarFraction(w), Failure(new NoSuchElementException)).orElse(parse(w)) match {
      case Success(value) => value
      case Failure(e) => throw e
    }

  /**
    * Creates a new Rational object by applying the given tuple consisting of numerator and denominator.
    *
    * @param t a tuple where the first element is the numerator and the second element is the denominator
    * @return a new Rational object constructed from the provided numerator and denominator
    */
  def apply(t: (Int, Int)): Rational =
    Rational(t._1, t._2)

  /**
   * Method to construct a Try[Rational] based on a String.
   *
   * @param w the String value.
   * @return either Success(Rational) with value corresponding to the value given by w
   *         or Failure(RationalParserException) if w is malformed.
   */
  def parse(w: String): Try[Rational] =
    RationalParser.parse(w)

  /**
    * Implicit object which bestows all of the Ordering, Numeric, etc. functionality to a Rational.
    */
  implicit object RationalFractional extends RationalIsFractional

  val MAX_PRIME_FACTORS = 7

  /**
   * Method to yield a Rational exponent (in the sense of the a literal Double: for example 1.0Ex).
   *
   * @param x the power to which we should raise 10.
   * @return 10 raised to the power x, expressed as a Rational.
   */
  def exponent(x: Int): Rational =
    ten.power(x)

  /**
    * Attempts to convert a Rational value to a Long within the range of Long values.
    *
    * @param x the Rational value to convert to a Long
    * @return a Try containing the Long value if the conversion is successful,
    *         or a Failure if the value is out of range or cannot be converted
    */
  def toLong(x: Rational): Try[Long] =
    narrow(x, Long.MinValue, Long.MaxValue) map (_.toLong)

  /**
    * Narrows a Rational value to a BigInt within the specified range.
    *
    * @param x   the Rational number to be narrowed
    * @param min the minimum bound of the range
    * @param max the maximum bound of the range
    * @return a Try instance containing the narrowed BigInt within the range,
    *         or a Failure if the narrowing operation fails
    */
  private def narrow(x: Rational, min: BigInt, max: BigInt): Try[BigInt] =
    for (b <- toBigInt(x); z <- narrow(b, min, max)) yield z

  // CONSIDER making this public
  private def toBigInt(x: Rational): Try[BigInt] =
    if (x.isWhole)
      Success(x.n)
    else
      Failure(RationalException(s"toBigInt: $x is " + (if (x.isInfinity)
        "infinite" else "not whole")))

  /**
    * This method gets an optional Int from a Rational.
    *
    * @param x a Rational.
    * @return an Option[Int]
    */
  def toIntOption(x: Rational): Option[Int] =
    if (x.isWhole && x.n.isValidInt)
      Some(x.n.toInt)
    else
      None

  /**
    * Checks if the given Rational number has the correct ratio defined by the provided numerator (top)
    * and denominator (bottom).
    *
    * @param r      the Rational number to verify
    * @param top    the expected numerator of the ratio
    * @param bottom the expected denominator of the ratio
    * @return true if the Rational number has the correct ratio, false otherwise
    * @throws com.phasmidsoftware.number.core.inner.RationalException if the ratio is incorrect
    */// CONSIDER making this private or moving back into RationalSpec
  def hasCorrectRatio(r: Rational, top: BigInt, bottom: BigInt): Boolean = {
    val _a = r * bottom
    val result = bottom == 0 || _a.isInfinity || (_a.isWhole && _a.toBigInt == top)
    if (!result)
      throw RationalException(s"incorrect ratio: r=${r.n}/${r.d}, top=$top, bottom=$bottom, _a=${_a}, gcd=${top.gcd(bottom)}")
    result
  }

  /**
   * Method to get the (integral) xth root of b.
   *
   * @param b a BigInt.
   * @param x an Int.
   * @return an optional BigInt which, when raised to the power of x, equals b.
   */
  def rootOfBigInt(b: BigInt, x: Int): Option[BigInt] =
    Try(BigInt(math.round(math.pow(b.toDouble, 1.0 / x)))) match {
      case Success(z) if z.pow(x) == b =>
        Some(z)
      case _ =>
        None
    }

  /**
   * A lazy value representing a map of predefined integers and their corresponding square root values.
   *
   * This map contains key-value pairs where the key represents a non-negative integer and the value is its integer square root.
   * The square root values are exact for perfect squares included in the map.
   */
  lazy val squareRoots: Map[Int, Int] = Map(0 -> 0, 1 -> 1, 4 -> 2, 9 -> 3, 16 -> 4, 25 -> 5, 36 -> 6, 49 -> 7, 64 -> 8, 81 -> 9, 100 -> 10, 121 -> 11, 144 -> 12, 256 -> 16, 625 -> 25, 1024 -> 32, 4096 -> 64, 10000 -> 100)

  /**
    * A lazy value representing a map of predefined integers and their corresponding cube root values.
    *
    * This map contains key-value pairs where the key represents a non-negative integer and the value is its integer cube root.
    * The cube root values are exact for perfect cubes included in the map.
    */
  lazy val cubeRoots: Map[Int, Int] = Map(0 -> 0, 1 -> 1, 8 -> 2, 27 -> 3, 64 -> 4, 125 -> 5, 216 -> 6, 343 -> 7, 512 -> 8, 729 -> 9, 1000 -> 10, 4096 -> 16, 32768 -> 32, 1000000 -> 100)

  /**
    * A lazily evaluated map that provides the base-2 logarithms of specific powers of 2.
    * The keys represent powers of 2 (e.g., 1, 2, 4, 8, etc.), and the values are their corresponding base-2 logarithms.
    */
  lazy val binaryLogs: Map[Int, Int] = Map(1 -> 0, 2 -> 1, 4 -> 2, 8 -> 3, 16 -> 4, 32 -> 5, 64 -> 6, 128 -> 7, 256 -> 8, 512 -> 9, 1024 -> 10, 2048 -> 11, 4096 -> 12, 8192 -> 13)

  /**
    * A constant value representing the maximum square value that is encoded in `squareRoots` (above).
    */
  val maxSquare = 100

  /**
   * This method gets an Int from a Rational.
   *
   * XXX: Needs to be public for testing
   *
   * @param x a Rational.
   * @return an Try[Int]
   */
  def toInt(x: Rational): Try[Int] =
    if (x.isWhole && x.n.isValidInt)
      Success(x.n.toInt)
    else
      Failure(RationalException(s"$x is not whole"))

  /**
   * Get Rational x as a pair of Ints (if possible).
   * The result will be at full precision or an exception will be thrown.
   *
   * @param x a Rational.
   * @return an optional tuple of Ints.
   */
  def toInts(x: Rational): Option[(Int, Int)] =
    for (a <- toInt(x.n); b <- toInt(x.d)) yield (a, b)

  /**
    * Get Rational x as a pair of Longs (if possible).
    * The result will be at full precision or an exception will be thrown.
    *
    * @param x a Rational.
    * @return an optional tuple of Longs.
    */
  def toLongs(x: Rational): Option[(Long, Long)] =
    (for {
      n <- narrow(x.n.toLong, Long.MinValue, Long.MaxValue)
      d <- narrow(x.d.toLong, Long.MinValue, Long.MaxValue)
    } yield (n.toLong, d.toLong)).toOption

  /**
   * Generate the String representation of n/d where d is a Prime number (at least, very probably).
   *
   * @param n the numerator.
   * @param d the (prime) denominator.
   * @return a String of repeated digits.
   */
  def findRepeatingSequence(n: BigInt, d: BigInt, primeFactors: Seq[Prime]): Try[String] = {
    def findRepeatingPattern(bigNumber: String, h: Int): Option[Int] = {
      val range = Range(0, bigNumber.length / 2) // This is somewhat arbitrary
      range.foldLeft[Option[Int]](None) { (b, x) => b.orElse(testSequence(bigNumber, h, x)) }
    }

    def createRecurrenceString(ps: Seq[Int]) = {
      val bigNumber = BigNumber.value(n).divide(BigNumber.value(d)).toString
      val l = bigNumber.length

      @tailrec
      def inner(candidates: List[Int]): Try[String] = candidates match {
        case Nil =>
          Failure[String](RationalException(s"Rational.findRepeatingSequence: no sequence"))
        case h :: t =>
          findRepeatingPattern(bigNumber, h) match {
            case Some(z) if z + h < l =>
              Success(bigNumber.substring(0, z) + "<" + bigNumber.substring(z, z + h) + ">")
            case Some(z) =>
              Failure[String](RationalException(s"Rational.findRepeatingSequence: logic error: pattern exhausts bigNumber: ${z + h} > $l"))
            case None =>
              inner(t)
          }
      }

      inner(ps.sorted.toList)
    }

    (n.isValidInt, d.isValidInt) match {
      case (true, true) =>
        getPeriods(d, primeFactors, MAX_PRIME_FACTORS) match {
          case Success(ps) =>
            createRecurrenceString(ps)
          case Failure(x) =>
            Failure(x)
        }
      case _ =>
        Failure[String](RationalException(s"Rational.numerator and denominator are not both of type Int"))
    }
  }

  /**
    * Calculates the potential periods of a repeating fractional part for a given denominator
    * based on its prime factors, given constraints, and a maximum threshold for calculations.
    *
    * @param d            The denominator of the fraction represented as a BigInt.
    * @param primeFactors A sequence of prime factors associated with the denominator.
    * @param max          The maximum threshold value used in the calculations, such as limits for prime factorization.
    * @return A Try containing a sequence of possible repeating period lengths as integers if successful,
    *         or a Failure containing an exception if the computation fails.
    */
  private def getPeriods(d: BigInt, primeFactors: Seq[Prime], max: Int): Try[Seq[Int]] = {

    def getCandidatePatternLengths(h: Int, t: List[Int]) = {
      val z: BigInt = t.foldLeft(BigInt(h))((b, y) => b * y)
      val pos = Prime.primeFactors(z, Some(max * max)).map(_.toIntOption)
      FP.sequence(pos) match {
        case Some(Nil) if z == BigInt(1) =>
          Success(Seq(1))
        case Some(h :: t) =>
          expandProducts(1 :: h :: t) match {
            case Success(products) =>
              Success(products.sorted.distinct)
            case x =>
              x
          }
        case _ =>
          Failure(RationalException(s"Rational.getPeriods.getCandidatePatternLengths: logic error: $pos"))
      }
    }

    def getCandidates(xs: Seq[Int]) = xs filterNot (x => x == 0) match {
      case Nil =>
        Success(Nil) // the only factors in the denominator are 2 or 5
      case h :: t =>
        getCandidatePatternLengths(h, t)
    }

    FP.sequence(primeFactors map (_.reciprocalPeriod)) match {
      case None if d < BigNumber.MAXDECIMALDIGITS => // XXX The reason for this is that we only generate 1000 characters for the rendering
        getCandidates(Seq(d.toInt - 1))
      case None =>
        Failure(RationalException(s"Rational.getPeriods: no suitable candidates for repeating sequence length"))
      case Some(xs) =>
        getCandidates(xs)
    }
  }

  /**
    * Expands a list of integers by computing additional products such as squares, cubes,
    * quads, quints, and sexts based on the size of the list, appending these computed values
    * to the original list. For larger sizes, it throws a custom exception indicating
    * the unsupported operation.
    *
    * @param bs A list of integers to be expanded with computed products.
    */
  private def expandProducts(bs: List[Int]) = {
    def squares(bs: List[Int]) =
      for (b1 <- bs; b2 <- bs) yield b1 * b2

    def cubes(bs: List[Int]) =
      for (b1 <- bs; b2 <- bs; b3 <- bs) yield b1 * b2 * b3

    def quads(bs: List[Int]) =
      for (b1 <- bs; b2 <- bs; b3 <- bs; b4 <- bs) yield b1 * b2 * b3 * b4

    def quints(bs: List[Int]) =
      for (b1 <- bs; b2 <- bs; b3 <- bs; b4 <- bs; b5 <- bs) yield b1 * b2 * b3 * b4 * b5

    def sexts(bs: List[Int]) =
      for (b1 <- bs; b2 <- bs; b3 <- bs; b4 <- bs; b5 <- bs; b6 <- bs) yield b1 * b2 * b3 * b4 * b5 * b6

    Try {
      bs.size match {
        case 0 =>
          Nil
        case 1 =>
          bs
        case 2 =>
          bs :+ bs.product
        case 3 =>
          bs ++ squares(bs) :+ bs.product
        case 4 =>
          bs ++ squares(bs) ++ cubes(bs) :+ bs.product
        case 5 =>
          bs ++ squares(bs) ++ cubes(bs) ++ quads(bs) :+ bs.product
        case 6 =>
          bs ++ squares(bs) ++ cubes(bs) ++ quads(bs) ++ quints(bs) :+ bs.product
        case 7 =>
          bs ++ squares(bs) ++ cubes(bs) ++ quads(bs) ++ quints(bs) ++ sexts(bs) :+ bs.product
        case _ =>
          throw RationalException(s"Rational.getPeriods: not yet implemented for: $bs")
      }
    }
  }

  /**
    * Converts a given Rational number to its Double representation.
    *
    * @param x the Rational number to be converted.
    * @return the Double representation of the given Rational number.
    */
  private def toDouble(x: Rational): Double =
    if (x eq negZero) -0.0
    else Try((BigDecimal(x.n) / BigDecimal(x.d)).toDouble).getOrElse(toDoubleViaString(x.n) / toDoubleViaString(x.d))

  private def testSequence(w: String, n: Int, i: Int): Option[Int] =
    w.substring(i).grouped(n).toSeq match {
      case Nil =>
        Some(i)
      case h :: t if t.forall(x => h startsWith x) =>
        Some(i)
      case _ =>
        None
    }

  /**
    * Calculates the square root of a given number as a `Rational` approximation
    * within a specified precision defined by epsilon.
    *
    * @param n       the number for which the square root is to be calculated
    * @param epsilon the precision for the approximation of the square root
    * @return an `Option[Rational]` containing the rational approximation of
    *         the square root if supported, or `None` if the case is not implemented
    */
  private def squareRoot(n: BigInt, epsilon: Double): Option[Rational] = n match {
    case `bigTwo` =>
      ContinuedFraction.root2.toRational(epsilon)
    case `bigThree` =>
      ContinuedFraction.root3.toRational(epsilon)
    case _ =>
      None // TODO implement these cases
  }

  /**
    * Narrows a given `BigInt` value to ensure it falls within a specified range.
    * If the value is outside the range, a `Failure` is returned with a `RationalException`.
    *
    * @param x   the `BigInt` value to be narrowed
    * @param min the minimum allowed value (inclusive)
    * @param max the maximum allowed value (inclusive)
    * @return a `Success` containing the value if it falls within the range,
    *         or a `Failure` with a `RationalException` in case of precision loss
    */
  def narrow(x: BigInt, min: BigInt, max: BigInt): Try[BigInt] =
    if (min <= x && x <= max)
      Success(x)
    else
      Failure(RationalException("narrow: loss of precision"))

  /**
    * Method to process the numerator and denominator to ensure that the denominator is never zero and never shares a common factor with the numerator.
    *
    * @param n the numerator
    * @param d the denominator
    * @return a Rational formed from n and d.
    */
  @scala.annotation.tailrec
  private def normalize(n: BigInt, d: BigInt): Rational = (n, d) match {
    case (Rational.bigZero, Rational.bigNegOne) =>
      new Rational(n, d) // negative zero: leave as is
    case _ if d < 0 =>
      normalize(-n, -d)
    case _ =>
      val g = n.gcd(d)
      g.signum match {
        case 0 =>
          NaN
        case _ =>
          new Rational(n / g, d / g)
      }
  }

  /**
    * Subtracts one rational number from another.
    *
    * @param x the rational number to be subtracted from
    * @param y the rational number to subtract
    * @return the result of subtracting y from x as a rational number
    */
  private def minus(x: Rational, y: Rational): Rational =
    plus(x, negate(y))

  /**
    * Negates the given Rational number, producing its additive inverse.
    *
    * @param x the Rational number to be negated
    * @return a new Rational number which is the negation of the input
    */
  private def negate(x: Rational): Rational =
    Rational(-x.n, x.d)

  /**
    * Adds two Rational numbers and returns the result.
    *
    * @param x the first rational number
    * @param y the second rational number
    * @return the sum of the two rational numbers as a Rational
    */
  private def plus(x: Rational, y: Rational): Rational = (x, y) match {
    case (Rational.negZero, z) =>
      z
    case (z, Rational.negZero) =>
      z
    case _ =>
      Rational((x.n * y.d) + (y.n * x.d), x.d * y.d)
  }

  /**
    * Multiplies two Rational numbers and returns the resulting Rational number.
    *
    * @param x the first Rational number
    * @param y the second Rational number
    * @return the product of the two Rational numbers, or Rational.zero if either operand is Rational.negZero
    */
  private def times(x: Rational, y: Rational): Rational = (x, y) match {
    case (Rational.negZero, _) =>
      zero
    case (_, Rational.negZero) =>
      zero
    case _ =>
      Rational(x.n * y.n, x.d * y.d)
  }

  /**
    * Converts a given BigInt value to a Double by first converting it to a string representation.
    *
    * @param x the BigInt value to be converted to a Double
    */
  private def toDoubleViaString(x: BigInt) =
    x.toString().toDouble

  /**
    * Converts a Rational number to a Float.
    *
    * @param x the Rational number to be converted
    * @return the Float representation of the given Rational number
    */
  private def toFloat(x: Rational): Float =
    toDouble(x).toFloat

  /**
    * Converts a given BigInt to an Option[Int], if it falls within the valid range of Int.
    *
    * @param x the input BigInt to be converted
    * @return Some(Int) if the input is within the range of Int, otherwise None
    */
  private def toInt(x: BigInt): Option[Int] =
    if (x.isValidInt)
      Some(x.toInt)
    else
      None

  /**
    * Compares two Rational numbers and returns an integer indicating their relative order.
    *
    * @param x the first Rational number to compare
    * @param y the second Rational number to compare
    * @return a negative integer, zero, or a positive integer as the first Rational
    *         is less than, equal to, or greater than the second Rational
    */
  private def compare(x: Rational, y: Rational): Int =
    minus(x, y).signum

  /**
    * Trait defined to support the methods of Ordering[Rational].
    */
  trait RationalIsOrdering extends Ordering[Rational] {
    def compare(x: Rational, y: Rational): Int =
      x.compare(y)
  }

  /**
    * Divides one Rational number by another.
    *
    * @param x the numerator rational number
    * @param y the denominator rational number
    * @return the result of dividing x by y as a Rational number
    */
  private def div(x: Rational, y: Rational): Rational = x / y

  /**
    * Trait defined to support the methods of Numeric[Rational].
    */
  trait RationalIsNumeric extends RationalIsOrdering with Numeric[Rational] {
    /**
      * Adds two Rational numbers together.
      *
      * @param x the first Rational number
      * @param y the second Rational number
      * @return the sum of the two Rational numbers
      */
    def plus(x: Rational, y: Rational): Rational = x + y

    /**
      * Subtracts two Rational numbers.
      *
      * @param x the first Rational number
      * @param y the second Rational number to be subtracted from the first
      * @return the result of subtracting y from x as a Rational number
      */
    def minus(x: Rational, y: Rational): Rational = x - y

    /**
      * Multiplies two Rational numbers and returns the resulting Rational.
      *
      * @param x the first Rational number
      * @param y the second Rational number
      * @return the product of the two Rational numbers
      */
    def times(x: Rational, y: Rational): Rational = x * y

    /**
      * Negates the given Rational number by inverting the sign of its numerator.
      *
      * @param x the Rational number to be negated
      * @return a new Rational number that is the negation of the input
      */
    def negate(x: Rational): Rational =
      Rational(-x.n, x.d)

    /**
      * Creates a Rational instance from an integer.
      *
      * @param x the integer value to be converted to a Rational
      * @return the corresponding Rational representation of the given integer
      */
    def fromInt(x: Int): Rational =
      Rational(x)

    /**
      * Parses the provided string to an optional Rational value.
      *
      * @param str the string representation of the rational number to be parsed
      * @return an Option containing the parsed Rational if successful, or None if parsing fails
      */
    def parseString(str: String): Option[Rational] =
      Rational.parse(str).toOption

    /**
      * Converts a `Rational` number to an `Int` by truncating towards zero.
      *
      * @param x the `Rational` number to be converted to an `Int`
      * @return the integer representation of the `Rational` value
      */
    def toInt(x: Rational): Int =
      x.toInt

    /**
      * Converts a Rational number to its Long representation.
      *
      * @param x the Rational number to convert
      * @return the Long value corresponding to the given Rational
      */
    def toLong(x: Rational): Long =
      x.toLong

    /**
      * Converts a given Rational number to its equivalent Float representation.
      *
      * @param x the Rational number to be converted.
      * @return the Float representation of the given Rational number.
      */
    def toFloat(x: Rational): Float =
      x.toFloat

    /**
      * Converts a Rational number to a Double.
      *
      * @param x the Rational number to be converted to a Double
      * @return the Double representation of the given Rational number
      */
    def toDouble(x: Rational): Double =
      x.toDouble
  }

  /**
    * Trait defined to support the methods of Fractional[Rational].
    */
  trait RationalIsFractional extends RationalIsNumeric with Fractional[Rational] {
    /**
      * Divides one Rational number by another.
      *
      * @param x the numerator Rational number
      * @param y the denominator Rational number
      * @return the resulting Rational number after division
      */
    def div(x: Rational, y: Rational): Rational =
      Rational.div(x, y)
  }

  private def approximatePositive(x: Double)(implicit epsilon: Tolerance) =
    if (x > 0.1 && x < 10)
      approximateSmall(x)
    else {
      val e: Int = getExponent(x)
      val scale = pow(2, -e)
      approximateSmall(x * scale) * two.power(e)
    }

  private def approximateSmall(x: Double)(implicit epsilon: Tolerance) =
    if (x < 1)
      approximate(x)
    else {
      val y = floor(x).toInt
      approximate(x - y) + y
    }
}

/**
 * Exception class for Rationals.
 *
 * @param s the cause as a String.
 */
case class RationalException(s: String) extends Exception(s)

/**
  * The `VulgarFraction` object provides a mapping of common fractional values
  * to their corresponding Unicode vulgar fraction characters. It also includes
  * functionality to extract a Unicode representation of a vulgar fraction
  * from a given `Rational` number.
  *
  * This object uses a predefined `Map` of numerator and denominator pairs
  * to their Unicode character equivalents.
  *
  * The primary use case for this object is working with `Rational` numbers
  * and converting them to their Unicode vulgar fraction representation when applicable.
  *
  * Functionality:
  * - Provides the mapping of common fractions to Unicode vulgar fraction characters.
  * - Extracts a Unicode vulgar fraction character from a `Rational` number when possible.
  */
object VulgarFraction {
  val mapping: Map[(Int, Int), String] = Map(
    (1, 4) -> "\u00BC",
    (1, 2) -> "\u00BD",
    (3, 4) -> "\u00BE",
    (1, 3) -> "\u2153",
    (2, 3) -> "\u2154",
    (1, 5) -> "\u2155",
    (2, 5) -> "\u2156",
    (3, 5) -> "\u2157",
    (4, 5) -> "\u2158",
    (1, 6) -> "\u2159",
    (5, 6) -> "\u215A",
    (1, 8) -> "\u215B",
    (3, 8) -> "\u215C",
    (5, 8) -> "\u215D",
    (7, 8) -> "\u215E",
    (1, 7) -> "\u2150",
    (1, 9) -> "\u2151",
    (1, 10) -> "\u2152",
  )

  val reverseMapping: Map[String, (Int, Int)] = mapping.map(_.swap)

  def apply(w: String): Option[Rational] =
    reverseMapping.get(w) map Rational.apply

  def unapply(x: Rational): Option[String] =
    toInts(x) flatMap { case (n, d) => mapping.get((n, d)) }
}

/**
 * Value class to define Tolerance.
 *
 * @param x        the tolerance (epsilon) value.
 * @param maxDenom used in the approximation of an irrational number --
 *                 whereby the denominator is limited to approximately this value.
 */
case class Tolerance(x: Double, maxDenom: BigInt)

/**
 * Companion object to Tolerance.
 */
object Tolerance {
  /**
   * Standard tolerance (epsilon) of 10 to the power of -15.
   */
  implicit val standardTolerance: Tolerance = Tolerance(1E-14, BigInt(1000000000L))
}
