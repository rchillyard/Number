package com.phasmidsoftware.number.algebra

import algebra.ring.Ring
import cats.Show
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.{Fuzziness, FuzzyNumber, NumberException}
import com.phasmidsoftware.number.parse.NumberParser
import com.phasmidsoftware.number.algebra.Real.realIsRing
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.misc.FP
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Represents a (usually) fuzzy real number, which combines a numerical value with an associated fuzziness attribute.
  * A fuzzy number expresses imprecision or uncertainty around a central numeric value, enabling computations
  * that consider tolerances or relative inaccuracies in mathematical operations.
  *
  * This class extends additive and multiplicative algebraic structures (`Additive` and `Multiplicative`
  * traits) while implementing `Number` as a base trait to encapsulate numeric-like behavior.
  *
  * NOTE that a Real with empty fuzz is considered to be exact.
  *
  * CONSIDER defining an Imaginary type to represent imaginary numbers.
  *
  * @param value the central numeric value of the fuzzy number
  * @param fuzz  the optional fuzziness associated with the numeric value
  */
case class Real(value: Double, fuzz: Option[Fuzziness[Double]]) extends CanAdd[Real, Structure] with Number {

  /**
    * Determines whether this `Valuable` is exact, i.e., has no fuzz.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  override def isExact: Boolean = fuzz.isEmpty

  /**
    * Converts the given numeric value to an optional representation.
    *
    * This method accepts a number of type T, where T is a subtype of Number,
    * and returns an Option containing the input number if certain conditions
    * (not detailed in this method's implementation) are met, otherwise None.
    *
    * @param t a prototype of the required output.
    * @return an Option wrapping the input number if the conversion is successful, otherwise None
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = t match {
//    case _: RationalNumber =>
//      FP.whenever(isExact)(Rational.createExact(value).toOption).asInstanceOf[Option[T]]
    case _: Real =>
      Some(this).asInstanceOf[Option[T]]
//    case _: Angle =>
//      scale(Real.one / Real.pi).map(Angle(_)).asInstanceOf[Option[T]]
    case _ =>
      None
  }

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    Option.when(isExact)(value)

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean =
    compare(Real.zero) == 0

  /**
    * Represents the additive identity element for the type `T`.
    *
    * The additive identity, commonly referred to as "zero," is the element in an
    * additive algebraic structure that, when added to any element of the structure,
    * results in the same element. For any element `x`, `x + zero` and `zero + x` should
    * equal `x`.
    */
  lazy val zero: Real = realIsRing.zero

  /**
    * Represents the multiplicative identity element of the structure.
    *
    * The `one` value serves as the neutral element for the multiplication operation, meaning
    * that for any instance `t` of type `T`, the equation `one * t = t * one = t` holds true.
    */
  lazy val one: Real = realIsRing.one

  /**
    * Converts this `Number` into its corresponding `Rational` representation, if possible.
    *
    * @return an `Option[Rational]` containing the `Rational` representation of this `Number`
    *         if it can be converted, or `None` if the conversion is not possible.
    */
  lazy val toRational: Option[Rational] = None

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  lazy val signum: Int = compare(zero)

  /**
    * Compares the current `Number` instance with another `Number` instance exactly.
    *
    * This method should only be used when both `Number` instances are exact. An exact
    * comparison ensures a precise evaluation without considering any approximations
    * or fuzziness. If either `Number` is not exact, this method is expected to
    * throw an exception, as the operation is undefined for non-exact numbers.
    *
    * @param that the `Number` to compare against
    * @return an integer value:
    *         - negative if the current `Number` is less than `that`
    *         - zero if the current `Number` is equal to `that`
    *         - positive if the current `Number` is greater than `that`
    */
  def compareExact(that: Scalar): Option[Int] =
    if isExact && that.isExact then
      that match {
        case Real(x, _) =>
          Some(value.compare(x))
        case WholeNumber(x) =>
          Some(value.compare(x.toDouble))
        case n =>
          n.convert(this) map (x => value.compare(x.value))
      }
    else
      None

  /**
    * Compares the current `Number` instance with another `Number` instance.
    *
    * This method performs a comparison of two `Number` instances. If both numbers are exact, it uses exact comparison.
    * If one or both numbers are not exact, it attempts to approximate and compare. Note that in some cases, this method
    * may throw an exception if an invalid approximation logic is encountered.
    *
    * @param that the `Number` instance to compare the current instance against
    * @return an integer value:
    *         - a negative value if this `Number` is less than `that`
    *         - zero if this `Number` is equal to `that`
    *         - a positive value if this `Number` is greater than `that`
    */
  override def compare(that: Number): Int = that match {
    case r: Real =>
      realIsRing.compare(this, r)
    case n =>
      FP.getOrThrow(n.approximation.map(a => compare(a)), NumberException(s"Real.compare: logic error: $this, $that"))
  }

  /**
    * Renders this `Real` for presentation.
    *
    * This method converts the current `Real` instance into its string representation,
    * including its value and fuzziness as defined by the internal rendering logic.
    *
    * @return a string representation of the `Real`
    */
  lazy val render: String = new core.FuzzyNumber(Value.fromDouble(Some(value)), PureNumber, fuzz).render

  /**
    * Performs an addition operation between the current scalar and another scalar.
    *
    * This method attempts to add the given `Scalar` to the current one. If the
    * provided scalar is a `Real` instance, it directly computes the sum using
    * the `realIsRing.plus` method. For other scalar types, it attempts
    * to convert them to a compatible type with this scalar and computes the
    * result if the conversion is successful.
    *
    * @param that the `Scalar` to be added to the current scalar
    * @return an `Option[Scalar]` containing the result of the addition,
    *         or `None` if the operation cannot be performed
    */
  infix def doPlus(that: Scalar): Option[Scalar] = that match {
    case f@Real(_, _) =>
      Some(realIsRing.plus(this, f))
    case n =>
      n.convert(this) map (x => realIsRing.plus(this, x))
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor: Option[Factor] = Some(PureNumber)

  /**
    * Adds the specified `T` to this `T` instance.
    *
    * @param t an instance of `T` to be added to this `T`
    * @return a new `T` representing the sum of this `T` and the given `T`
    */
  def +(t: Real): Real = realIsRing.plus(this, t)

  /**
    * Computes the additive inverse of this instance.
    *
    * This method returns a new instance representing the negation of this value,
    * as defined in the additive structure of the type `T`.
    *
    * @return a new instance of type `T` that is the additive inverse of this instance
    */
  def unary_- : Real = realIsRing.negate(this)

  /**
    * Subtracts the specified `T` from this `T` instance.
    *
    * @param t an instance of `T` to be subtracted from this `T`
    * @return a new `Additive[T]` representing the result of the subtraction of the given `T` from this `T`
    */
  def -(t: Real): Real = realIsRing.plus(this, -t)

  /**
    * Multiplies the specified `T` by this `T` instance.
    *
    * @param t an instance of `T` to be multiplied by this `T`
    * @return a new `Multiplicative[T]` representing the product of this `T` and the given `T`
    */
  def *(t: Real): Real = realIsRing.times(this, t)

  /**
    * Divides this `T` instance by the specified `T`.
    *
    * @param t an instance of `T` to be the divisor
    * @return a new `Multiplicative[T]` representing the quotient of this `T` and `t`
    */
  def /(t: Real): Real =
    realIsRing.inverse(t) map (z => realIsRing.times(this, z)) getOrElse Real.Infinity

  /**
    * Scales the instance of type T by the given integer multiplier.
    *
    * This method performs a multiplication operation between the current instance and
    * the specified integer, returning an optional result. The result is defined if
    * the scaling operation is valid for the specific implementation.
    *
    * @param that the integer multiplier used to scale the instance
    * @return an Option containing the scaled result of type T, or None if the operation is invalid
    */
  def doScaleInt(that: Int): Option[Monotone] =
    Some(copy(value = that * value))

  /**
    * Scale this Real by the given scalar, provided that it is exact.
    * This method is used to scale a Real by a scalar that is known to be exact.
    * If you want to simply multiply this Real by a scalar, use the * operator.
    *
    * @param scalar the exact scalar to scale by
    * @return a scaled Real with the same relative error as this.
    */
  def scale(scalar: Scalar): Option[Real] =
    for
      x <- scalar.maybeDouble if scalar.isExact
      f <- fuzz
    yield Real(value * x, f.normalize(x, true))

  /**
    * Converts the current `Real` instance into an instance of `FuzzyNumber`.
    *
    * This method creates a `FuzzyNumber` using the value of the current
    * `Real`, its numeric type as `PureNumber`, and its associated fuzziness.
    *
    * @return a `FuzzyNumber` representation of this `Real` instance
    */
  def toOldFuzzyNumber: FuzzyNumber =
    FuzzyNumber(Value.fromDouble(Some(value)), PureNumber, fuzz)

  /**
    * Scales the current `Real` instance by the mathematical constant π (pi).
    *
    * This method creates a new `Real` by multiplying the internal value of
    * the current instance with the constant π.
    * The fuzziness remains unaffected in the resulting instance.
    *
    * @return a new `Real` instance with its value scaled by π
    */
  private[algebra] lazy val scaleByPi: Real =
    Real(value * Real.pi.value, Some(Fuzziness.doublePrecision))

  /**
    * Raises the current instance of type `T` to the rational power `n`.
    * This operation extends the `Multiplicative` structure by allowing
    * instances to be exponentiated with rational exponents.
    *
    * @param n the rational exponent to which the current instance is raised
    * @return an `Option` containing the result of the exponentiation if applicable, or `None` if the operation is undefined
    */
  def power(r: Rational): Option[Real] =
    Some(Real(math.pow(value, r.toDouble), fuzz.map(Fuzziness.scaleTransform(r.toDouble))))
}

/**
  * Represents a fuzzy number, combining a numeric value with a level of fuzziness.
  *
  * A fuzzy number encapsulates imprecision or uncertainty alongside its value,
  * and supports arithmetic operations and algebraic structure integration.
  * Fuzzy numbers are useful in computational scenarios involving uncertainty,
  * approximate calculations, or tolerances. This object provides factory methods,
  * constants, and implicit type class instances for working with fuzzy numbers.
  */
object Real {
  /**
    * Constructs a `Real` instance using the given long value and no fuzziness.
    *
    * This method converts the specified long value to a double and delegates to the
    * corresponding `apply(value: Double, fuzziness: Option[Fuzziness[Double]])` method
    * to construct the `Real` instance.
    *
    * @param value the long value to be associated with the `Real`
    * @return a `Real` instance initialized with the given value (converted to double) and default fuzziness
    */
  def apply(value: Long): Real = apply(value.toDouble, None)

  /**
    * Constructs a `Real` with the given numeric value and a default fuzziness level.
    *
    * This method creates a `Real` instance using the provided value and a default
    * level of fuzziness based on double-precision.
    *
    * @param value the numeric value to be associated with the `Real`
    * @return a `Real` instance initialized with the specified value and default fuzziness
    */
  def apply(value: Double): Real = apply(value, Some(Fuzziness.doublePrecision))

  /**
    * Constructs a `Real` instance using the numerical value and optional fuzziness of a given `core.Real` instance.
    *
    * This method extracts a double-precision value and an optional fuzziness level from the input `core.Real`,
    * and creates a new `Real` instance initialized with those parameters.
    *
    * @param x the input `core.Real` instance, providing the value and optional fuzziness for the new `Real`
    * @return a `Real` instance initialized with the value and fuzziness derived from the input `core.Real`
    */
  def apply(x: core.Real): Real = apply(x.toDouble, x.asNumber.flatMap(_.fuzz))

  /**
    * Parses a string representation of a number and constructs a `Real` instance.
    * If the input string cannot be parsed into a valid number, a `NumberException` is thrown.
    *
    * @param w the input string to be parsed and converted into a `Real` instance
    * @return a `Real` instance constructed from the parsed numeric value of the input string
    */
  def apply(w: String): Real = {
    val z: Try[core.Real] = NumberParser.parseNumber(w).map(x => core.Real(x))
    FP.getOrThrow[Real](z.map(x => Real(x)).toOption, NumberException(s"Real.apply(String): cannot parse $w"))
  }

  /**
    * Constructs a new `Real` instance based on the values of an existing `Real`.
    *
    * This method takes a `Real` instance as an input and creates a new `Real`
    * with the same numerical value but potentially adjusted fuzziness.
    *
    * @param real the input `Real` used to initialize the new `Real` instance
    * @return a `Real` instance initialized with the value of the input `Real`
    */
  def convertFromOldReal(real: core.Real): Real =
    new Real(real.toDouble, real.asNumber.flatMap(_.fuzz))

  /**
    * Creates a `Real` instance with a value of 0 and a default fuzziness level.
    *
    * @return a `Real` representing the value zero with default fuzziness
    */
  val zero: Real = apply(0)

  /**
    * Returns a `Real` representing the value one with double precision fuzziness.
    *
    * @return a `Real` instance initialized with the numeric value 1 and a default level of fuzziness.
    */
  val one: Real = apply(1)

  /**
    * Returns a `Real` representing the mathematical constant π (pi)
    * with a default level of fuzziness.
    *
    * @return a `Real` initialized with the value of π and the default fuzziness level
    */
  def pi: Real = apply(Math.PI)

  /**
    * Provides an implicit instance of `Show` for the `Real` type.
    *
    * This implementation defines how instances of `Real` are converted
    * to a human-readable string representation by invoking their `render` method.
    *
    * It enables seamless integration with type classes requiring a `Show` instance,
    * allowing `Real` objects to be printed or logged in a human-readable format.
    */
  implicit val showFuzzyNumber: Show[Real] = Show.show(_.render)

  /**
    * Provides an implicit implementation of the `Ring` typeclass for the `Real` type.
    *
    * This object supports arithmetic operations (addition, multiplication, and negation)
    * and the retrieval of constants (`zero` and `one`) for the `Real` type, thus
    * conforming to the requirements of the `Ring` algebraic structure.
    *
    * CONSIDER extending Semiring instead (Claude feels that it would be appropriate for fuzzy numbers),
    * but I don't have a problem with the additive (or multiplicative) inverse.
    */
  implicit object realIsRing extends Ring[Real] with Ordering[Real] {

    /**
      * Adds two Real instances and returns their sum as a new Real.
      *
      * @param x the first Real operand
      * @param y the second Real operand
      * @return the sum of the two Real instances
      */
    def plus(x: Real, y: Real): Real = {
      val value = x.value + y.value
      val combination = Fuzziness.combine(x.value, y.value, relative = false, independent = true)(x.fuzz -> y.fuzz)
      Real(value, combination)
    }

    /**
      * Multiplies two Real instances and returns the result.
      *
      * @param x the first Real instance
      * @param y the second Real instance
      * @return the product of the two Real instances
      */
    def times(x: Real, y: Real): Real = {
      val value = x.value * y.value
      Real(value, Fuzziness.combine(x.value, y.value, relative = true, independent = true)(x.fuzz -> y.fuzz))
    }

    /**
      * Computes the negation of the given fuzzy number.
      *
      * @param x the input fuzzy number to negate
      * @return the negated fuzzy number
      */
    def negate(x: Real): Real =
      x.copy(value = -x.value)

    /**
      * Computes the multiplicative inverse of a given `Real` number.
      * If the input value is zero, the operation returns `None` since zero
      * does not have a multiplicative inverse. Otherwise, the method returns
      * an `Option` containing a new `Real` instance representing the inverse.
      *
      * @param x the input `Real` number for which the inverse is to be calculated
      * @return an `Option` containing the inverse of the input as a `Real`,
      *         or `None` if the input value is zero
      */
    def inverse(x: Real): Option[Real] = x.value match {
      case 0 =>
        None
      case v =>
        val maybeFuzz: Option[Fuzziness[Double]] = x.fuzz.flatMap(f => f.normalize(v, relative = true))
        Some(Real(1 / v, maybeFuzz))
    }

    /**
      * Divides one `Real` by another `Real`.
      *
      * NOTE that this method is not defined for `Ring[Real]` because, strictly speaking,
      * fuzzy numbers do not all have a multiplicative inverse.
      *
      * This method performs a division operation between two `Real` instances,
      * producing a new `Real` that represents the result of the division.
      * The operation takes into account the fuzziness of the input numbers.
      *
      * @param x the numerator `Real` (dividend)
      * @param y the denominator `Real` (divisor)
      * @return a `Real` representing the result of dividing `x` by `y`
      */
    def div(x: Real, y: Real): Real = {
      (realIsRing.inverse(y) map (z => realIsRing.times(x, z))
          ).getOrElse(Real(Double.PositiveInfinity, Some(Fuzziness.createFuzz(0))))
    }

    /**
      * Converts an integer value into a `Real` representation.
      *
      * @param n the integer value to be converted
      * @return a `Real` instance representing the given integer
      */
    override def fromInt(n: Int): Real = Real(n, None)

    /**
      * Returns the additive identity element for the `Real` type.
      * The additive identity is the element that, when added to any other element,
      * results in that other element unchanged.
      *
      * @return the additive identity (zero) as a `Real`
      */
    lazy val zero: Real = fromInt(0)

    /**
      * Returns the multiplicative identity element of the `Real` type.
      *
      * @return a `Real` instance representing the value "1"
      */
    lazy val one: Real = fromInt(1)

    /**
      * Parses a given string representation and attempts to convert it into an Option of Real.
      * The method handles parsing errors and exceptional conditions appropriately.
      *
      * @param str the input string to be parsed, representing a potential Real number
      * @return an Option containing the parsed Real if successful, or None if parsing fails
      */
    def parseString(str: String): Option[Real] =
      core.Number.parse(str) match {
        case Success(core.Number(v, PureNumber)) =>
          fromFuzzyPureValue(v, None)
        case Success(core.FuzzyNumber(v, PureNumber, fo)) =>
          fromFuzzyPureValue(v, fo)
        case Failure(NonFatal(ex)) =>
          println(ex.getLocalizedMessage); None
        case Failure(ex) =>
          throw ex
        case _ =>
          println(s"Real: cannot parse String as Real: $str"); None
      }

    /**
      * Compares two `Real` instances and returns an integer indicating their relative order.
      *
      * @param x the first `Real` instance to compare
      * @param y the second `Real` instance to compare
      * @return an integer value:
      *         - negative if `x` is less than `y`
      *         - zero if `x` is equal to `y`
      *         - positive if `x` is greater than `y`
      */
    def compare(x: Real, y: Real): Int =
      x.compareExact(y) getOrElse
          x.toOldFuzzyNumber.fuzzyCompare(y.toOldFuzzyNumber, 0.5)
  }

  /**
    * Attempts to construct an `Option[Real]` instance using a provided `Value` and optional fuzziness level.
    * This method converts the given `Value` into a `Double` if possible, and combines it with the specified
    * fuzziness to create a `Real` instance.
    *
    * @param v  the input `Value` to potentially convert into a `Real`
    * @param fo an optional fuzziness level to associate with the created `Real` if the conversion is successful
    * @return an `Option[Real]`, which will contain a `Real` instance if the conversion from `Value` to `Double`
    *         succeeds, or `None` otherwise
    */
  private def fromFuzzyPureValue(v: Value, fo: Option[Fuzziness[Double]]): Option[Real] =
    Value.maybeDouble(v).map(x => Real(x, fo))

  /**
    * Represents an immutable instance of positive infinity as a `Real`.
    *
    * `Infinity` is a predefined object within the `Real` class hierarchy that
    * symbolizes the mathematical concept of positive infinity. It is constructed
    * using `Double.PositiveInfinity` as the underlying representation and
    * does not associate any fuzziness with its value.
    *
    * This object can be used in computations requiring an explicit representation
    * of positive infinity.
    *
    * @see [[Real]]
    */
  object Infinity extends Real(Double.PositiveInfinity, None)
}

