package com.phasmidsoftware.number.algebra

import algebra.ring.{AdditiveCommutativeGroup, Field}
import cats.Show
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.Fuzziness
import scala.reflect.ClassTag

/**
  * Represents a rational number based on the `Rational` type, providing various
  * mathematical operations and utilities applicable to rational numbers.
  *
  * This class extends several traits to enable arithmetic operations such as addition,
  * subtraction, multiplication, and division. It also supports comparisons, conversions,
  * scaling, and power operations.
  *
  * The class is designed to work seamlessly with precise and exact number representations,
  * ensuring that operations maintain a high level of numeric fidelity.
  *
  * CONSIDER adding a percentage flag to Real.
  *
  * @constructor Creates a `RationalNumber` with the given `Rational` value.
  * @param r The `Rational` value represented by this `RationalNumber`.
  * @param percentage whether this is a percentage value or not.
  *                   Similarly to the `degrees` attribute of `Angle`,
  *                   this is a flag that is primarily cosmetic.
  */
case class RationalNumber(r: Rational, percentage: Boolean = false) extends Q with CanAddAndSubtract[RationalNumber, RationalNumber] with CanMultiplyAndDivide[RationalNumber] with Scalable[RationalNumber] with CanPower[RationalNumber] with Number {
  /**
    * Subtracts the given rational number from this one.
    *
    * @param that the rational number to subtract
    * @return the result of the subtraction
    */
  def -(that: RationalNumber)(using AdditiveCommutativeGroup[RationalNumber]): RationalNumber =
    AdditiveCommutativeGroup[RationalNumber].plus(this, -that)

  /**
    * Represents the zero value of the `RationalNumber` type within the field of rational numbers.
    *
    * This value is defined as the additive identity in the context of the `Field` type class,
    * meaning any `RationalNumber` added to this value will result in the original `RationalNumber`.
    */
  val zero: RationalNumber = rationalNumberIsField.zero

  /**
    * A constant that represents the identity element for multiplication within the field of `RationalNumber`.
    *
    * This value is provided by the implicit `Field` instance for `RationalNumber` and is equal to a `RationalNumber` representation of one.
    */
  val one: RationalNumber = rationalNumberIsField.one

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] = None

  /**
    * Computes the optional representation of this value as a `Z` (integer-like structure).
    * Transforms the optional integer representation of the underlying number (`maybeInt`)
    * into a corresponding `Z` instance, wrapped in `Option`.
    *
    * @return an `Option[Z]` where `Some(Z)` represents a valid integer-like structure
    *         derived from the current value, and `None` indicates that the value cannot
    *         be expressed as a `Z` (e.g., if the number is not a whole number or out of range).
    */
  def maybeZ: Option[Z] =
    r.maybeInt.map(WholeNumber(_))

  /**
    * Converts this `RationalNumber` instance to its corresponding `Double` representation.
    *
    * @return the `Double` value of this `RationalNumber`
    */
  def asDouble: Double = r.toDouble

  /**
    * Returns an Option containing this instance cast as type `Q`.
    *
    * @return an Option wrapping this instance as a type `Q`
    */
  def maybeQ: Option[Q] = Some(this)

  /**
    * Compares this `RationalNumber` instance with another `Scalar` instance for exact equivalence.
    * The comparison is performed by checking the type of the given `Scalar` and attempting an exact comparison
    * if applicable. The result is encapsulated in an `Option[Int]`.
    *
    * @param that the `Scalar` instance to compare against
    * @return an `Option[Int]`:
    *         - `Some(-1)` if this `RationalNumber` is less than `that`
    *         - `Some(0)` if this `RationalNumber` is equal to `that`
    *         - `Some(1)` if this `RationalNumber` is greater than `that`
    *         - `None` if the exact comparison is not possible
    */
  def compareExact(that: Scalar): Option[Int] = that match {
    case RationalNumber(o, _) =>
      Some(r.compareTo(o))
    case WholeNumber(x) =>
      Some(r.compare(Rational(x.toBigInt)))
    case _ =>
      None
  }

  /**
    * Converts the given instance of type `T` (a subtype of `Structure`) into an optional transformed instance of the same type.
    *
    * The method attempts to match the input object `t` to specific subtypes (`Real`, `WholeNumber`) and performs
    * a corresponding transformation if possible. If the input `t` does not match any recognized subtype, the result is `None`.
    *
    * @param t the input object of type `T`, where `T` is a subtype of `Structure`.
    * @tparam T the type of the input object, which must be a subtype of `Structure` and must have an available `ClassTag`.
    * @return an `Option` containing the transformed instance of type `T` if the conversion is successful, or `None` if it is not.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = t match {
    case x if x.getClass == this.getClass =>
      Some(this.asInstanceOf[T])
    case _: Real =>
      Some(Real(r.toDouble, None).asInstanceOf[T])
    case _: WholeNumber =>
      Option.when(r.isWhole)(WholeNumber(r.toBigInt).asInstanceOf[T])
    case _ =>
      None
  }

  /**
    * Converts the current RationalNumber instance to a Rational representation.
    *
    * @return the Rational representation of this RationalNumber.
    */
  def toRational: Rational = r

  /**
    * Computes the result of raising an instance of type `T` to the power 
    * specified by the given `RationalNumber`.
    *
    * The method returns an `Option[T]` to represent the possibility of invalid
    * operations or unsupported inputs where the computation cannot be performed.
    *
    * @param that the `RationalNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid, 
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: RationalNumber): Option[RationalNumber] = 
    r.power(that.r).map(RationalNumber(_)).toOption

  /**
    * Computes the result of raising an instance of type `T` to the power 
    * specified by the given `WholeNumber`.
    *
    * This method performs the power operation and returns the result wrapped 
    * in an `Option[T]`. If the operation is invalid or cannot be performed, 
    * `None` is returned.
    *
    * @param that the `WholeNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid, 
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: WholeNumber): Option[RationalNumber] = that.convert(RationalNumber.zero) flatMap pow

  /**
    * Scales the current scalar instance by the specified rational factor.
    *
    * @param r the `Rational` factor by which to scale the scalar
    * @return a new `Scalar` instance representing the scaled value
    */
  def scale(r: Rational): Scalar = RationalNumber(r * this.r)

  /**
    * Scales the current instance of type `T` by the specified `Double` value.
    *
    * CONSIDER does this method makes sense here? Maybe it's declared in the wrong place.
    *
    * This method applies a scaling factor to the instance, returning an `Option`
    * that contains the scaled instance if the operation is valid. If the scaling
    * operation is not valid or feasible, `None` is returned.
    *
    * @param that the `Double` value to scale the instance by
    * @return an `Option` containing the scaled instance of type `T`, or `None`
    *         if scaling is not possible
    */
  def doScaleDouble(that: Double): Option[Monotone] = None

  /**
    * Computes the power of the rational number with the given rational exponent.
    *
    * @param p the rational exponent to which the rational number is to be raised
    * @return an Option containing the resulting RationalNumber if successful, or None if the computation is not possible
    */
  def power(p: Rational): Option[RationalNumber] =
    r.power(p).map(RationalNumber(_)).toOption

  /**
    * Scales the current instance of `RationalNumber` by the given `Int` value.
    *
    * @param factor the integer value to scale the `RationalNumber` instance by
    * @return an `Option[RationalNumber]` representing the scaled result; returns `None` if the operation cannot be performed
    */
  def *(factor: Rational): RationalNumber =
    RationalNumber(r * factor)

  /**
    * Computes the sign of the current `RationalNumber`.
    * It evaluates to:
    * - 0 if the value is zero.
    * - 1 if the value is positive.
    * - -1 if the value is negative.
    *
    * @return an integer representing the sign of the number: 0 for zero, 1 for positive, and -1 for negative.
    */
  def signum: Int = r.signum

  /**
    * Determines if the current instance of RationalNumber is represented exactly.
    *
    * @return true as the instance is always exact.
    */
  override def isExact: Boolean = true

  /**
    * Retrieves an optional `Double` representation of the current rational number.
    * This method checks if the rational number can be exactly represented as a `Double`.
    * If it can, the value is converted to `Double` and returned wrapped in `Some`.
    * Otherwise, `None` is returned.
    *
    * @return an `Option[Double]`, where `Some(Double)` is returned if the rational
    *         number can be exactly represented as a `Double`, and `None` otherwise.
    */
  def maybeDouble: Option[Double] = r.maybeDouble

  /**
    * Checks if the value represented by this instance is zero.
    *
    * @return true if the value is zero, false otherwise
    */
  def isZero: Boolean = r.isZero

  /**
    * Renders the string representation of the current `RationalNumber` instance.
    * Delegates the rendering logic to the associated `r` instance's `render` method.
    *
    * @return the string representation of this `RationalNumber`.
    */
  def render: String =
    if (percentage)
      s"${(r * 100).render}%" // CHECK that this never gives a ratio.
    else
      r.render

  /**
    * Retrieves an optional Rational value.
    *
    * @return an Option containing a Rational, if available; otherwise, None.
    */
  def maybeRational: Option[Rational] = Some(r)

  /**
    * Returns an optional integer representation of this `RationalNumber`.
    * The result depends on whether the underlying `Rational` value can be converted to an integer.
    *
    * @return an `Option[Int]` containing the integer value if the conversion is possible; `None` otherwise.
    */
  def maybeInt: Option[Int] = maybeRational.flatMap(_.maybeInt)

  /**
    * Computes the negation of this `RationalNumber`.
    *
    * This method returns a new `RationalNumber` instance that represents the negation of the current value.
    *
    * @return a new `RationalNumber` which is the additive inverse of this `RationalNumber`
    */
  def unary_- : RationalNumber =
    rationalNumberIsField.negate(this)
}

/**
  * Represents a rational number with basic arithmetic operations and field properties.
  * This class provides methods and implicit instances for managing rational numbers
  * and their integration with mathematical abstractions such as the `Field` typeclass.
  */
object RationalNumber {
  /**
    * Converts a given `Rational` to a `RationalNumber`.
    *
    * @param r the `Rational` value to be converted
    * @return a new `RationalNumber` instance representing the given `Rational`
    */
  def apply(r: Rational): RationalNumber = new RationalNumber(r)

  /**
    * Creates a `RationalNumber` instance from two provided `Long` values,
    * representing the numerator and denominator of a rational number.
    *
    * @param x the numerator of the rational number
    * @param y the denominator of the rational number
    * @return a `RationalNumber` instance constructed from the given numerator and denominator
    */
  def apply(x: Long, y: Long): RationalNumber = RationalNumber(Rational(x, y))

  /**
    * Creates a rational number representation from a given integer value.
    *
    * @param x the integer value to be converted into a rational number
    * @return a `RationalNumber` representing the input integer
    */
  def apply(x: Long): RationalNumber = RationalNumber(Rational(x))

  /**
    * Converts a given `Rational` value into a `RationalNumber` representing its percentage equivalent.
    *
    * @param r the `Rational` value expressed as a percentage, e.g., 50 for 50%.
    * @return a `RationalNumber` instance representing the percentage value of the given `Rational`
    */
  def percentage(r: Rational): RationalNumber =
    RationalNumber(r / 100, percentage = true)

  /**
    * Parses a string representation of a rational number and converts it into an `Option[RationalNumber]`.
    *
    * @param w the string representation of the rational number to be parsed
    * @return an `Option[RationalNumber]` containing the parsed rational number if successful, or `None` if parsing fails
    */
  def parse(w: String): Option[RationalNumber] = {
    val regex = """([\d/]+)(%)?""".r
    w match {
      case regex(num, null) =>
        Some(RationalNumber(Rational(num)))
      case regex(num, s) =>
        Some(percentage(Rational(num)))
      case _ =>
        None
    }
  }

  /**
    * Provides an implicit `Show` instance for the `RationalNumber` class.
    * This instance defines how a `RationalNumber` is transformed into a `String`
    * representation by using its `render` method.
    */
  implicit val showRationalNumber: Show[RationalNumber] =
    Show.show(_.render)

  /**
    * Provides an implicit instance of the `Eq` typeclass for the `RationalNumber` class.
    * NOTE that we do not consider the `percentage` flag when comparing two `RationalNumber`s for equality.
    *
    * This instance allows for equality comparison between two `RationalNumber` instances,
    * based on the equality of their underlying `Rational` representations (`r` field of `RationalNumber`).
    * It ensures that two `RationalNumber` instances with the same rational value are considered equal.
    */
  implicit val rationalNumberEq: Eq[RationalNumber] = Eq.instance {
    (a1, a2) =>
      // NOTE that we should be using the === operator here, but it hasn't been defined yet for for Rational.
      a1.r == a2.r
  }

  /**
    * Represents the rational number zero.
    * This is a predefined constant in the RationalNumber class.
    */
  val zero: RationalNumber =
    RationalNumber(Rational.zero)

  /**
    * Represents the rational number one as an instance of RationalNumber.
    */
  val one: RationalNumber =
    RationalNumber(Rational.one)

  /**
    * Represents the rational number -1 as a predefined constant.
    * This is an instance of the `RationalNumber` class initialized
    * with the negative one value from the `Rational` type.
    */
  val minusOne: RationalNumber =
    RationalNumber(Rational.negOne)

  /**
    * Represents a predefined constant value of one-half as a RationalNumber.
    * This is a convenience representation of the fraction 1/2 in the system of rational numbers.
    */
  val half: RationalNumber =
    RationalNumber(Rational.half)

  /**
    * Represents a rational number with an infinite value.
    *
    * This value is constructed from the infinity representation of the `Rational` type.
    */
  val infinity: RationalNumber =
    RationalNumber(Rational.infinity)

  given Convertible[RationalNumber, RationalNumber] with
    def convert(witness: RationalNumber, u: RationalNumber): RationalNumber = u

  given Convertible[RationalNumber, Real] with
    def convert(witness: RationalNumber, u: Real): RationalNumber =
      FP.recover(u.toMaybeRationalNumber)(AlgebraException("cannot convert $u to RationalNumber"))

  given Convertible[RationalNumber, WholeNumber] with
    def convert(witness: RationalNumber, u: WholeNumber): RationalNumber =
      u.toRationalNumber

  /**
    * Provides an implicit implementation of the `Field` type class for the `RationalNumber` type.
    *
    * This object defines the standard operations required for `RationalNumber`
    * to function as a field, including addition, multiplication, division,
    * and their respective identity and inverse operations. By extending
    * the `Field` type class, it ensures compliance with field axioms.
    */
  implicit object rationalNumberIsField extends Field[RationalNumber] {
    /**
      * Returns the zero value of a `RationalNumber`.
      *
      * @return the zero value of the `RationalNumber` type
      */
    val zero: RationalNumber =
      RationalNumber(Rational.zero)

    /**
      * Retrieves the constant rational number representing one.
      *
      * @return A `RationalNumber` instance equal to one.
      */
    val one: RationalNumber =
      RationalNumber(Rational.one)

    /**
      * Adds two rational numbers and returns their sum.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the sum of the two rational numbers
      */
    def plus(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r + y.r)

    /**
      * Returns the additive inverse of the given rational number.
      *
      * @param x the rational number to negate
      * @return a rational number representing the additive inverse of the input
      */
    def negate(x: RationalNumber): RationalNumber =
      RationalNumber(-x.r)

    /**
      * Divides one rational number by another.
      *
      * @param x the numerator rational number
      * @param y the denominator rational number
      * @return a new RationalNumber representing the result of the division
      */
    def div(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r / y.r)

    /**
      * Multiplies two rational numbers.
      *
      * @param x the first rational number
      * @param y the second rational number
      * @return the product of the two rational numbers
      */
    def times(x: RationalNumber, y: RationalNumber): RationalNumber =
      RationalNumber(x.r * y.r)
  }
}
