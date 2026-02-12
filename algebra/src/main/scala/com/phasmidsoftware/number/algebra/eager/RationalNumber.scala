/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.ring.{AdditiveCommutativeGroup, Field}
import cats.Show
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.algebra.util.LatexRenderer.{LatexRendererOps, frac}
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.Rational

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Success, Try}

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
  * CONSIDER merging the Rational class into this class (there's no compelling reason to have two separate classes).
  *
  * @constructor Creates a `RationalNumber` with the given `Rational` value.
  * @param r          The `Rational` value represented by this `RationalNumber`.
  * @param percentage whether this is a percentage value or not.
  *                   Similarly to the `degrees` attribute of `Angle`,
  *                   this is a flag that is primarily cosmetic.
  */
case class RationalNumber(r: Rational, percentage: Boolean = false)(val maybeName: Option[String] = None) extends ExactNumber with Q with CanAddAndSubtract[RationalNumber] with CanMultiplyAndDivide[RationalNumber] with Scalable[ExactNumber] {
  /**
    * Normalizes the current object to ensure it is represented in a simplified form.
    * If the denominator (r.d) is 1, it represents the object as a WholeNumber.
    * Otherwise, it returns the object as is.
    *
    * @return a simplified version of the object as a `Valuable`,
    *         either as a `WholeNumber` or the current object.
    */
  def normalize: ExactNumber =
    if (r.isInteger && !percentage)
      WholeNumber(r.n)
    else
      this

  /**
    * Subtracts the given rational number from this one.
    * NOTE that this result is not normalized.
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
      Some(r.compare(Rational(x)))
    case Real(v, None) => // exact Real only
      Some(r.toDouble.compare(v))
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
    * Scales the current instance of `RationalNumber` by the given `Int` value.
    *
    * @param factor the integer value to scale the `RationalNumber` instance by
    * @return an `Option[RationalNumber]` representing the scaled result; returns `None` if the operation cannot be performed
    */
  def *(factor: Rational): ExactNumber =
    RationalNumber(r * factor).normalize

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
    * Renders the string representation of the current `RationalNumber` instance.
    * Delegates the rendering logic to the associated `r` instance's `render` method.
    *
    * @return the string representation of this `RationalNumber`.
    */
  def render: String = maybeName getOrElse (
      if (percentage)
        s"${(r * 100).render}%" // CHECK that this never gives a ratio.
      else
        r.render
      )

  /**
    * Computes the negation of this `RationalNumber`.
    *
    * This method returns a new `RationalNumber` instance that represents the negation of the current value.
    *
    * @return a new `RationalNumber` which is the additive inverse of this `RationalNumber`
    */
  def unary_- : RationalNumber =
    rationalNumberIsField.negate(this)

  /**
    * Compares two instances of `Eager` for equality.
    *
    * This method is intended to check if the provided instances, `x` and `y`,
    * are equivalent. Currently, this functionality is not implemented
    * and will return a failure with an appropriate exception message.
    *
    * @param that the first `Eager` instance to compare
    * @return a `Try[Boolean]` where:
    *         - `Success(true)` indicates the objects are equivalent
    *         - `Success(false)` indicates the objects are not equivalent
    *         - `Failure` indicates this functionality is not implemented
    */
  override def eqv(that: Eager): Try[Boolean] = (this.normalize, that.normalize) match {
    case (RationalNumber(rx, _), RationalNumber(ry, _)) =>
      Success(rx == ry)
    case _ =>
      super.eqv(that)
  }
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
  def apply(r: Rational): RationalNumber = new RationalNumber(r)()

  /**
    * Creates a `RationalNumber` instance from two provided `Long` values,
    * representing the numerator and denominator of a rational number.
    *
    * @param x the numerator of the rational number
    * @param y the denominator of the rational number
    * @return a `RationalNumber` instance constructed from the given numerator and denominator
    */
  def apply(x: Long, y: Long): RationalNumber = new RationalNumber(Rational(x, y))()

  /**
    * Constructs a `RationalNumber` instance from the given numerator and denominator.
    *
    * @param x The numerator as a `BigInt`.
    * @param y The denominator as a `BigInt`.
    * @return A `RationalNumber` representing the fraction `x / y`.
    */
  def apply(x: BigInt, y: BigInt): RationalNumber = new RationalNumber(Rational(x, y))()

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
    RationalNumber(r / 100, percentage = true)()

  /**
    * Parses a string representation of a rational number and converts it into an `Option[RationalNumber]`.
    *
    * @param w the string representation of the rational number to be parsed
    * @return an `Option[RationalNumber]` containing the parsed rational number if successful, or `None` if parsing fails
    */
  def parse(w: String): Option[RationalNumber] = {
    val regex = """([\d/]+)(%)?""".r
    w match {
      case regex(num, s) if Option(s).isDefined =>
        Some(percentage(Rational(num)))
      case regex(num, _) =>
        Some(RationalNumber(Rational(num)))
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
    * Implicitly converts a `Rational` instance into a `RationalNumber`.
    *
    * @param x the `Rational` value to be converted
    * @return a `RationalNumber` instance representing the given `Rational`
    */
  implicit def convRationalRationalNumber(x: Rational): RationalNumber = RationalNumber(x)

  /**
    * A given instance of `DyadicOperator` for `RationalNumber` that provides an implementation of 
    * the `op` method, enabling the execution of a dyadic operation on two rational numbers. 
    * The operation is defined by a user-supplied function `f`, which specifies how the operands 
    * should be combined and produces a result wrapped in a `Try`.
    *
    * @return A `Try[Z]` representing the result of the operation `f` applied to the operands,
    *         or an exception if the operation fails.
    */
  given DyadicOperator[RationalNumber] = new DyadicOperator[RationalNumber] {
    def op[B <: RationalNumber, Z](f: (RationalNumber, B) => Try[Z])(x: RationalNumber, y: B): Try[Z] =
      f(x, y)
  }

  /**
    * Provides an instance of `Eq[RationalNumber]`, enabling equality comparison
    * between two `RationalNumber` instances.
    *
    * @return an `Eq[RationalNumber]` instance that defines equality by delegating to
    *         the `eqv` method of `RationalNumber`, or returning `false` if undefined
    */
  given Eq[RationalNumber] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  /**
    * Provides an instance of the `FuzzyEq` type class for `RationalNumber`.
    *
    * This instance defines the fuzzy equality for `RationalNumber` using
    * the `==` operator to compare two `RationalNumber` instances based on
    * their exact equality.
    *
    * @return a `FuzzyEq[RationalNumber]` instance defining the fuzzy equality for `RationalNumber`
    */
  given FuzzyEq[RationalNumber] = FuzzyEq.instance {
    (x, y, p) =>
      x == y  // Use == here, not ===
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

  /**
    * Provides a given instance of the `Convertible` typeclass for `RationalNumber` to `RationalNumber`.
    *
    * This instance defines a no-op conversion, where a `RationalNumber` is returned as-is
    * when converted to another `RationalNumber`. It enables compatibility with the `Convertible`
    * typeclass interface, but does not alter or transform the provided `RationalNumber`.
    *
    * @param witness an optional `RationalNumber` instance provided as a template (unused in this implementation)
    * @param u       the `RationalNumber` instance to be returned
    * @return the input `RationalNumber` returned directly without modification
    */
  given Convertible[RationalNumber, RationalNumber] with
    def convert(witness: RationalNumber, u: RationalNumber): RationalNumber = u

  /**
    * Provides an instance of the `Convertible` typeclass for transforming
    * an instance of `Real` into a `RationalNumber`.
    *
    * This conversion attempts to recover a `RationalNumber` representation
    * from a given `Real` object. If the `Real` instance cannot be converted
    * into a `RationalNumber`, an `AlgebraException` is thrown describing 
    * the failure.
    *
    * @return a `RationalNumber` instance that is equivalent to the provided `Real`,
    *         if the conversion is successful
    *
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the conversion fails.
    */
  given Convertible[RationalNumber, Real] with
    def convert(witness: RationalNumber, u: Real): RationalNumber =
      FP.recover(u.toMaybeRationalNumber)(AlgebraException("cannot convert $u to RationalNumber"))

  /**
    * Provides a given instance of the `Convertible` type class to enable the conversion
    * of a `WholeNumber` into a `RationalNumber`.
    *
    * This implementation defines the `convert` method, which delegates the conversion
    * of a `WholeNumber` to its corresponding `RationalNumber` representation by invoking
    * the `toRationalNumber` method on the `WholeNumber` instance.
    */
  given Convertible[RationalNumber, WholeNumber] with
    def convert(witness: RationalNumber, u: WholeNumber): RationalNumber =
      u.toRationalNumber

  /**
    * LatexRenderer for Rational numbers.
    *
    * Renders as:
    * - Integer if denominator is 1
    * - Fraction otherwise using \frac{numerator}{denominator}
    * - Handles negative values appropriately
    */
  implicit val rationalLatexRenderer: LatexRenderer[Rational] = LatexRenderer.instance { r =>
    r.renderExact match {
      case Rational.repeatingDecimals(x, repeating) if repeating.nonEmpty =>
        s"$x\\overline{$repeating}"
      case Rational.repeatingDecimals(x, _) =>
        x // TESTME this never happens, it seems.
      case Rational.rationalForm(_, _) if r.isExactDouble =>
        r.toDouble.toString // XXX It's very doubtful that this case will match
      case Rational.integerForm(n) =>
        n
      case Rational.unicodeForm(_) =>
        frac(r.n.toString(), r.d.toString())
      case Rational.rationalForm(n, d) =>
        frac(n, d)
      case x =>
        x
    }
  }

  /**
    * LatexRenderer for RationalNumber.
    *
    * Delegates to the Rational renderer.
    */
  implicit val rationalNumberLatexRenderer: LatexRenderer[RationalNumber] = LatexRenderer.instance {
    case RationalNumber(r, false) => r.toLatex
    case RationalNumber(r, _) => s"${r.value}\\%"
  }

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
