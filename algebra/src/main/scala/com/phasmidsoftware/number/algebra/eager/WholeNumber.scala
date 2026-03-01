/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.ring.{AdditiveCommutativeGroup, CommutativeRing}
import cats.Show
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.WholeNumber.WholeNumberIsCommutativeRing
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.toIntOption

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Success, Try}

/**
  * A case class representing a whole number.
  *
  * The `WholeNumber` class models any integer in the Z domain and its associated operations,
  * enabling addition, inversion, comparison, and rendering.
  *
  * @param x a BigInt value representing the whole number
  */
case class WholeNumber(x: BigInt)(val maybeName: Option[String] = None) extends ExactNumber with Z with CanAddAndSubtract[WholeNumber] with CanMultiply[WholeNumber, WholeNumber] {
  /**
    * Normalizes the current object and returns a standardized or canonical form of it.
    *
    * @return A normalized instance of the current object represented as a Valuable.
    */
  def normalize: WholeNumber = this

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * The result is not guaranteed to be normalized.
    *
    * @param factor the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  infix def *(factor: Rational): ExactNumber = RationalNumber(toRational * factor).normalize

  /**
    * Converts this instance to its corresponding integer representation.
    *
    * @return the integer value corresponding to this instance
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the conversion is not possible.
    */
  def toInt: Int =
    FP.recover(toIntOption(toRational))(AlgebraException("WholeNumber.toInt: cannot convert $this to Int"))

  /**
    * Converts the WholeNumber instance into a Rational equivalent.
    *
    * @return a Rational representation of the current WholeNumber.
    */
  def toRational: Rational = Rational(x)

  /**
    * Retrieves an `Option` containing this instance as an object of type `Z`.
    *
    * @return an `Option[Z]` that wraps the current instance, which is always `Some(this)`.
    */
  def maybeZ: Option[Z] = Some(this)

  /**
    * Converts the current instance of `WholeNumber` to an optional `Q`.
    *
    * @return an `Option[Q]` representing the current `WholeNumber` as a `Q`,
    *         or `None` if the conversion is not possible. The returned value
    *         will typically encapsulate the `WholeNumber` as a rational
    *         representation.
    */
  def maybeQ: Option[Q] = Some(RationalNumber(toRational))

  /**
    * Subtracts the specified WholeNumber from this WholeNumber.
    *
    * @param that the WholeNumber to subtract from this WholeNumber
    * @return the result of subtracting the specified WholeNumber from this WholeNumber
    */
  def -(that: WholeNumber)(using AdditiveCommutativeGroup[WholeNumber]): WholeNumber =
    WholeNumberIsCommutativeRing.minus(this, that)

  /**
    * Compares this instance with another `Scalar` for exact equivalence.
    * This method attempts to compare the two instances based on their exact numerical representation.
    *
    * @param that the `Scalar` instance to compare against
    * @return an `Option[Int]` value:
    *         - `Some(-1)` if this `Scalar` is less than `that`
    *         - `Some(0)` if this `Scalar` is equal to `that`
    *         - `Some(1)` if this `Scalar` is greater than `that`
    *         - `None` if the exact comparison is not possible
    */
  def compareExact(that: Scalar): Option[Int] = that match {
    case WholeNumber(o) =>
      Some(x.compare(o))
    case RationalNumber(r, _) =>
      Some(Rational(x).compare(r))
    case Real(v, None) => // exact Real only
      Some(x.toDouble.compare(v))
    case _ =>
      None
  }

  /**
    * Converts the given `Structure` instance to an optional transformed instance of the same type.
    * The conversion will vary depending on the specific subclass of `Structure`.
    * If the input type is unsupported for conversion, `None` is returned.
    *
    * @param t the input object of type `T`, which must be a subtype of `Structure`.
    *          The input is expected to be of a type that matches the patterns in the conversion logic.
    *
    * @tparam T the type parameter constrained to subtypes of `Structure` with a `ClassTag` evidence.
    * @return an `Option` containing a converted instance of the same type `T`
    *         if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = t match {
    case x if x.getClass == this.getClass =>
      Some(this.asInstanceOf[T])
    case _: RationalNumber =>
      Some(toRationalNumber.asInstanceOf[T])
    case _: Real =>
      Some(toReal.asInstanceOf[T])
    case _ =>
      None
  }

  /**
    * Represents the zero element of the `WholeNumber` type, adhering to the identity element defined
    * in the commutative ring algebraic structure for whole numbers.
    *
    * This `lazy val` provides a reference to the identity of the additive group, which is a `WholeNumber`
    * with a value of zero. It delegates the instantiation to `WholeNumberIsCommutativeRing.empty`.
    */
  lazy val zero: WholeNumber = WholeNumberIsCommutativeRing.empty

  /**
    * Represents the multiplicative identity for WholeNumbers.
    *
    * This value denotes one, serving as the identity element in
    * the group structure of WholeNumbers under multiplication.
    */
  lazy val one: WholeNumber = WholeNumber.one

  /**
    * Scales the current instance by a given scalar.
    *
    * @param scalar the scalar by which the instance should be scaled
    * @return an `Option[Number]` representing the scaled value:
    *         - `Some(Number)` containing the result of scaling when the operation is successful
    *         - `None` if the operation cannot be performed or is not supported for the given scalar
    */
  def scale(scalar: Scalar): Option[Number] =
    scalar match {
      case WholeNumber(i) =>
        Some(WholeNumber(x * i))
      case RationalNumber(r, _) =>
        val product: Rational = Rational(x) * r
        Option.when(product.isWhole)(WholeNumber(product.toBigInt))
      case _ => // TODO add other cases as they become available
        None
    }

  /**
    * Scales the current `WholeNumber` instance using the provided `Number`.
    *
    * This method leverages the `scale` function to perform the scaling operation and ensures
    * the result is downcast to an `Option[WholeNumber]` if possible. The operation may return
    * `None` if the scaling cannot be performed or the resulting type is not compatible.
    *
    * @param that the `Number` to be used as the scaling factor
    * @return an `Option[WholeNumber]` containing the scaled result, or `None` if the operation fails or the result is not a `WholeNumber`
    */
  def doScale(that: Number): Option[WholeNumber] =
    scale(that).asInstanceOf[Option[WholeNumber]] // TODO check this (it's not right)

  /**
    * Computes the signum of this WholeNumber.
    *
    * @return an integer value representing the sign of the number:
    *         - `0` if the number is zero,
    *         - `1` if the number is positive,
    *         - `-1` if the number is negative.
    */
  def signum: Int = x.signum

  /**
    * Renders the `WholeNumber` instance as a string representation.
    *
    * @return the string representation of this `WholeNumber` instance
    */
  lazy val render: String = maybeName getOrElse x.toString

  /**
    * Negates the current `WholeNumber` instance, returning its additive inverse.
    *
    * @return A new `WholeNumber` instance representing the negation of the current number.
    */
  def unary_- : WholeNumber =
    negate

  /**
    * Determines if the current `WholeNumber` instance is equivalent to another `Eager` instance.
    * NOTE no normalization is performed or required here.
    *
    * This method compares the current instance with the provided `that` instance. If both instances
    * are `WholeNumber` and their values are the same, it returns `Success(true)`. Otherwise, it
    * delegates to the `super` implementation of `eqv`.
    *
    * @param that the `Eager` instance to compare against
    * @return a `Try[Boolean]` yielding:
    *         - `Success(true)` if the instances are `WholeNumber` with equivalent values,
    *         - `Success(false)` if they are not equivalent,
    *         - `Failure` if any error occurs during comparison
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (WholeNumber(a), WholeNumber(b)) =>
      Success(a == b)
    case _ =>
      super.eqv(that)
  }

  /**
    * Converts the given structure to a `Real` representation.
    * CONSIDER creating a fuzzy Real.zero that we could pass in to force fuzziness here.
    * For now, we use the default fuzziness of None.
    *
    * This method transforms the value `x` into an instance of the `Real` class
    * by converting `x` to a `Double` and setting its optional parameter to `None`.
    *
    * @tparam T The type of the input structure, which must extend `Structure`
    *           and have an associated `ClassTag`.
    *
    * @return A `Real` object constructed from the input structure.
    */
  private def toReal[T <: Structure : ClassTag] =
    Real(x.toDouble, None)

  override def toString: String = s"WholeNumber($x)"
}

/**
  * The `WholeNumber` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with WholeNumbers. WholeNumbers are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object WholeNumber {
  /**
    * Constructs a new `WholeNumber` instance from a given `BigInt` value.
    *
    * @param x    the `BigInt` value to be wrapped as a `WholeNumber`
    * @param name an optional name associated with the `WholeNumber` instance; defaults to `None` if not provided
    * @return a new instance of `WholeNumber` created using the provided `BigInt` value and optional name
    */
  def apply(x: BigInt): WholeNumber = new WholeNumber(x)()

  /**
    * Represents the WholeNumber constant for the value zero (0).
    *
    * This is a lazily evaluated instance of `WholeNumber` initialized with the value 0.
    */
  lazy val zero: WholeNumber = WholeNumber(0L)(Some("0"))
  /**
    * Represents the whole number value `1` as an instance of `WholeNumber`.
    *
    * This is a lazy value, ensuring that the object is not instantiated until it is accessed,
    * and thereafter remains constant for the duration of the application's lifecycle.
    */
  lazy val one: WholeNumber = WholeNumber(1L)(Some("1"))
  /**
    * Represents the constant whole number value of -1.
    *
    * This is a lazily computed instance of the `WholeNumber` type,
    * initialized with the value -1.
    */
  lazy val minusOne: WholeNumber = WholeNumber(-1L)(Some("-1"))
  /**
    * Represents the whole number two as a lazy value of type `WholeNumber`.
    *
    * This is a predefined constant for convenient access to the numeric value 2
    * in the context of `WholeNumber` operations and conversions.
    */
  lazy val two: WholeNumber = WholeNumber(2L)(Some("2"))

  /**
    * Provides an implicit `Show` instance for the `WholeNumber` class, enabling conversion
    * of an `WholeNumber` instance to a string representation using its `render` method.
    *
    * This allows the `WholeNumber` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showWholeNumber: Show[WholeNumber] = Show.show(_.render)

  /**
    * Converts an `Int` to a `WholeNumber` implicitly.
    *
    * This implicit conversion allows an `Int` to be seamlessly treated as a `WholeNumber`.
    *
    * @param x the integer value to be converted to a `WholeNumber`
    * @return the converted `WholeNumber` instance
    */
  implicit def convIntWholeNumber(x: Int): WholeNumber = WholeNumber(x)

  /**
    * Provides a `Convertible` implementation for converting instances of `WholeNumber` to `WholeNumber`.
    *
    * This implementation allows a `WholeNumber` instance to be converted to another `WholeNumber`
    * without applying any transformation, effectively serving as an identity conversion.
    *
    * @note This implementation adheres to the contract of the `Convertible` typeclass.
    * @param witness a `WholeNumber` instance that serves as a template or context for the conversion
    * @param u       the source `WholeNumber` to be converted
    * @return the same `WholeNumber` instance as passed in `u`
    */
  given Convertible[WholeNumber, WholeNumber] with
    def convert(witness: WholeNumber, u: WholeNumber): WholeNumber = u

  /**
    * Represents a given instance of the `Convertible` typeclass to transform a `Real` into a `WholeNumber`,
    * provided the `Real` value is exact and can be represented as a whole number.
    *
    * The implementation leverages `FP.recover` to handle failures gracefully and will throw an
    * `AlgebraException` if the provided `Real` cannot be converted into a `WholeNumber`.
    *
    * The conversion is only possible if the `Real` value satisfies the following conditions:
    * - It is marked as exact.
    * - Its numerical value can be expressed as an integer.
    *
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input `Real` cannot be converted to a `WholeNumber`.
    * @return a `WholeNumber` instance representing the converted value.
    */
  given Convertible[WholeNumber, Real] with
    def convert(witness: WholeNumber, u: Real): WholeNumber =
      FP.recover(
        FP.whenever(u.isExact)(
          Rational.createExact(u.value).toOption.flatMap(r => r.maybeInt).map(WholeNumber(_))
        )
      )(AlgebraException("cannot convert $u to WholeNumber"))

  /**
    * Provides an implementation of the `DyadicOperator` trait for the `WholeNumber` type.
    *
    * This given instance enables the application of binary operations on `WholeNumber`
    * instances using a provided function `f`. The function encapsulates the operation logic
    * and handles the operand types and potential failures via `Try`.
    *
    * @return A `DyadicOperator` instance for the `WholeNumber` type,
    *         facilitating binary operations with safety provided by `Try`.
    */
  given DyadicOperator[WholeNumber] = new DyadicOperator[WholeNumber] {
    def op[B <: WholeNumber, Z](f: (WholeNumber, B) => Try[Z])(x: WholeNumber, y: B): Try[Z] =
      f(x, y)
  }

  /**
    * Provides an implementation of the `Eq` type class for the `WholeNumber` type.
    *
    * This given instance defines equality for `WholeNumber` values. It uses the `eqv`
    * method defined on `WholeNumber`, falling back to `false` if the equality operation
    * is not defined.
    *
    * @return an `Eq` instance for comparing `WholeNumber` values for equality
    */
  given Eq[WholeNumber] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  /**
    * Provides an instance of `FuzzyEq` for the `WholeNumber` type.
    *
    * This instance defines the fuzzy equality behavior for `WholeNumber`
    * values, where two `WholeNumber` instances are considered equal if
    * they are exactly the same, regardless of the probability `p`.
    *
    * @return An instance of `FuzzyEq` for `WholeNumber` that implements
    *         strict equality.
    */
  given FuzzyEq[WholeNumber] = FuzzyEq.instance {
    (x, y, p) => x == y
  }

  /**
    * Provides an implicit implementation of a commutative group for the `WholeNumber` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `WholeNumber` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  trait WholeNumberIsCommutativeRing extends CommutativeRing[WholeNumber] {
    /**
      * Provides the identity element for the `WholeNumber` group, representing a WholeNumber of zero.
      *
      * @return the `WholeNumber` instance zero.
      */
    def empty: WholeNumber = WholeNumber.zero

    /**
      * Computes the additive inverse of the given `WholeNumber`.
      *
      * This method negates the input WholeNumber, returning a `WholeNumber` instance
      * that represents its additive inverse, relative to `WholeNumber.zero`.
      *
      * @param x the `WholeNumber` instance to be inverted
      * @return a new `WholeNumber` instance representing the additive inverse of the input
      */
    def negate(x: WholeNumber): WholeNumber = WholeNumber(-x.x)

    /**
      * Returns the zero value of the `WholeNumber` type.
      *
      * @return The zero value representing a `WholeNumber` with the value of 0.
      */
    def zero: WholeNumber = WholeNumber.zero

    /**
      * Adds two `WholeNumber` instances together.
      *
      * This method computes the sum of the two provided `WholeNumber` objects 
      * and returns a new `WholeNumber` representing the result.
      *
      * @param x the first `WholeNumber` operand
      * @param y the second `WholeNumber` operand
      * @return a new `WholeNumber` instance representing the sum of `x` and `y`
      */
    def plus(x: WholeNumber, y: WholeNumber): WholeNumber =
      WholeNumber(x.x + y.x)

    /**
      * Returns the constant representing the number one as a `WholeNumber`.
      *
      * @return a `WholeNumber` instance representing the numeric value of one.
      */
    def one: WholeNumber = WholeNumber.one

    /**
      * Multiplies two `WholeNumber` instances.
      *
      * This method computes the product of the two provided `WholeNumber` objects
      * and returns a new `WholeNumber` representing the result.
      *
      * @param x the first `WholeNumber` operand
      * @param y the second `WholeNumber` operand
      * @return a new `WholeNumber` instance representing the product of `x` and `y`
      */
    def times(x: WholeNumber, y: WholeNumber): WholeNumber = WholeNumber(x.x * y.x)
  }

  /**
    * Provides an implicit implementation of a commutative group for the `WholeNumber` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `WholeNumber` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has an additive inverse.
    */
  implicit object WholeNumberIsCommutativeRing extends WholeNumberIsCommutativeRing
}
