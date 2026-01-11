/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.ring.{AdditiveCommutativeMonoid, MultiplicativeGroup}
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.algebra.eager.Real.realIsRing
import com.phasmidsoftware.number.algebra.eager.WholeNumber.WholeNumberIsCommutativeRing
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Represents a pure number that can be compared, approximated, and converted to other types.
  *
  * `Number` is a trait that extends the `Scalar` trait, adding functionality for ordered comparison.
  *
  * CONSIDER why does it not extend Ordered[Number]?
  */
trait Number extends Scalar with Unitary with Ordered[Scalar] {

  /**
    * Normalizes this `T` to its simplest equivalent form where T is the supertype of all possible results.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest representation of this value that is a subtype of `T`.
    */
  def normalize: Number

  /**
    * Multiplies the current number with another number using the defined multiplicative group.
    *
    * @param other The number to be multiplied with the current number.
    * @return The result of the multiplication as a number.
    */
  def *(other: Number): Number =
    summon[MultiplicativeGroup[Number]].multiplicative.combine(this, other).normalize

  /**
    * Divides this `Number` by another `Number`.
    *
    * The division is performed based on the type of the input `Number`:
    * - If the input is an `ExactNumber`, it computes the reciprocal of the input, converts it to a `RationalNumber`,
    *   multiplies it with `this` `Number`, and normalizes the result.
    * - For unsupported types, an `AlgebraException` is thrown.
    *
    * @param other the `Number` by which this `Number` is to be divided
    * @return the result of division as a `Number`
    * @throws com.phasmidsoftware.number.algebra.util.AlgebraException if the division is unsupported for the provided `other` type
    */
  def /(other: Number): Number = other match {
    case c: CanMultiplyAndDivide[Number] =>
      (this * c.reciprocal).normalize
    case e: ExactNumber =>
      (this * e.toRationalNumber.reciprocal).normalize
    case _ =>
      throw AlgebraException(s"Number./: logic error: $this / $other")
  }


  /**
    * Adds this `Number` instance to another `Number` instance and returns the result.
    *
    * CONSIDER: there is an alternative way of adding Numbers and that is to summon an commutative addend. Which should we be using?
    * This method allows for appropriate conversions whereas the other method does not.
    *
    * The addition is performed based on the types of the two `Number` instances:
    * - If both are `WholeNumber`, their values are added directly.
    * - If one is a `RationalNumber` and the other is an `ExactNumber`, the `ExactNumber` is converted to match the type of the `RationalNumber` before addition.
    * - If one is a `Real`, an attempt is made to convert the other number to match the type of the `Real` before addition.
    * - For unsupported or invalid type combinations, an `AlgebraException` is thrown.
    *
    * @param other the `Number` instance to add to this instance
    * @return a `Number` instance representing the sum of this instance and the provided `other` instance
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the addition cannot be performed due to unsupported or invalid type combinations
    */
  def +(other: Number): Number = (this, other) match {
    case (a: WholeNumber, b: WholeNumber) =>
      (a + b).normalize
    case (a: WholeNumber, b: Q) =>
      RationalNumber(a.toRational + b.toRational)
    case (a: RationalNumber, b: ExactNumber) =>
      b.convert(a).map(x => (a + x).normalize).getOrElse(throw AlgebraException(s"Number.+: logic error: RationalNumber: $this + $other"))
    case (a: Real, b) =>
      b.convert(a).map(x => (a + x).normalize).getOrElse(throw AlgebraException(s"Number.+: logic error: Real: $this + $other"))
    case _ =>
      throw AlgebraException(s"Number.+: logic error: $this + $other")
  }

  /**
    * Attempts to obtain a `Factor` representation in a specific `Context`.
    *
    * A `Factor` could represent entities like `PureNumber`, `Radian`, etc., that
    * provide numerical or dimensional meaning to a value. This method evaluates
    * the current instance within the given `Context` to determine if a valid
    * factor can be generated.
    *
    * @param context the evaluation context in which to determine the factor. The `Context`
    *                specifies the criteria under which the factorization is valid.
    * @return an `Option[Factor]` containing the factor if it can be determined,
    *         or `None` if no suitable factor exists within the provided `Context`.
    */
  def maybeFactor(context: Context): Option[Factor] =
    Option.when(context.factorQualifies(PureNumber))(PureNumber)

  /**
    * Compares this `Number` instance with a `Scalar` instance.
    *
    * This method matches the type of the input `Scalar` and performs a comparison based on its type. If the input is a `Number`,
    * it delegates to the `compare(Number)` method. If the input is a `Radians`, it first attempts to convert the `Radians`
    * into a comparable value and then performs the comparison.
    *
    * @param that the `Scalar` instance to compare the current instance against.
    * @return an integer value:
    *         - a negative value if the current instance is less than `that`.
    *         - zero if the current instance is equal to `that`.
    *         - a positive value if the current instance is greater than `that`.
    *
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the conversion to a comparable value is not possible
    */
  def compare(that: Scalar): Int = that match {
    case number: Number =>
      compare(number)
    case radians: Radians =>
      radians.convert(this) match {
        case Some(x) =>
          compare(x)
        case None =>
          throw AlgebraException(s"Number.compare(Scalar): logic error: $this, $that")
      }
  }

  /**
    * Compares the current `Number` instance with another `Number` instance.
    *
    * This method performs a comparison of two `Number` instances. If both numbers are exact, it uses exact comparison.
    * If one or both numbers are not exact, it attempts to approximate and compare. Note that in some cases, this method
    * may throw an exception if an invalid approximation logic is encountered.
    *
    * @param that the `Number` instance to compare the current instance against
    * @return an integer value:
    *         - a negative value if this `Number` is lower than `that`
    *         - zero if this `Number` is equal to `that`
    *         - a positive value if this `Number` is greater than `that`
    *
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the comparison cannot be performed due to invalid approximation logic
    */
  def compare(that: Number): Int =
    if isExact && that.isExact then // XXX both are exact
      FP.recover(compareExact(that))(AlgebraException(s"Number.compare(Number): logic error: $this, $that"))
    else if !isExact then { // XXX this is not exact
      val maybeInt: Option[Int] = for
        x <- approximation()
        y <- that.approximation()
      yield x.compare(y)
      FP.recover(maybeInt)(AlgebraException("Number.compare: Logic error"))
    }
    else // XXX this is exact and that is not exact
      -that.compare(this)

  /**
    * Determines if this instance is equivalent to another `Eager` instance.
    *
    * Compares two instances, `this` and `that`, for equivalence. If both instances
    * are of type `Number`, it checks for exact equality when both are exact. If
    * one or both are not exact, it attempts an approximate comparison using their
    * approximation values. For types other than `Number`, it delegates to the
    * super implementation.
    *
    * @param that the `Eager` instance to compare with this instance
    * @return a `Try[Boolean]` indicating:
    *         - `Success(true)` if the instances are equivalent
    *         - `Success(false)` if the instances are not equivalent
    *         - `Failure` if an error occurs during the comparison process
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: Number, b: Number) =>
      // Both are Numbers - compare them
      if (a.isExact && b.isExact) {
        Success(a.compareExact(b).contains(0)) // TODO check if we should normalize a and b first.
      } else {
        // At least one is not exact - use approximation
        val maybeResult = for {
          aApprox <- a.approximation(true)
          bApprox <- b.approximation(true)
        } yield aApprox.value == bApprox.value

        FP.toTry(maybeResult, Failure(AlgebraException(s"Number.eqv: cannot compare $this and $that")))
      }
    case _ =>
      super.eqv(that)
  }

  /**
    * Performs a fuzzy equivalence comparison between this `Number` instance and another `Eager` instance.
    *
    * This method evaluates whether two instances are approximately equal within a given tolerance `p`.
    * If both instances are of type `Number`, it converts them to `Real` for the comparison.
    * If either of the approximations fails, or the comparison is not applicable, it delegates to the super implementation.
    *
    * @param p    the tolerance level for the fuzzy equality comparison. A higher value increases the permissible difference.
    * @param that the `Eager` instance to compare this `Number` instance against.
    * @return a `Try[Boolean]` indicating whether the two instances are approximately equal (`true`) or not (`false`).
    *         Returns `Failure` if the comparison cannot be performed or encounters an error.
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: Number, b: Number) =>
      // Convert both to Real for fuzzy comparison
      val maybeResult = for {
        aReal <- a match {
          case real: Real => Some(real)
          case _ => a.approximation(true)
        }
        bReal <- b match {
          case real: Real => Some(real)
          case _ => b.approximation(true)
        }
      } yield aReal.fuzzyEqv(p)(bReal).getOrElse(false)

      Success(maybeResult.getOrElse(false))
    case _ =>
      super.fuzzyEqv(p)(that)
  }

  /**
    * A scale factor applied to this `Number`.
    *
    * The `scaleFactor` represents a multiplier that influences computations or adjustments involving this number.
    * It is commonly used to scaleFactor or manipulate the magnitude of the number in various arithmetic or operational contexts.
    */
  lazy val scaleFactor: Double = 1.0
}

/**
  * Represents a number that can be expressed and operated on with exactitude.
  *
  * Subtypes of ExactNumber include WholeNumber and RationalNumber.
  * Although some instances of Real are exact, the type in general cannot be assumed to represent an exact value.
  */
trait ExactNumber extends Number with Exact with Q with Scalable[ExactNumber] with CanPower[ExactNumber] {

  /**
    * Converts this ExactNumber into a RationalNumber representation.
    *
    * @return A RationalNumber instance representing the current value.
    */
  def toRationalNumber: RationalNumber = RationalNumber(toRational)

  /**
    * Attempts to retrieve an optional integer representation of the current exact number.
    * The value is returned as an `Option[Int]` if the underlying rational number is a whole
    * number and fits within the range of a 32-bit signed integer. Otherwise, `None` is returned.
    *
    * @return an optional integer representation of the number.
    */
  def maybeInt: Option[Int] = toRational.maybeInt

  /**
    * Determines if the current instance of RationalNumber is represented exactly.
    *
    * @return true as the instance is always exact.
    */
  def isExact: Boolean = true

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = maybeInt.contains(1)

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = signum == 0

  /**
    * Converts this `WholeNumber` instance to its `Double` representation.
    *
    * @return the `Double` value obtained by converting this `WholeNumber` to a `Rational` and then to a `Double`.
    */
  def asDouble: Double = toRational.toDouble

  /**
    * Scales the current `WholeNumber` by a given Rational multiplier and returns the result
    * wrapped as an `Option[Monotone]`.
    * CONSIDER renaming as `*`
    *
    * @param scale the Rational multiplier to scale the current `WholeNumber`
    * @return an `Option[Monotone]` representing the scaled result
    */
  def scale(scale: Rational): Number =
    toRationalNumber * scale

  /**
    * Raises the current `ExactNumber` to the power of the given `ExactNumber`.
    * The operation leverages the underlying rational representations of both numbers
    * and computes the result exactly if possible.
    *
    * @param that the exponent as an `ExactNumber` to which the current number is raised
    * @return an `Option[ExactNumber]` containing the resulting value if the power
    *         operation can be computed exactly, or `None` if it cannot
    */
  infix def pow(that: ExactNumber): Option[ExactNumber] =
    for (r <- toRational.power(that.toRational).toOption) yield RationalNumber(r).normalize
}
/**
  * The `Number` object provides predefined constants and implicit conversions related to numerical operations.
  * It includes representations of common numbers and utilities to work with the `Number` type.
  */
object Number {

//  private val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Provides a `DyadicOperator` instance for the `Number` type. This instance
    * defines how dyadic operations are performed on different subtypes of `Number`,
    * such as `RationalNumber`, `WholeNumber`, and `Real`. Cross-type operations
    * between different subtypes are also handled within this implementation.
    *
    * The operation `op` delegates to the respective `DyadicOperator` instances for each
    * specific subtype, and in the case of cross-type operations, attempts to convert
    * between the subtypes to ensure the operation can be conducted.
    *
    * @return A `DyadicOperator[Number]` instance that encapsulates the logic for
    *         performing dyadic operations on numbers and their subtypes, including
    *         cross-type compatibility.
    */
  given DyadicOperator[Number] = new DyadicOperator[Number] {
    def op[B <: Number, Z](f: (Number, B) => Try[Z])(x: Number, y: B): Try[Z] = (x, y) match {
      case (a: RationalNumber, b: RationalNumber) =>
        implicitly[DyadicOperator[RationalNumber]].op(f)(a, b)
      case (a: WholeNumber, b: WholeNumber) =>
        implicitly[DyadicOperator[WholeNumber]].op(f)(a, b)
      case (a: Real, b: Real) =>
        implicitly[DyadicOperator[Real]].op(f)(a, b)

      // Cross-type operations:
      case (x: RationalNumber, y: WholeNumber) =>
        op(f)(x, y.toRationalNumber.asInstanceOf[B])
      case (x: WholeNumber, y: RationalNumber) =>
        op(f)(x.toRationalNumber, y)
      case (x: Real, y: R) =>
        val zyo: Option[Try[Z]] = y.convert(Real.zero).map(r => op(f)(x, r.asInstanceOf[B]))
        FP.recoverWithTry(zyo)(FP.fail(s"Number.DyadicOperator: logic error $x. $y")).flatten
      case (x: R, y: Real) =>
        val zyo: Option[Try[Z]] = x.convert(Real.zero).map(r => op(f)(r, y))
        FP.recoverWithTry(zyo)(FP.fail(s"Number.DyadicOperator: logic error $x. $y")).flatten
      case (a, b) =>
        f(a, b)
    }
  }

  /**
    * Provides an instance of type `Eq` for the `Number` type.
    *
    * This instance defines equality for `Number` objects by delegating to the
    * `eqv` method. If the `eqv` method successfully determines equivalence,
    * the result is used. Otherwise, `false` is returned.
    *
    * @return an `Eq` instance for the `Number` type, implementing custom equality
    *         logic based on the `eqv` method's logic.
    */
  given Eq[Number] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  /**
    * Provides a given implementation of `FuzzyEq` for the `Number` type.
    *
    * This implementation defines fuzzy equality for `Number` instances by checking
    * for exact equality (`===`) or performing a fuzzy equivalence comparison with a given
    * probability `p`. The method delegates the fuzzy equivalence logic to the `fuzzyEqv` method
    * of the `Number` type if exact equivalence is not established.
    *
    * @return An instance of `FuzzyEq[Number]` that supports fuzzy equality comparison for `Number` objects.
    */
  given FuzzyEq[Number] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * Represents the value `0` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val zero: Number = WholeNumber.zero
  /**
    * Represents the number one as a `WholeNumber` instance.
    */
  val one: Number = WholeNumber.one
  /**
    * Represents the value `-1` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val minusOne: Number = WholeNumber.minusOne
  /**
    * Represents the value `2` as an instance of `WholeNumber`.
    * It is a predefined constant in the `Number` object.
    */
  val two: Number = WholeNumber.two

  /**
    *
    */
  implicit def convIntToNumber(x: Int): Number = WholeNumber(x)()

  /**
    * Provides an implicit additive commutative monoid implementation for the `Number` type.
    *
    * This object defines the `zero` element and the `plus` operation for the `Number` type,
    * conforming to the algebraic structure of an additive commutative monoid.
    *
    * The `zero` element acts as the additive identity, and the `plus` operation is
    * both associative and commutative, enabling addition of two `Number` instances.
    */
  implicit object NumberIsAdditiveCommutativeMonoid extends AdditiveCommutativeMonoid[Number] {
    /**
      * Returns the additive identity element for the `Number` type.
      *
      * This method provides the `zero` element, which serves as the neutral element in
      * addition operations for instances of `Number`. Adding `zero` to any `Number`
      * instance will result in the same `Number` instance.
      *
      * @return the additive identity element for `Number`
      */
    def zero: Number = Number.zero

    /**
      * Adds two `Number` instances together.
      *
      * The method determines the type of the given `Number` instances and performs the appropriate
      * addition operation. It supports addition for `Real`, `RationalNumber`, and `WholeNumber` types.
      * If the types are incompatible or cannot be converted to corresponding types, it throws a `AlgebraException`.
      *
      * This method is marked as `@tailrec` to optimize recursive addition operations.
      *
      * @param x the first `Number` instance
      * @param y the second `Number` instance
      * @return a `Number` instance representing the result of the addition of `x` and `y`
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the types of `x` and `y` cannot be added
      */
    @tailrec
    def plus(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.plus(a, b)
      case (a, b: Real) =>
        plus(y, x)
      case (a: Real, b) =>
        FP.recover(b.convert(Real.zero).map(bAsReal => realIsRing.plus(a, bAsReal)))
            (throw AlgebraException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: RationalNumber, b: RationalNumber) =>
        rationalNumberIsField.plus(a, b)
      case (a, b: RationalNumber) =>
        plus(y, x)
      case (a: RationalNumber, b) =>
        FP.recover(b.convert(RationalNumber.zero).map(bAsRational => rationalNumberIsField.plus(a, bAsRational)))
            (throw AlgebraException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: WholeNumber, b: WholeNumber) =>
        WholeNumberIsCommutativeRing.plus(a, b)
      case (a, b: WholeNumber) =>
        plus(y, x)
      case _ =>
        throw AlgebraException("Number.plus: logic error: cannot add " + x + " and " + y)
    }
  }

  given Convertible[Number, Number] with
    def convert(witness: Number, u: Number): Number = u

  /**
    * An implicit object defining the multiplicative group structure for `Number` instances.
    *
    * The `NumberIsMultiplicativeGroup` provides implementations for the methods required 
    * by the `MultiplicativeGroup` type class, enabling operations such as multiplication 
    * (`times`) and division (`div`) as well as access to the multiplicative identity (`one`).
    * It supports a variety of `Number` subtypes, including `Real`, `RationalNumber`, and `WholeNumber`, 
    * and handles necessary conversions or raises exceptions when operations cannot be performed.
    */
  implicit object NumberIsMultiplicativeGroup extends MultiplicativeGroup[Number] {
    /**
      * Retrieves the numerical value that represents one.
      *
      * @return the predefined constant representing the number one
      */
    def one: Number = Number.one

    /**
      * Multiplies two `Number` instances and returns the resulting `Number`.
      *
      * This method performs multiplication between two instances of `Number`. The exact operation depends on the type of the
      * input numbers, supporting various types such as `Real`, `RationalNumber`, and `WholeNumber`. If the types mismatch
      * or a conversion between types is required, the appropriate logic is applied to carry out the multiplication. If the
      * numbers cannot be multiplied due to an unsupported type or incorrect conversion, an exception is thrown.
      *
      * @param x the first `Number` to be multiplied
      * @param y the second `Number` to be multiplied
      * @return the result of multiplying `x` and `y` as a `Number`
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the multiplication is not possible for the given inputs
      */
    @tailrec
    def times(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.times(a, b)
      case (a, b: Real) =>
        times(y, x)
      case (a: Real, b) =>
        FP.recover(b.convert(Real.zero).map(_ * a))(AlgebraException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: RationalNumber, b: RationalNumber) =>
        a * b
      case (a, b: RationalNumber) =>
        times(y, x)
      case (a: RationalNumber, b) =>
        FP.recover(b.convert(RationalNumber.zero).map(a * _))(AlgebraException("Number.plus: logic error: cannot convert " + b + " to a RationalNumber"))
      case (a: WholeNumber, b: WholeNumber) =>
        a * b
      case (a, b: WholeNumber) =>
        times(y, x)
      case _ =>
        throw AlgebraException("Number.times: logic error: cannot multiply " + x + " and " + y)
    }

    /**
      * Divides one `Number` by another and returns the resulting `Number`.
      *
      * This method performs safe division for different types of `Number` instances, such as `Real`. 
      * If the divisor is zero or an unsupported type is encountered, an exception is thrown.
      *
      * @param x the numerator as a `Number`
      * @param y the denominator as a `Number`
      * @return the result of dividing `x` by `y` as a `Number`
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if division by zero occurs or if the operation cannot be performed
      */
    def div(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.div(a, b)
      case (a: Number, b: Real) =>
        realIsRing.inverse(b) match {
          case Some(z) => times(a, z)
          case None => throw AlgebraException("Number.div: logic error: cannot divide by zero")
        }
      case (a: Number, b: ExactNumber) =>
        a.scale(b.toRational.invert) match {
          case number: Number => number
          case x => throw AlgebraException(s"Number.div: logic error: result of division is not a Number: $x")
        }
      case _ =>
        throw AlgebraException("Number.div: logic error: cannot divide " + x + " by " + y)
    }
  }
}