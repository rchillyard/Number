package com.phasmidsoftware.number.algebra

import algebra.ring.{AdditiveCommutativeMonoid, MultiplicativeGroup}
import com.phasmidsoftware.number.algebra.RationalNumber.rationalNumberIsField
import com.phasmidsoftware.number.algebra.Real.realIsRing
import com.phasmidsoftware.number.algebra.WholeNumber.WholeNumberIsCommutativeRing
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber}
import com.phasmidsoftware.number.core.numerical.NumberException
import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Represents a pure number that can be compared, approximated, and converted to other types.
  *
  * `Number` is a trait that extends the `Scalar` trait, adding functionality for ordered comparison.
  *
  * CONSIDER why does it not extend Ordered[Number]?
  */
trait Number extends Scalar with Ordered[Scalar] {

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
  def maybeFactor(context: Context): Option[Factor] = Some(PureNumber)

  /**
    * Compares this `Number` instance with a `Scalar` instance.
    *
    * This method matches the type of the input `Scalar` and performs a comparison based on its type. If the input is a `Number`,
    * it delegates to the `compare(Number)` method. If the input is a `Radians`, it first attempts to convert the `Radians`
    * into a comparable value and then performs the comparison. If the conversion is not possible, it throws a `NumberException`.
    *
    * @param that the `Scalar` instance to compare the current instance against.
    * @return an integer value:
    *         - a negative value if the current instance is less than `that`.
    *         - zero if the current instance is equal to `that`.
    *         - a positive value if the current instance is greater than `that`.
    */
  def compare(that: Scalar): Int = that match {
    case number: Number =>
      compare(number)
    case radians: Radians =>
      radians.convert(this) match {
        case Some(x) =>
          compare(x)
        case None =>
          throw NumberException(s"Number.compare(Scalar): logic error: $this, $that")
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
    *         - a negative value if this `Number` is less than `that`
    *         - zero if this `Number` is equal to `that`
    *         - a positive value if this `Number` is greater than `that`
    */
  def compare(that: Number): Int =
    if isExact && that.isExact then // XXX both are exact
      FP.recover(compareExact(that))(NumberException(s"Number.compare(Number): logic error: $this, $that"))
    else if !isExact then { // XXX this is not exact
      val maybeInt: Option[Int] = for
        x <- approximation()
        y <- that.approximation()
      yield x.compare(y)
      FP.recover(maybeInt)(NumberException("Number.compare: Logic error"))
    }
    else // XXX this is exact and that is not exact
      -that.compare(this)

  /**
    * A scale factor applied to this `Number`.
    *
    * The `scaleFactor` represents a multiplier that influences computations or adjustments involving this number.
    * It is commonly used to scaleFactor or manipulate the magnitude of the number in various arithmetic or operational contexts.
    */
  lazy val scaleFactor: Double = 1.0
}

/**
  * The `Number` object provides predefined constants and implicit conversions related to numerical operations.
  * It includes representations of common numbers and utilities to work with the `Number` type.
  */
object Number {
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
  implicit def convIntToNumber(x: Int): Number = WholeNumber(x)

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
      * If the types are incompatible or cannot be converted to corresponding types, it throws a `NumberException`.
      *
      * This method is marked as `@tailrec` to optimize recursive addition operations.
      *
      * @param x the first `Number` instance
      * @param y the second `Number` instance
      * @return a `Number` instance representing the result of the addition of `x` and `y`
      * @throws NumberException if the types of `x` and `y` cannot be added
      */
    @tailrec
    def plus(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.plus(a, b)
      case (a, b: Real) =>
        plus(y, x)
      case (a: Real, b) =>
        b.convert(Real.zero).map(bAsReal => realIsRing.plus(a, bAsReal))
            .getOrElse(throw AlgebraException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: RationalNumber, b: RationalNumber) =>
        rationalNumberIsField.plus(a, b)
      case (a, b: RationalNumber) =>
        plus(y, x)
      case (a: RationalNumber, b) =>
        b.convert(RationalNumber.zero).map(bAsRational => rationalNumberIsField.plus(a, bAsRational))
            .getOrElse(throw AlgebraException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: WholeNumber, b: WholeNumber) =>
        WholeNumberIsCommutativeRing.plus(a, b)
      case (a, b: WholeNumber) =>
        plus(y, x)
      case _ =>
        throw NumberException("Number.plus: logic error: cannot add " + x + " and " + y)
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
      * @throws NumberException if the multiplication is not possible for the given inputs
      */
    @tailrec
    def times(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.times(a, b)
      case (a, b: Real) =>
        times(y, x)
      case (a: Real, b) =>
        FP.recover(b.convert(Real.zero).map(_ * a))(NumberException("Number.plus: logic error: cannot convert " + b + " to a Real"))
      case (a: RationalNumber, b: RationalNumber) =>
        a * b
      case (a, b: RationalNumber) =>
        times(y, x)
      case (a: RationalNumber, b) =>
        FP.recover(b.convert(RationalNumber.zero).map(a * _))(NumberException("Number.plus: logic error: cannot convert " + b + " to a RationalNumber"))
      case (a: WholeNumber, b: WholeNumber) =>
        a * b
      case (a, b: WholeNumber) =>
        times(y, x)
      case _ =>
        throw NumberException("Number.times: logic error: cannot multiply " + x + " and " + y)
    }

    def div(x: Number, y: Number): Number = (x, y) match {
      case (a: Real, b: Real) =>
        realIsRing.div(a, b)
      case (a: Number, b: Real) =>
        realIsRing.inverse(b) match {
          case Some(z) => times(a, z)
          case None => throw AlgebraException("Number.div: logic error: cannot divide by zero")
        }
      case _ =>
        throw AlgebraException("Number.div: logic error: cannot divide " + x + " by " + y)
    }
  }
}