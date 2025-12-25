/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
import com.phasmidsoftware.number.core.inner.{Factor, Rational}
import com.phasmidsoftware.number.core.numerical.{ExactNumber, Fuzziness, FuzzyNumber}
import com.phasmidsoftware.number.core.{inner, numerical}
import scala.reflect.ClassTag

/**
  * Represents a `Scalar`, which is a `Monotone` that is linear with other scalar quantities and
  * thus has a scale factor defined.
  * Supports various mathematical operations and properties. Scalars include both
  * exact and approximate numerical entities.
  *
  * Scalar does not support ordering because not all scalars are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Scalar` object.
  */
trait Scalar extends Monotone {

  /**
    * Compares this `Scalar` with another `Scalar` for exact equivalence.
    * This method checks if both instances can be compared exactly.
    *
    * CONSIDER moving this up into Monotone and having `that` be a `Monotone`
    *
    * @param that the `Scalar` instance to compare against
    * @return an `Option[Int]` value:
    *         - `Some(-1)` if this `Scalar` is less than `that`
    *         - `Some(0)` if this `Scalar` is equal to `that`
    *         - `Some(1)` if this `Scalar` is greater than `that`
    *         - `None` if the exact comparison is not possible
    */
  def compareExact(that: Scalar): Option[Int]

  /**
    * Represents the scaleFactor of a scalar value as a `Double`.
    * This value indicates the magnitude by which a scalar is scaled,
    * and the conversion factor to yield a `PureNumber`.
    */
  def scaleFactor: Double

  /**
    * Scales the current scalar instance by the specified rational factor.
    * TODO eliminate this method and replace it with * (below).
    *
    * @param r the `Rational` factor by which to scale the scalar
    * @return a new `Scalar` instance representing the scaled value
    */
  def scale(r: Rational): Scalar

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    */
  def negate: Monotone = scale(Rational.negOne)
}

/**
  * The `Scalar` object is a utility for creating and representing scalar values
  * with dimensional factors, exact or approximate numerical precision,
  * and optional fuzziness (uncertainty or imprecision).
  */
object Scalar {
  /**
    * Creates a `Scalar` instance based on the input `core.Number`.
    * Converts the number into an appropriate scalar representation,
    * either exact or fuzzy, depending on the properties of the input.
    *
    * CONSIDER moving this up into Monotone.
    *
    * @param x the `core.Number` to be converted into a `Scalar`.
    *          It can be an `ExactNumber` or a `FuzzyNumber`, each with specific
    *          properties such as value, factor, and optional fuzziness.
    *
    * @return the resulting `Scalar` based on the input number's properties, which
    *         encapsulates its exact value, factor, and optional fuzziness.
    */
  def apply(x: numerical.Number): Monotone = x match {
    case ExactNumber(value, factor) =>
      createScalar(value, factor, None)
    case FuzzyNumber(value, factor, fuzz) =>
      createScalar(value, factor, fuzz)
  }

  val zero: Scalar = createScalar(Right(0), inner.PureNumber, None).asInstanceOf[Scalar]

  import org.slf4j.{Logger, LoggerFactory}
  import scala.util.Try

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Scalar] = new DyadicOperator[Scalar] {
    def op[B <: Scalar, Z](f: (Scalar, B) => Try[Z])(x: Scalar, y: B): Try[Z] = (x, y) match {
      case (a: Number, b: Number) =>
        implicitly[DyadicOperator[Number]].op(f)(a, b)
      case (a: Angle, b: Angle) =>
        implicitly[DyadicOperator[Angle]].op(f)(a, b)

      // Cross-type operations:
      case (x: Number, y: Angle) =>
        tryConvertAndCompareScalar(f)(x, y.asInstanceOf[B])
      case (x: Angle, y: Number) =>
        tryConvertAndCompareScalar(f)(y, x.asInstanceOf[B])
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Scalar] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  given FuzzyEq[Scalar] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  private def tryConvertAndCompareScalar[B <: Scalar, Z](f: (Scalar, B) => Try[Z])(s: Scalar, e: B): Try[Z] =
    FP.fail(s"Scalar: unsupported cross-type operation: ${s.getClass.getSimpleName} op ${e.getClass.getSimpleName}")

  /**
    * Creates a `Scalar` instance based on the given input parameters.
    * The method evaluates the input value and optional fuzziness to determine
    * the appropriate numerical representation (e.g., `WholeNumber`, `Real`, `RationalNumber`)
    * and associates it with the provided factor (e.g., `PureNumber`, `Radian`).
    *
    * CONSIDER this should return a Scalar.
    *
    * @param value  the numerical value, which can either be a right value for exact numbers
    *               (e.g., integers, floating-point values) or a left value for rational
    *               or other representations.
    *
    * @param factor the dimensional factor associated with the scalar, such as
    *               `PureNumber`, `Radian`, or other domain-specific factors.
    *
    * @param fuzz   an optional fuzziness component representing the uncertainty
    *               or imprecision in the numerical value.
    *
    * @return the resulting `Scalar` based on the input values, factor, and optional fuzziness.
    */
  def createScalar(value: inner.Value, factor: inner.Factor, fuzz: Option[Fuzziness[Double]]): Monotone = {
    val number: Number = (value, fuzz) match {
      case (Right(x), None) =>
        WholeNumber(x)
      case (Right(x), _) =>
        Real(x, fuzz)
      case (Left(Right(x)), None) =>
        RationalNumber(x)
      case (Left(Right(x)), _) =>
        Real(x.toDouble, fuzz)
      case (Left(Left(Some(x))), _) =>
        Real(x, fuzz)
      case (Left(Left(None)), _) =>
        Real(Double.NaN, fuzz)
    }
    factor match {
      case inner.PureNumber =>
        number
      case inner.Radian =>
        Angle(number)
      case inner.NatLog =>
        NatLog(number)
      case inner.InversePower(r) =>
        r.maybeInt match {
          case Some(n) =>
            InversePower(n, number)
          case _ =>
            throw AlgebraException(s"Scalar.createScalar: unsupported inverse power $factor")
        }
      case _ =>
        // TODO implement other factors (%, and degrees)
        throw AlgebraException(s"Scalar.createScalar: unsupported factor $factor")
    }
  }
}

/**
  * `NoScalar` is a singleton object representing a special case of a `Scalar` that does not
  * conform to standard scalar operations. It serves as a placeholder or non-standard entity
  * in the scalar hierarchy, and most of its operations are unsupported or return default values.
  *
  * This object extends both the `Scalar` and `Exact` traits, adhering to their respective contracts
  * while providing specific overrides and limitations.
  */
case object NoScalar extends Scalar with Exact {
  /**
    * Retrieves an optional name associated with this instance.
    *
    * @return an `Option[String]` containing the name if present, otherwise `None`
    */
  def maybeName: Option[String] = None

  /**
    * Assigns a specified name to the `Eager` instance and returns the updated instance.
    *
    * @param name the name to assign to this `Eager` instance
    * @return the updated `Eager` instance with the specified name
    */
  def named(name: String): Eager = this

  /**
    * Normalizes this `NoScalar` instance to its simplest equivalent form.
    *
    * This operation ensures that the `NoScalar` object is in its most reduced or canonical representation.
    * If no changes are required to achieve normalization, the instance itself is returned.
    *
    * @return the normalized `Valuable` representation of this instance
    */
  def normalize: Scalar = this

  /**
    * Compares this `Scalar` with another `Scalar` for exact equivalence.
    * This method checks if both instances can be compared exactly.
    *
    * CONSIDER moving this up into Monotone and having `that` be a `Monotone`
    *
    * @param that the `Scalar` instance to compare against
    * @return an `Option[Int]` value:
    *         - `Some(-1)` if this `Scalar` is less than `that`
    *         - `Some(0)` if this `Scalar` is equal to `that`
    *         - `Some(1)` if this `Scalar` is greater than `that`
    *         - `None` if the exact comparison is not possible
    */
  def compareExact(that: Scalar): Option[Int] = throw AlgebraException(s"NoScalar.compareExact: unsupported operation")

  /**
    * Represents the scaleFactor of a scalar value as a `Double`.
    * This value indicates the magnitude by which a scalar is scaled,
    * and the conversion factor to yield a `PureNumber`.
    */
  def scaleFactor: Double = Double.NaN

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = false

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int = throw AlgebraException(s"NoScalar.signum: unsupported operation")

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor(context: Context): Option[Factor] = None

  /**
    * Converts the given `Structure` object to an optional instance of the same type.
    *
    * @param t the input object of type `T` which is a subtype of `Structure`.
    * @return an `Option` containing a transformed instance of type `T` if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = None

  /**
    * If this is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  override def maybeDouble: Option[Double] = None

  /**
    * Scales the current scalar instance by the specified rational factor.
    *
    * @param r the `Rational` factor by which to scale the scalar
    * @return a new `Scalar` instance representing the scaled value
    */
  def scale(r: Rational): Scalar = throw AlgebraException(s"NoScalar.scale: unsupported operation")

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = "Not a Scalar"
}
