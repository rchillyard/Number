package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.inner.{CoreContext, Factor, PureNumber, Radian, Rational}
import com.phasmidsoftware.number.core.{ExactNumber, Fuzziness, FuzzyNumber, NumberException, inner}
import scala.reflect.ClassTag

/**
  * Represents a `Scalar`, which is a `Monotone` that is linear with other scalar quantities and
  * thus has a scale factor defined.
  * and supports various mathematical operations and properties. Scalars include both
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
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int

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
    * @return the resulting `Scalar` based on the input number's properties, which
    *         encapsulates its exact value, factor, and optional fuzziness.
    */
  def apply(x: core.Number): Monotone = x match {
    case ExactNumber(value, factor) =>
      createScalar(value, factor, None)
    case FuzzyNumber(value, factor, fuzz) =>
      createScalar(value, factor, fuzz)
  }

  /**
    * Creates a `Scalar` instance based on the given input parameters.
    * The method evaluates the input value and optional fuzziness to determine
    * the appropriate numerical representation (e.g., `WholeNumber`, `Real`, `RationalNumber`)
    * and associates it with the provided factor (e.g., `PureNumber`, `Radian`).
    *
    * @param value  the numerical value, which can either be a right value for exact numbers
    *               (e.g., integers, floating-point values) or a left value for rational
    *               or other representations.
    * @param factor the dimensional factor associated with the scalar, such as
    *               `PureNumber`, `Radian`, or other domain-specific factors.
    * @param fuzz   an optional fuzziness component representing the uncertainty
    *               or imprecision in the numerical value.
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
            throw NumberException(s"Scalar.createScalar: unsupported inverse power $factor")
        }
      case _ =>
        throw NumberException(s"Scalar.createScalar: unsupported factor $factor")
    }
  }
}

case object NoScalar extends Scalar {
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
  def compareExact(that: Scalar): Option[Int] = throw NumberException(s"NoScalar.compareExact: unsupported operation")

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
  def signum: Int = throw NumberException(s"NoScalar.signum: unsupported operation")

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor(context: ExpressionContext): Option[Factor] = None

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
  def maybeDouble: Option[Double] = None

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = "Not a Scalar"
}

/**
  * The `Radians` trait represents a scalar quantity expressed in radians, a unit of angular measure.
  * It extends the `Scalar` trait, inheriting its properties and behaviors for numerical operations
  * and comparison, while specifically associating the scalar with a conversion factor defined by Pi.
  */
trait Radians extends Scalar {

  /**
    * Retrieves an optional factor associated with the specified context.
    * The factor may represent a transformation or scaling associated with a computation
    * in a particular evaluation context.
    *
    * @param context the evaluation context within which the factor is to be determined.
    * @return an `Option` containing the factor if it exists; `None` otherwise.
    */
  def maybeFactor(context: ExpressionContext): Option[Factor] = Some(Radian)

  /**
    * Represents the scalar value for converting radians to a pure number, using Pi as the scaling factor.
    */
  val scaleFactor: Double = math.Pi // TODO change this to be an exact number (not a Double)
}
