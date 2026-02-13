/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.{Factor, Rational}
import com.phasmidsoftware.number.core.numerical.{ExactNumber, Fuzziness, FuzzyNumber}
import com.phasmidsoftware.number.core.{inner, numerical}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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
    * Scales the current scalar instance by a factor derived from the specified integer.
    *
    * @param x the integer factor by which to scale the scalar
    * @return a new `Scalar` instance representing the scaled value
    */
  def scale(x: Int): Scalar = scale(Rational(x))

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    */
  def negate: Monotone = scale(Rational.negOne)

  /**
    * Adds the specified `Scalar` instance to this `Scalar` and returns the result.
    * The addition operation is performed for compatible scalar types.
    * If the types are incompatible or the operation fails, a `Failure` is returned.
    *
    * @param y the `Scalar` instance to add to this instance
    * @return a `Try[Scalar]` containing the resulting `Scalar` if the addition
    *         is successful, or a `Failure` if the operation cannot be performed
    */
  def add[B <: Scalar](y: B): Try[Scalar] =
    summon[DyadicOperator[Scalar]].op[B, Scalar](addScalars)(this, y)

  /**
    * Multiplies this `Scalar` instance by another `Scalar` of type `B`.
    * The operation uses a dyadic operator to compute the product of the two scalars.
    * The result is wrapped in a `Try` to handle any potential failures during the operation.
    *
    * @param y the `Scalar` instance to multiply with this instance. Must be a subtype of `Scalar`.
    * @return a `Try[Scalar]` representing the product of the multiplication, or a failure if the operation is not possible.
    */
  def multiply[B <: Scalar](y: B): Try[Scalar] =
    summon[DyadicOperator[Scalar]].op[B, Scalar](multiplyScalars)(this, y)

  /**
    * Subtracts the given scalar from this scalar and returns the result wrapped in a `Try`.
    * The operation is performed using the dyadic operator logic powered by `DyadicOperator`,
    * with the subtraction behavior provided by `subtractScalars`.
    *
    * @param y The scalar value to subtract, which must be a subtype of `Scalar`.
    * @return A `Try[Scalar]` containing the result of the subtraction if successful,
    *         or a failure if the operation cannot be completed (e.g., type mismatch or invalid operation).
    */
  def subtract[B <: Scalar](y: B): Try[Scalar] =
    summon[DyadicOperator[Scalar]].op[B, Scalar](subtractScalars)(this, y)

  /**
    * Divides this `Scalar` instance by another `Scalar` instance of type `B`.
    * The result of the division is wrapped in a `Try` to safely handle potential failures.
    *
    * @param y The `Scalar` instance to divide by. Must be a subtype of `Scalar`.
    * @return A `Try` containing the result of the division as a `Scalar`,
    *         or a failure if the division cannot be performed.
    */
  def divide[B <: Scalar](y: B): Try[Scalar] =
    summon[DyadicOperator[Scalar]].op[B, Scalar](divideScalars)(this, y)

  private def addScalars[B <: Scalar](x: Scalar, y: B): Try[Scalar] = (x, y) match
    case (a: Number, b: Number) => a.add(b)
    case (a: Angle, b: Angle) => a.add(b)

    // Cross-type operations - convert Angle to dimensionless Number
    case (n: Number, a: Angle) =>
      FP.toTry(a.convert(Real.zero))(Failure(AlgebraException(s"unable to convert $a to Real"))).flatMap(n.add)
    case (a: Angle, n: Number) =>
      FP.toTry(a.convert(Real.zero))(Failure(AlgebraException(s"unable to convert $a to Real"))).flatMap(_.add(n))

    case _ =>
      Failure(Exception(s"Cannot add $x and $y"))

  private def multiplyScalars[B <: Scalar](x: Scalar, y: B): Try[Scalar] = (x, y) match
    case (a: Number, b: Number) =>
      a.multiply(b)
    case (a: Angle, b: Angle) =>
      a.multiply(b) // TODO remove this as it only returns a Failure

    // Cross-type operations - dimensionless scaling
    case (n: Number, a: Angle) =>
      a.multiply(n) // TODO remove this as it only returns a Failure
    case (a: Angle, n: Number) =>
      a.multiply(n) // TODO remove this as it only returns a Failure

    case _ =>
      Failure(Exception(s"Cannot multiply $x and $y"))

  private def subtractScalars[B <: Scalar](x: Scalar, y: B): Try[Scalar] = (x, y) match
    case (a: Number, b: Number) => a.subtract(b)
    case (a: Angle, b: Angle) => a.subtract(b)

    // Cross-type operations
    case (n: Number, a: Angle) =>
      FP.toTry(a.convert(Real.zero))(Failure(AlgebraException(s"unable to convert $a to Real"))).flatMap(an => n.subtract(an))
    case (a: Angle, n: Number) =>
      FP.toTry(a.convert(Real.zero))(Failure(AlgebraException(s"unable to convert $a to Real"))).flatMap(_.subtract(n))

    case _ =>
      Failure(Exception(s"Cannot subtract $y from $x"))

  private def divideScalars[B <: Scalar](x: Scalar, y: B): Try[Scalar] = (x, y) match
    case (a: Number, b: Number) => a.divide(b)
    case (a: Angle, b: Angle) => a.divide(b) // Angle / Angle => Number (dimensionless)
    case (a: Angle, n: Number) => a.divide(n) // Angle / Number => Angle (scaling)
    case (n: Number, a: Angle) =>
      FP.toTry(a.convert(Real.zero))(Failure(AlgebraException(s"unable to convert $a to Real"))).flatMap(an => n.divide(an))

    case _ =>
      Failure(Exception(s"Cannot divide $x by $y"))
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

  /**
    * LatexRenderer for Scalar (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val scalarLatexRenderer: LatexRenderer[Scalar] = LatexRenderer.instance {
    case rn: RationalNumber => rn.toLatex
    case ip: InversePower => InversePower.inversePowerLatexRenderer.toLatex(ip)
    case s => s.render // Fallback to render method
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
    * @param factor the dimensional factor associated with the scalar, such as
    *               `PureNumber`, `Radian`, or other domain-specific factors.
    * @param fuzz   an optional fuzziness component representing the uncertainty
    *               or imprecision in the numerical value.
    * @return the resulting `Scalar` based on the input values, factor, and optional fuzziness.
    *         @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if called with unsupported factors.
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
        NaturalExponential(number)
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
    *         -
    *
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if called
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
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = false

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    *         @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if called
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
    * Converts the given `Monotone` object to an optional instance of the same type.
    *
    * @param t the input object of type `T` which is a subtype of `Monotone`.
    * @return an `Option` containing a transformed instance of type `T` if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Monotone : ClassTag](t: T): Option[T] = None

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
    *         @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if called
    */
  def scale(r: Rational): Scalar = throw AlgebraException(s"NoScalar.scale: unsupported operation")

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = "Not a Scalar"

  /**
    * Determines whether this object is exact, i.e., is not any kind of approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if the value is represented exactly
    * in IEEE 754 format, or Two's complement _or_ the exact value must be represented symbolically.
    * Examples of the first type include the number one (represented here as either `WholeNumber(1)` or `Succ(NatZero)`.
    * Examples of the second type include 1.5 or the square root of two (represented here as `InversePower(2,2)`).
    *
    * When we are forced to approximate a value, for example when we want to print the decimal value of 𝛗, the Golden Ratio, 
    * then we mark such a representation as approximate, i.e., this method returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or is an approximation (`false`).
    */
  def isExact: Boolean = false
}
