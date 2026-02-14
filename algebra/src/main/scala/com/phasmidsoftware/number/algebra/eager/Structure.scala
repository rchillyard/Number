/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Represents a `Structure`, which is one-dimensional `Structure` that
  * supports various mathematical operations and properties.
  * Structures include both exact and approximate numerical entities.
  *
  * `Structure` does not support ordering because not all Structures are comparable.
  *
  * Multidimensional mathematical quantities like `Solution` cannot be represented by a `Structure` object.
  */
trait Structure extends Eager with WithFuzziness with Negatable[Structure] {

  /**
    * Attempts to approximate the current instance to a `Real` value.
    * If the instance is already of type `Real`, it is simply returned, wrapped inside `Some`.
    * Otherwise, depending on the value of `force`, it either attempts a conversion
    * to a default `Real` (if `force` is true), or returns `None`.
    *
    * NOTE that this method tries to keep exact quantities exact.
    *
    * @param force a boolean flag indicating whether to force the conversion to a default `Real`
    *              value when the current instance is not of type `Real`
    *
    * @return an `Option[Real]` containing the approximated value if successful, or `None` if approximation fails
    */
  def approximation(force: Boolean = false): Option[Real] = this match {
    case real: Real =>
      Some(real)
    case functional: Functional =>
      functional.convert(Real.zero)
    case _ if force =>
      convert(Real.zero)
    case _ =>
      None
  }

  /**
    * Converts the given `Structure` object to an optional instance of the same type.
    *
    * TODO refactor this method to use a type class instance of `CanConvert[T, Structure]` instead of ClassTag.
    *
    * @param t the input object of type `T` which is a subtype of `Structure`.
    * @return an `Option` containing a transformed instance of type `T` if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T]

  /**
    * Converts this `Structure` object into an optional `java.lang.Number` provided that the conversion can be
    * performed without loss of precision.
    *
    * The method determines whether the current `Structure` object can be represented as a `java.lang.Number`
    * by leveraging the `asNumber` method and further evaluating certain conditions:
    * - If the `Structure` object is an `ExactNumber` and its factor is `PureNumber`, the result
    *   is converted using `Value.asJavaNumber`.
    * - If the `Structure` object is a `Real` with a `wiggle` value below a specified tolerance,
    *   the result is also converted using `Value.asJavaNumber`.
    * - In all other cases, `None` is returned.
    *
    * @return an optional `java.lang.Number` representation of this object. The result is `Some(java.lang.Number)`
    *         if the conversion is successful under the stated conditions; otherwise, `None`.
    */
  def asJavaNumber: Option[java.lang.Number] = this match {
    case Real(value, _) =>
      Some(value)
    case s: Structure =>
      s.convert(Real.zero).flatMap(x => x.asJavaNumber)
  }

  /**
    * Performs a fuzzy equality comparison between the current `Structure` instance and another `Eager` instance.
    * The comparison is based on a specified tolerance level.
    *
    * If both instances are of type `Structure`, this method attempts to convert them to `Real`
    * and performs a fuzzy equality check with the specified tolerance. If the conversion or comparison fails,
    * a failure with an appropriate `AlgebraException` is returned.
    *
    * If either of the instances is not of type `Structure`, a failure with an `AlgebraException` is returned.
    *
    * @param p    the tolerance level (as a `Double`) to be used for the fuzzy equality comparison.
    * @param that the `Eager` instance to compare against the current instance.
    * @return a `Try[Boolean]` indicating whether the two instances are fuzzy equal within the specified tolerance.
    *         Returns a `Success(true)` or `Success(false)` if the comparison completes successfully,
    *         or a `Failure(AlgebraException)` if an error occurs.
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: Structure, b: Structure) =>
      FP.toTry(for {
        p: Real <- a.convert(Real.one)
        q: Real <- b.convert(Real.one)
        r = p ~= q
      } yield r)(Failure(AlgebraException("Structure.fuzzyEqv")))
    case _ =>
      Failure(AlgebraException(s"Structure.fuzzyEqv: unexpected input: $this and $that"))
  }

  /**
    * Adds the specified `Structure` instance to the current instance, producing a new `Structure`.
    *
    * @param y The `Structure` instance to be added.
    * @return A `Try` wrapping the resulting `Structure` instance, or a failure if the operation cannot be completed.
    */
  def add(y: Structure): Try[Structure] =
    summon[DyadicOperator[Structure]].op[Structure, Structure](addStructures)(this, y)

  /**
    * Multiplies the current `Structure` instance by the specified `Structure` operand.
    * The result of the operation is wrapped in a `Try`, allowing for safe handling of failures.
    *
    * @param y the `Structure` operand with which the current instance is multiplied
    * @return a `Try[Structure]` containing the result of the multiplication if successful,
    *         or a failure if the operation is not valid or cannot be performed
    */
  def multiply(y: Structure): Try[Structure] =
    summon[DyadicOperator[Structure]].op[Structure, Structure](multiplyStructures)(this, y)

  /**
    * Subtracts the given `Structure` instance (`y`) from the current instance.
    * The operation is performed using a dyadic operator that ensures type safety
    * and handles any failures during the subtraction via a `Try`.
    *
    * @param y the `Structure` instance to be subtracted from the current instance
    * @return a `Try[Structure]` containing the result of the subtraction if successful,
    *         or a failure if the subtraction operation cannot be performed
    */
  def subtract(y: Structure): Try[Structure] =
    summon[DyadicOperator[Structure]].op[Structure, Structure](subtractStructures)(this, y)

  /**
    * Divides the current `Structure` instance by another `Structure` instance.
    * The operation is performed using the `DyadicOperator` defined for `Structure`.
    * The result is wrapped in a `Try` to handle potential failures during the operation.
    *
    * @param y the `Structure` instance to divide the current instance by
    * @return a `Try[Structure]` containing the result of the division if successful,
    *         or a failure if the operation cannot be performed
    */
  def divide(y: Structure): Try[Structure] =
    summon[DyadicOperator[Structure]].op[Structure, Structure](divideStructures)(this, y)

  private def addStructures(x: Structure, y: Structure): Try[Structure] = (x, y) match
    case (a: Scalar, b: Scalar) => a.add(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot add Functional types: $a and $b")
    case _ => FP.fail(s"Cannot add $x and $y")

  private def multiplyStructures(x: Structure, y: Structure): Try[Structure] = (x, y) match
    case (a: Scalar, b: Scalar) => a.multiply(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot multiply Functional types: $a and $b")
    case _ => FP.fail(s"Cannot multiply $x and $y")

  private def subtractStructures(x: Structure, y: Structure): Try[Structure] = (x, y) match
    case (a: Scalar, b: Scalar) => a.subtract(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot subtract Functional types: $a and $b")
    case _ => FP.fail(s"Cannot subtract $y from $x")

  private def divideStructures(x: Structure, y: Structure): Try[Structure] = (x, y) match
    case (a: Scalar, b: Scalar) => a.divide(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot divide Functional types: $a and $b")
    case _ => FP.fail(s"Cannot divide $x by $y")

}

/**
  * The `Exact` trait represents an extension of the `WithFuzziness` trait, providing
  * functionality to describe and manage fuzziness for instances requiring precise control.
  *
  * This trait serves as a specialized version of `WithFuzziness`, with a default implementation
  * overriding the `fuzz` method to provide a `None` value by default, indicating no fuzziness.
  * It is particularly useful in cases where fuzziness is not applicable or is explicitly excluded.
  */
trait Exact extends WithFuzziness {
  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] = None
}

/**
  * The `Functional` trait serves as an abstraction for entities that wrap `Number`, such as `Angle`, `NaturalExponential`, etc.
  * These are the types that can potentially have fuzz.
  *
  * Classes or traits that extend `Functional` encapsulate a `Number` instance, providing a consistent interface
  * to access and work with numerical values. It is particularly useful in scenarios where mathematical operations
  * or transformations are required on numbers.
  */
trait Functional extends Structure with MaybeFuzzy with Ordered[Functional] {

  /**
    * Retrieves the value associated with this `Functional` instance.
    *
    * This method provides access to the underlying numerical representation
    * encapsulated as a `Number` type. The value can represent exact or approximate
    * numerical quantities, facilitating mathematical operations or transformations.
    *
    * @return the `Number` associated with this instance
    */
  def number: Number

  /**
    * This yields the scale function for this Functional.
    *
    * @return a function to transform the nominal value into the actual value as it would appear in a PureNumber context.
    */
  val scaleFunction: Double => Double

  /**
    * Represents the derivative function associated with this `Functional` instance.
    * That's to say `d(f(number))` by `d(number)` where `f` is this `Functional`.
    * For a Structure, the derivative should be positive, however, it is possible
    * that it is not positive for certain types of `Functional`.
    *
    * The `derivativeFunction` provides a mathematical operation that computes the derivative
    * with respect to a given input value. It is typically used to evaluate rates of change
    * or sensitivity in the context of numerical transformations.
    *
    * @return A function that accepts a `Double` value and returns the computed derivative as a `Double`.
    */
  val derivativeFunction: Double => Double
  
  /**
    * Retrieves an optional fuzziness value for a given number.
    *
    * @return An Option containing the fuzziness representation of the number, or None if not available.
    */
  def maybeFuzz: Option[Fuzziness[Double]] =
    number.fuzz map {
      val fuzzFunction: Double => Double = x => derivativeFunction(x) * x / scaleFunction(x)
      fuzz => fuzz.transform[Double, Double](fuzzFunction)(scaleFunction(number.toDouble))
    }

  /**
    * Retrieves the nominal (non-fuzzy) value associated with the entity.
    *
    * This value represents the precise or primary measurement or parameter of the entity,
    * without considering any associated fuzziness or uncertainty.
    *
    * @return The nominal value as a `Double`.
    */
  def nominalValue: Double = scaleFunction(number.toDouble)

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  override def maybeDouble: Option[Double] =
    Option.when(isExact)(nominalValue)
}

/**
  * Represents a `Structure`, which is a one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Structures include both
  * exact and approximate numerical entities.
  *
  * Structure does not support ordering because not all Structures are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Structure` object.
  */
trait Transformed extends Functional {

  /**
    * Defines a transformation that transforms a `Structure` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Structure` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Structure` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T]
}

/**
  * The `Structure` object serves as a companion object for the `Structure` class, providing
  * common operations, typeclass instances, and supplementary functionality.
  *
  * Key responsibilities of this object include:
  * - Defining typeclass instances for equality (`Eq`) and fuzzy equality (`FuzzyEq`) for `Structure` types.
  * - Implementing a `DyadicOperator` instance that provides a mechanism for combining 
  *   two `Structure` objects, supporting both same-type and cross-type operations.
  * - Logging and error handling during certain cross-type comparisons.
  *
  * The `Structure` class represents a hierarchy of mathematical structures, including scalars,
  * functional values, and other specific instances, and this companion object facilitates their
  * interoperation.
  */
object Structure {

  import scala.util.Try

//  private val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Structure] = new DyadicOperator[Structure] {
    def op[B <: Structure, Z](f: (Structure, B) => Try[Z])(x: Structure, y: B): Try[Z] = (x, y) match {
      case (a: Scalar, b: Scalar) =>
        implicitly[DyadicOperator[Scalar]].op(f)(a, b)
      case (a: Functional, b: Functional) =>
        implicitly[DyadicOperator[Functional]].op(f)(a, b)

      // Cross-type operations:
      case (x: Scalar, y: Functional) =>
        tryConvertAndCompareScalar(f)(x, y)

      // Default case:  
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Structure] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Structure]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Structure] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * LatexRenderer for Structure (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val monotoneLatexRenderer: LatexRenderer[Structure] = LatexRenderer.instance {
    case s: Scalar => s.toLatex
    case f: Functional => f.toLatex
    case m =>
      throw new IllegalArgumentException(s"No LaTeX renderer for Structure type: ${m.getClass.getName}")
  }

  implicit def convRationalToMonotone(r: Rational): Structure = RationalNumber(r)

  /**
    * Attempts to cast the provided `Structure` instance to the specified subtype `T`.
    * Throws an `AlgebraException` if the provided instance cannot be cast to the target type.
    *
    * @param x the input instance of `Structure` to be cast to the desired type `T`.
    * @tparam T the target subtype of `Structure` to which the input instance will be cast.
    * @return the input instance cast to the type `T` if the cast is valid and successful.
    * @note Throws [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input instance cannot be cast to the type `T`.
    */
  def asT[T <: Structure : ClassTag](x: Structure): T = {
    val clazz = summon[ClassTag[T]].runtimeClass
    if (clazz.isAssignableFrom(x.getClass))
      x.asInstanceOf[T]
    else
      throw AlgebraException(s"Logic error: Can.asT failed to cast ${x.getClass} to $clazz")
  }

  private def tryConvertAndCompareScalar[B <: Structure, Z](f: (Structure, B) => Try[Z])(s: Scalar, e: B): Try[Z] = e match {
    case n: Functional =>
      f(s, n.asInstanceOf[B])
    case _ =>
      FP.fail(s"tryConvertAndCompareScalar: unexpected Structure comparison: ${s.getClass.getSimpleName} === ${e.getClass.getSimpleName}")
  }
}

/**
  * Companion object for the `Exact` type, providing utility methods and typeclass instances.
  *
  * The `Exact` object contains a strictly defined `FuzzyEq` instance that enforces
  * exact equality. This implementation does not consider any probability thresholds
  * or fuzziness levels, ensuring comparisons are deterministic and precise.
  */
object Exact {
  /**
    * Defines a `FuzzyEq` instance for the `Exact` type.
    *
    * This implementation of fuzzy equality is strictly based on exact equality,
    * meaning that two values of type `Exact` are considered equal if and only if
    * they are precisely the same, regardless of the provided probability threshold.
    *
    * @return A `FuzzyEq[Exact]` instance where equality is determined by the default equality operator.
    */
  given FuzzyEq[Exact] = FuzzyEq.instance(exactEqual)

  @tailrec
  private def exactEqual(x: Exact, y: Exact, p: Double): Boolean = {
    (x, y) match {
      case (r: RationalNumber, w: WholeNumber) =>
        r == summon[Convertible[RationalNumber, WholeNumber]].convert(r, w)
      case (w: WholeNumber, r: RationalNumber) =>
        exactEqual(r, w, p)
      case _ if x.getClass == y.getClass =>
        x == y
      case _ =>
        false
    }
  }
}

/**
  * Companion object for the `Functional` trait. It provides a `FuzzyEq` instance
  * to enable fuzzy equality comparisons for instances of `Functional` or its subclasses.
  *
  * Fuzzy equality allows approximate comparisons based on a specified probability threshold.
  * This is particularly useful for entities that encapsulate numeric values and may require
  * imprecise equivalence checks.
  */
object Functional {

  import org.slf4j.{Logger, LoggerFactory}

  import scala.util.Try

  val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Functional] = new DyadicOperator[Functional] {
    def op[B <: Functional, Z](f: (Functional, B) => Try[Z])(x: Functional, y: B): Try[Z] = (x, y) match {
      case (a: Radians, b: Radians) =>
        implicitly[DyadicOperator[Radians]].op(f)(a, b)
      case (a: Transformed, b: Transformed) =>
        implicitly[DyadicOperator[Transformed]].op(f)(a, b)

      // Cross-type operations:
      case (x: Radians, y: Transformed) =>
        tryConvertAndCompareFunctional(f)(x, y.asInstanceOf[B])
      case (x: Transformed, y: Radians) =>
        tryConvertAndCompareFunctional(f)(y, x.asInstanceOf[B])
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Functional] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Functional]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Functional] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  // In Functional companion object
  implicit val latexRenderer: LatexRenderer[Functional] = LatexRenderer.instance {
    case ip: InversePower => ip.toLatex
    case log: Exponential => log.toLatex  // You'll need to implement this
    case f => f.render  // Or throw exception
  }

  private def tryConvertAndCompareFunctional[B <: Functional, Z](f: (Functional, B) => Try[Z])(s: Functional, e: B): Try[Z] =
    FP.fail(s"Functional: unsupported cross-type operation: ${s.getClass.getSimpleName} op ${e.getClass.getSimpleName}")
}

/**
  * The `Transformed` object provides utility methods and type class instances
  * for working with entities of type `Transformed`. It includes implementations
  * of type-safe equality and fuzzy equality, as well as the ability to handle
  * dyadic operations between instances of `Transformed` or its subtypes.
  *
  * This object defines functionality for:
  * - Performing dyadic operations using contextual `DyadicOperator` instances.
  * - Providing type class instances for equality (`Eq`) and fuzzy equality (`FuzzyEq`).
  * - Handling edge cases and unsupported cross-type operations between subtypes.
  */
object Transformed {
  val logger: Logger = LoggerFactory.getLogger(getClass)

  import scala.util.Try

  given DyadicOperator[Transformed] = new DyadicOperator[Transformed] {
    def op[B <: Transformed, Z](f: (Transformed, B) => Try[Z])(x: Transformed, y: B): Try[Z] = (x, y) match {
      case (a: Exponential, b: Exponential) =>
        implicitly[DyadicOperator[Exponential]].op(f)(a, b)
      case (a: InversePower, b: InversePower) =>
        implicitly[DyadicOperator[InversePower]].op(f)(a, b)

      // Cross-type operations:
      case (a: Exponential, b: InversePower) =>
        tryConvertAndCompareTransformed(f)(a, b.asInstanceOf[B])
      case (a: InversePower, b: Exponential) =>
        tryConvertAndCompareTransformed(f)(a, b.asInstanceOf[B])
      case (a, b) =>
        f(a, b)
    }
  }

  given Eq[Transformed] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Transformed]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Transformed] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  private def tryConvertAndCompareTransformed[B <: Transformed, Z](f: (Transformed, B) => Try[Z])(s: Transformed, e: B): Try[Z] =
    FP.fail(s"Transformed: unsupported cross-type operation: ${s.getClass.getSimpleName} op ${e.getClass.getSimpleName}")
}
