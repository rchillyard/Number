/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Represents a `Monotone`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * CONSIDER renaming as Functional? because there are some subtypes where dy/dx is not always positive.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Monotone extends Structure with WithFuzziness with Negatable[Monotone] {

  /**
    * Attempts to approximate the current instance to a `Real` value.
    * If the instance is already of type `Real`, it is simply returned, wrapped inside `Some`.
    * Otherwise, depending on the value of `force`, it either attempts a conversion
    * to a default `Real` (if `force` is true), or returns `None`.
    *
    * CONSIDER moving this up into Approximate.
    *
    * NOTE that this method tries to keep exact quantities exact.
    *
    * @param force a boolean flag indicating whether to force the conversion to a default `Real`
    *              value when the current instance is not of type `Real`
    *
    * @return an `Option[Real]` containing the approximated value if successful, or `None` if approximation fails
    */
  def approximation(force: Boolean = false): Option[Real] = this match {
    case real: Real => Some(real)
    case _ if force => convert(Real.zero)
    case _ => None
  }

  /**
    * Adds the specified `Monotone` instance to the current instance, producing a new `Monotone`.
    *
    * @param y The `Monotone` instance to be added.
    * @return A `Try` wrapping the resulting `Monotone` instance, or a failure if the operation cannot be completed.
    */
  def add(y: Monotone): Try[Monotone] =
    summon[DyadicOperator[Monotone]].op[Monotone, Monotone](addMonotones)(this, y)

  /**
    * Multiplies the current `Monotone` instance by the specified `Monotone` operand.
    * The result of the operation is wrapped in a `Try`, allowing for safe handling of failures.
    *
    * @param y the `Monotone` operand with which the current instance is multiplied
    * @return a `Try[Monotone]` containing the result of the multiplication if successful,
    *         or a failure if the operation is not valid or cannot be performed
    */
  def multiply(y: Monotone): Try[Monotone] =
    summon[DyadicOperator[Monotone]].op[Monotone, Monotone](multiplyMonotones)(this, y)

  /**
    * Subtracts the given `Monotone` instance (`y`) from the current instance.
    * The operation is performed using a dyadic operator that ensures type safety
    * and handles any failures during the subtraction via a `Try`.
    *
    * @param y the `Monotone` instance to be subtracted from the current instance
    * @return a `Try[Monotone]` containing the result of the subtraction if successful,
    *         or a failure if the subtraction operation cannot be performed
    */
  def subtract(y: Monotone): Try[Monotone] =
    summon[DyadicOperator[Monotone]].op[Monotone, Monotone](subtractMonotones)(this, y)

  /**
    * Divides the current `Monotone` instance by another `Monotone` instance.
    * The operation is performed using the `DyadicOperator` defined for `Monotone`.
    * The result is wrapped in a `Try` to handle potential failures during the operation.
    *
    * @param y the `Monotone` instance to divide the current instance by
    * @return a `Try[Monotone]` containing the result of the division if successful,
    *         or a failure if the operation cannot be performed
    */
  def divide(y: Monotone): Try[Monotone] =
    summon[DyadicOperator[Monotone]].op[Monotone, Monotone](divideMonotones)(this, y)

  private def addMonotones(x: Monotone, y: Monotone): Try[Monotone] = (x, y) match
    case (a: Scalar, b: Scalar) => a.add(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot add Functional types: $a and $b")
    case _ => FP.fail(s"Cannot add $x and $y")

  private def multiplyMonotones(x: Monotone, y: Monotone): Try[Monotone] = (x, y) match
    case (a: Scalar, b: Scalar) => a.multiply(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot multiply Functional types: $a and $b")
    case _ => FP.fail(s"Cannot multiply $x and $y")

  private def subtractMonotones(x: Monotone, y: Monotone): Try[Monotone] = (x, y) match
    case (a: Scalar, b: Scalar) => a.subtract(b)
    case (a: Functional, b: Functional) => FP.fail(s"Cannot subtract Functional types: $a and $b")
    case _ => FP.fail(s"Cannot subtract $y from $x")

  private def divideMonotones(x: Monotone, y: Monotone): Try[Monotone] = (x, y) match
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
trait Functional extends Monotone with MaybeFuzzy with Ordered[Functional] {

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
    * For a Monotone, the derivative should be positive, however, it is possible
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
  def maybeFuzz: Option[Fuzziness[Double]] = number.fuzz

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
  * Represents a `Monotone`, which is a one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Transformed extends Functional {

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T]
}

/**
  * The `Monotone` object serves as a companion object for the `Monotone` class, providing
  * common operations, typeclass instances, and supplementary functionality.
  *
  * Key responsibilities of this object include:
  * - Defining typeclass instances for equality (`Eq`) and fuzzy equality (`FuzzyEq`) for `Monotone` types.
  * - Implementing a `DyadicOperator` instance that provides a mechanism for combining 
  *   two `Monotone` objects, supporting both same-type and cross-type operations.
  * - Logging and error handling during certain cross-type comparisons.
  *
  * The `Monotone` class represents a hierarchy of mathematical structures, including scalars,
  * functional values, and other specific instances, and this companion object facilitates their
  * interoperation.
  */
object Monotone {

  import scala.util.Try

//  private val logger: Logger = LoggerFactory.getLogger(getClass)

  given DyadicOperator[Monotone] = new DyadicOperator[Monotone] {
    def op[B <: Monotone, Z](f: (Monotone, B) => Try[Z])(x: Monotone, y: B): Try[Z] = (x, y) match {
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

  given Eq[Monotone] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Monotone]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Monotone] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * LatexRenderer for Monotone (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val monotoneLatexRenderer: LatexRenderer[Monotone] = LatexRenderer.instance {
    case s: Scalar => s.toLatex
    case f: Functional => f.toLatex
    case m =>
      throw new IllegalArgumentException(s"No LaTeX renderer for Monotone type: ${m.getClass.getName}")
  }

  implicit def convRationalToMonotone(r: Rational): Monotone = RationalNumber(r)

  private def tryConvertAndCompareScalar[B <: Monotone, Z](f: (Monotone, B) => Try[Z])(s: Scalar, e: B): Try[Z] = e match {
    case n: Functional =>
      f(s, n.asInstanceOf[B])
    case _ =>
      FP.fail(s"tryConvertAndCompareScalar: unexpected Monotone comparison: ${s.getClass.getSimpleName} === ${e.getClass.getSimpleName}")
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
