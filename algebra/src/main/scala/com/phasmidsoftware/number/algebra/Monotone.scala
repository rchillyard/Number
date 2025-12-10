package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.Structure
import com.phasmidsoftware.number.algebra.misc.FuzzyEq
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
  * Represents a `Monotone`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Monotone extends Structure with Approximate with WithFuzziness {

  /**
    * Method to determine if this `Structure` object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `Structure` object is exact in the context of no factor, else false.
    */
  def isExact: Boolean = fuzz.isEmpty && approximation().isEmpty

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  def maybeFactor(context: Context): Option[Factor]

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
    * @return an `Option[Real]` containing the approximated value if successful, or `None` if approximation fails
    */
  def approximation(force: Boolean = false): Option[Real] = this match {
    case real: Real => Some(real)
    case _ if force => convert(Real.zero)
    case _ => None
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int
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
  * The `NumberBased` trait serves as an abstraction for entities that wrap `Number`, such as `Angle`, `NatLog`, etc.
  * These are the types that can potentially have fuzz.
  *
  * Classes or traits that extend `NumberBased` encapsulate a `Number` instance, providing a consistent interface
  * to access and work with numerical values. It is particularly useful in scenarios where mathematical operations
  * or transformations are required on numbers.
  */
trait NumberBased extends Monotone {

  /**
    * Retrieves the value associated with this `NumberBased` instance.
    *
    * This method provides access to the underlying numerical representation
    * encapsulated as a `Number` type. The value can represent exact or approximate
    * numerical quantities, facilitating mathematical operations or transformations.
    *
    * @return the `Number` associated with this instance
    */
  def number: Number
}

/**
  * Companion object for the `NumberBased` trait. It provides a `FuzzyEq` instance
  * to enable fuzzy equality comparisons for instances of `NumberBased` or its subclasses.
  *
  * Fuzzy equality allows approximate comparisons based on a specified probability threshold.
  * This is particularly useful for entities that encapsulate numeric values and may require
  * imprecise equivalence checks.
  */
object NumberBased {
  /**
    * Provides a `FuzzyEq` instance for the `NumberBased` type, enabling fuzzy equality comparisons for objects
    * that extend the `NumberBased` trait.
    *
    * This instance compares two `NumberBased` objects by:
    * - Verifying that both objects belong to the same subclass of `NumberBased`.
    * - Comparing their underlying `number` values using the `FuzzyEq` instance for `Eager`, considering the provided probability threshold.
    * - Falling back to standard equality check (`==`) if the objects are not comparable via the above steps.
    *
    * @return A `FuzzyEq[NumberBased]` instance capable of performing fuzzy equality checks on `NumberBased` objects.
    */
  given FuzzyEq[NumberBased] = FuzzyEq.instance {
    (x, y, p) =>
      (x, y) match {
        case (a: NumberBased, b: NumberBased) if a.getClass == b.getClass =>
          summon[FuzzyEq[Eager]].eqv(a.number, b.number, p)
        case _ =>
          x == y
      }
  }
}

/**
  * Represents a `Monotone`, which is one-dimensional `Structure` that can be ordered
  * and supports various mathematical operations and properties. Monotones include both
  * exact and approximate numerical entities.
  *
  * Monotone does not support ordering because not all Monotones are comparable.
  *
  * Multidimensional mathematical quantities such as Complex cannot be represented by a `Monotone` object.
  */
trait Transformed extends NumberBased {

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T]
}
