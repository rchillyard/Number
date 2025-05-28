package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.expression.Expression
import org.scalactic.Equality
import scala.annotation.tailrec

/**
  * Provides a trait for fuzzy equality comparisons for specific types.
  * This trait defines implicit equality behaviors for the following types:
  *
  * - `Number`: Compares `Number` instances or compares a `Number` with an `Expression`.
  * - `Real`: Compares `Real` instances or interprets `Number` as `Real` for comparison.
  * - `Field`: Compares `Field` instances.
  *
  * The custom equality logic is defined using the `Equality` type class, enabling
  * domain-specific comparison rules for each type.
  */
trait FuzzyEquality {

  /**
    * Defines an implicit equality logic for comparing `Number` instances using the `Equality` type class.
    *
    * Provides rules for determining equality between instances of `Number` and other supported types:
    *
    * - Compares two `Number` instances using the `isSame` method of the `Number` class.
    * - Compares a `Number` instance with an `Expression` by utilizing the `compare` method of the `Number` class.
    * Equality is defined when the `compare` method returns `0`.
    * - Returns `false` for any other comparison types.
    */
  implicit object NumberEquality extends Equality[Number] {

    /**
      * Determines whether a given `Number` instance is equal to another object, following specific equality rules.
      *
      * @param a the `Number` instance to compare.
      * @param b the object to compare against the `Number` instance. It can be of type `Number`, `Expression`, or any other type.
      * @return `true` if `b` is a `Number` and is the same as `a`, or if `b` is an `Expression` and compares to `a` with a result of `0`.
      *         Returns `false` for all other types or comparisons.
      */
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number =>
        a.isSame(n)
      case n: Expression =>
        a.compare(n) == 0
      case _ =>
        false
    }
  }

  /**
    * An implicit object providing equality comparison for instances of `Real`,
    * implementing the `Equality` type class.
    *
    * This object defines a custom method for evaluating equality between
    * `Real` instances and other values. The comparison logic accounts for:
    * - Equality between two `Real` instances, using the `isSame` method.
    * - Equality between a `Real` instance and a numeric value (`Number`),
    * converting the `Number` to a `Real` for comparison.
    *
    * The comparison operation uses tail recursion to ensure stack safety
    * when handling conversions and comparisons.
    */
  implicit object RealEquality extends Equality[Real] {

    /**
      * Compares an instance of `Real` with another value to determine equality.
      * This method supports equality checks between `Real` instances and other
      * numeric types by converting the numeric value into a `Real` instance.
      *
      * @param a the `Real` instance to compare.
      * @param b the value to compare against the `Real` instance. It can be either
      *          another `Real` instance or a numeric type (`Number`).
      * @return `true` if the two values are considered equal, `false` otherwise.
      */
    @tailrec
    def areEqual(a: Real, b: Any): Boolean = b match {
      case r@Real(_) =>
        a.isSame(r)
      case n: Number =>
        areEqual(a, Real(n))
    }
  }

  /**
    * Implicit object used to define an equality mechanism for comparing two `Field` instances.
    * This provides an implementation for the `Equality` type class, specifically for `Field`.
    *
    * The equality check is performed as follows:
    * - If the object `b` being compared is of type `Field`, the `isSame` method of the
    * `Field` instance `a` is used to determine equivalence.
    * - For any other type, the equality check returns false.
    */
  implicit object FieldEquality extends Equality[Field] {

    /**
      * Checks whether two objects are considered equal. Specifically, it compares
      * a `Field` instance (a) to another object (b) and determines equality.
      *
      * If the second object (b) is of type `Field`, the method relies on the `isSame`
      * method of the `Field` instance to establish equivalence. Otherwise, it returns `false`.
      *
      * @param a the `Field` instance to compare.
      * @param b the other object to be compared, which can be of any type.
      * @return true if both objects are `Field` instances and are considered equal by
      *         `a.isSame`, otherwise false.
      */
    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field =>
        a.isSame(n)
      case _ =>
        false
    }
  }
}
