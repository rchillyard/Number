/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.RationalFloor.*

/**
  * Typeclass for types that support modular arithmetic.
  * This provides operations needed to normalize values into a range using modulo.
  *
  * @tparam X the numeric type
  */
trait Modulo[X] {
  /**
    * Compute the modulo of x with respect to m.
    * The result should always be in the range [0, m) for positive m.
    *
    * @param x the value to normalize
    * @param m the modulus
    * @return x mod m
    */
  def mod(x: X, m: X): X

  /**
    * Add two values.
    */
  def plus(x: X, y: X): X

  /**
    * Subtract two values.
    */
  def minus(x: X, y: X): X

  /**
    * Compare two values.
    * Returns negative if x < y, zero if x == y, positive if x > y.
    */
  def compare(x: X, y: X): Int

  /**
    * The zero value for this numeric type.
    */
  def zero: X
}

/**
  * Typeclass that defines boundary behavior for a type when normalizing to a range.
  * This captures the intrinsic properties of how a type handles range boundaries.
  *
  * @tparam X the numeric type
  */
trait BoundaryBehavior[X] {
  /**
    * When a value lands exactly on a boundary (min or max) due to wrapping,
    * which boundary should be preferred?
    *
    * @return true if max is preferred (like angles preferring π over -π),
    *         false if min is preferred (like regular numbers preferring 0 over n)
    */
  def preferMax: Boolean

  /**
    * Are the min and max boundaries equivalent (represent the same value)?
    *
    * @return true if min ≡ max (like angles where -π ≡ π),
    *         false if they are distinct values (like regular numbers where 0 ≢ n)
    */
  def circularEquivalent: Boolean
}

object BoundaryBehavior {
  /**
    * Create a boundary behavior with specified properties.
    */
  def apply[X](prefersMax: Boolean, isCircular: Boolean): BoundaryBehavior[X] =
    new BoundaryBehavior[X] {
      def preferMax: Boolean = prefersMax

      def circularEquivalent: Boolean = isCircular
    }

  /**
    * Standard number behavior: prefer min, non-circular.
    */
  def number[X]: BoundaryBehavior[X] = apply(false, false)

  /**
    * Angle behavior: prefer max, circular equivalent.
    */
  def angle[X]: BoundaryBehavior[X] = apply(true, true)

  /**
    * Provides boundary behavior for the `Int` type when normalizing to a range.
    * Specifically, this instance models non-circular, number-like behavior:
    *
    * - `preferMax` is set to `false`, indicating that the minimum boundary
    *   is preferred when a value lands exactly on a boundary due to wrapping.
    * - `circularEquivalent` is set to `false`, indicating that the minimum
    *   and maximum boundaries are distinct and do not represent the same value.
    */
  given BoundaryBehavior[Int] with {
    def preferMax: Boolean = false

    def circularEquivalent: Boolean = false
  }
}

object Modulo {
  def apply[X](using m: Modulo[X]): Modulo[X] = m

  /**
    * Normalize a value into a range using modular arithmetic (with explicit boundary behavior).
    * This replaces the recursive modulate function with an efficient modulo-based approach.
    *
    * The behavior is controlled by:
    * - boundaryBehavior parameter: explicit boundary behavior (polarity and circular equivalence)
    * - inclusive parameter: context-dependent choice of whether both endpoints are valid
    *
    * @param value            the value to normalize
    * @param min              the minimum of the range
    * @param max              the maximum of the range
    * @param inclusive        if true, both min and max are valid; if false, max is excluded
    * @param boundaryBehavior explicit boundary behavior for this normalization
    * @return the normalized value in the appropriate range
    */
  def normalize[X](
                    value: X,
                    min: X,
                    max: X,
                    inclusive: Boolean,
                    boundaryBehavior: BoundaryBehavior[X]
                  )(using m: Modulo[X]): X = {
    // Special case: if min = max, return that value
    if (m.compare(min, max) == 0)
      return min

    val range = m.minus(max, min)

    // Check if value is already in the acceptable range
    val inRange = m.compare(value, min) >= 0 && (
      if (inclusive) m.compare(value, max) <= 0
      else m.compare(value, max) < 0
      )

    if (inRange) {
      // Value is in range
      // Only apply circular equivalence conversion if inclusive = false
      if (!inclusive && boundaryBehavior.circularEquivalent && m.compare(value, min) == 0 && boundaryBehavior.preferMax)
        return max
      else if (!inclusive && boundaryBehavior.circularEquivalent && m.compare(value, max) == 0 && !boundaryBehavior.preferMax)
        return min
      else
        return value
    }

    // Value is out of range - apply modulo
    val shifted = m.minus(value, min)
    val modded = m.mod(shifted, range)
    val result = m.plus(modded, min)

    // Handle boundary cases based on boundary behavior
    if (m.compare(result, min) == 0) {
      // Landed on min boundary
      if (boundaryBehavior.circularEquivalent && boundaryBehavior.preferMax)
        max // For angles: convert -π to π
      else
        min // For numbers: keep at min
    } else if (!inclusive && m.compare(result, max) == 0) {
      // Landed on max boundary, but max is excluded (shouldn't happen with proper modulo, but handle it)
      min
    } else {
      result
    }
  }

  /**
    * Normalize a value into a range using modular arithmetic (with implicit boundary behavior).
    * This version uses implicit resolution to find the appropriate BoundaryBehavior.
    *
    * @param value     the value to normalize
    * @param min       the minimum of the range
    * @param max       the maximum of the range
    * @param inclusive if true, both min and max are valid; if false, max is excluded
    * @return the normalized value in the appropriate range
    */
  def normalize[X](value: X, min: X, max: X, inclusive: Boolean)(using m: Modulo[X], b: BoundaryBehavior[X]): X = {
    normalize(value, min, max, inclusive, b)
  }

  /**
    * Given instance for Rational numbers.
    * Requires the RationalFloor extension to be in scope.
    */
  given Modulo[Rational] with {
    def mod(x: Rational, m: Rational): Rational = {
      // For rationals: x mod m = x - floor(x/m) * m
      // This ensures result is in [0, m)
      val quotient = x / m
      val floored = quotient.floor
      x - (floored * m)
    }

    def plus(x: Rational, y: Rational): Rational = x + y

    def minus(x: Rational, y: Rational): Rational = x - y

    def compare(x: Rational, y: Rational): Int = x.compare(y)

    def zero: Rational = Rational.zero
  }

  /**
    * Given instance for Double.
    */
  given Modulo[Double] with {
    def mod(x: Double, m: Double): Double = {
      val result = x % m
      if (result < 0) result + m else result
    }

    def plus(x: Double, y: Double): Double = x + y

    def minus(x: Double, y: Double): Double = x - y

    def compare(x: Double, y: Double): Int = x.compare(y)

    def zero: Double = 0.0
  }

  /**
    * Given instance for Int.
    */
  given Modulo[Int] with {
    def mod(x: Int, m: Int): Int = {
      val result = x % m
      if (result < 0) result + m else result
    }

    def plus(x: Int, y: Int): Int = x + y

    def minus(x: Int, y: Int): Int = x - y

    def compare(x: Int, y: Int): Int = x.compare(y)

    def zero: Int = 0
  }

  /**
    * Given instance for BigDecimal.
    */
  given Modulo[BigDecimal] with {
    def mod(x: BigDecimal, m: BigDecimal): BigDecimal = {
      val result = x % m
      if (result < 0) result + m else result
    }

    def plus(x: BigDecimal, y: BigDecimal): BigDecimal = x + y

    def minus(x: BigDecimal, y: BigDecimal): BigDecimal = x - y

    def compare(x: BigDecimal, y: BigDecimal): Int = x.compare(y)

    def zero: BigDecimal = BigDecimal(0)
  }

  object NumberBoundaryBehavior {
    /**
      * Default boundary behavior for regular numbers.
      * Prefers min (like 0 over n) and treats boundaries as distinct.
      */
    given numberBoundaryBehavior[X]: BoundaryBehavior[X] with {
      def preferMax: Boolean = false

      def circularEquivalent: Boolean = false
    }
  }

  /**
    * Boundary behavior for angles.
    * Prefers max (π over -π) and treats -π ≡ π as equivalent.
    *
    * Note: This should be explicitly imported or defined where angles are used,
    * as it overrides the default number behavior.
    */
  object AngleBoundaryBehavior {
    given angleBoundaryBehavior[X]: BoundaryBehavior[X] with {
      def preferMax: Boolean = true

      def circularEquivalent: Boolean = true
    }
  }
}
