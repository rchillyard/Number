/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

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

object Modulo {
  def apply[X](using m: Modulo[X]): Modulo[X] = m

  /**
    * Normalize a value into the range [min, max] using modular arithmetic.
    * This replaces the recursive modulate function with an efficient modulo-based approach.
    *
    * @param value    the value to normalize
    * @param min      the minimum of the range (inclusive)
    * @param max      the maximum of the range (inclusive)
    * @param circular if true, min wraps to max (e.g., for angles where -π ≡ π)
    * @return the normalized value in the range [min, max]
    */
  def normalize[X](value: X, min: X, max: X, circular: Boolean)(using m: Modulo[X]): X = {
    val range = m.minus(max, min)

    // Shift value so that min becomes 0
    val shifted = m.minus(value, min)

    // Apply modulo to get into [0, range)
    val modded = m.mod(shifted, range)

    // When modded is 0 and shifted wasn't originally 0, we have an exact multiple
    // In that case, map to max instead of min to make the range [min, max] inclusive
    val adjustedModded =
      if (m.compare(modded, m.zero) == 0 && m.compare(shifted, m.zero) != 0)
        range // Map to max
      else
        modded

    // Shift back to [min, max]
    val result = m.plus(adjustedModded, min)

    // Handle circular case where min should wrap to max
    if (circular && m.compare(result, min) == 0)
      max
    else
      result
  }

  /**
    * Given instance for Rational numbers.
    * Requires the RationalFloor extension to be in scope.
    */
  given Modulo[Rational] with {

    import com.phasmidsoftware.number.core.inner.RationalFloor.*

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
}
