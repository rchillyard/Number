/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

/**
  * Extension methods for Rational to support floor and ceiling operations.
  */
object RationalFloor {

  extension (r: Rational) {
    /**
      * Computes the floor of this Rational number.
      * The floor is the largest integer (as a Rational) that is less than or equal to this number.
      *
      * Examples:
      * - Rational(7, 2).floor = Rational(3)    // 3.5 -> 3
      * - Rational(-7, 2).floor = Rational(-4)  // -3.5 -> -4
      * - Rational(6, 2).floor = Rational(3)    // 3.0 -> 3
      *
      * @return the floor of this Rational as a Rational
      */
    def floor: Rational = {
      if (r.isWhole)
        r
      else if (r.signum >= 0)
        // Positive: truncate towards zero (same as floor)
        Rational(r.n / r.d)
      else
        // Negative: truncate towards zero, then subtract 1 to get floor
        Rational(r.n / r.d - 1)
    }

    /**
      * Computes the ceiling of this Rational number.
      * The ceiling is the smallest integer (as a Rational) that is greater than or equal to this number.
      *
      * Examples:
      * - Rational(7, 2).ceiling = Rational(4)    // 3.5 -> 4
      * - Rational(-7, 2).ceiling = Rational(-3)  // -3.5 -> -3
      * - Rational(6, 2).ceiling = Rational(3)    // 3.0 -> 3
      *
      * @return the ceiling of this Rational as a Rational
      */
    def ceiling: Rational = {
      if (r.isWhole)
        r
      else if (r.signum >= 0)
        // Positive: truncate towards zero, then add 1 to get ceiling
        Rational(r.n / r.d + 1)
      else
        // Negative: truncate towards zero (same as ceiling)
        Rational(r.n / r.d)
    }
  }
}

