/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.core.inner.Rational

/**
  * Trait `R` extends `Q` and represents all Real numbers.
  *
  * It inherits the capability to be represented as a `Rational` from `Q` and adds functionality for conversion to a `Double`.
  */
trait R extends Q {
  /**
    * Converts the current instance to a Double representation.
    *
    * @return the Double value corresponding to the current instance
    */
  def asDouble: Double
}

/**
  * Trait `Q` serves as a marker for entities that can be represented as a rational number.
  *
  * It provides a method to convert the implementing instance into a `Rational` representation.
  */
trait Q extends Z {

  /**
    * Converts this instance of `Q` to its corresponding rational representation.
    *
    * @return a Rational instance representing the current value
    */
  def asRational: Rational
}

/**
  * Trait `Z` represents a structure that can potentially be converted to an integer representation.
  * This abstraction can be used in scenarios where values may or may not fit within the bounds of
  * a 32-bit signed integer.
  */
trait Z {
  /**
    * Converts this instance of `Z` to its corresponding int representation--if possible.
    *
    * @return an `Option[Int]` representing this value, providing that it can fit in an `Int`.
    *         If the value cannot fit in an `Int`, then the `Option` will be `None`.
    *         If the value can fit in an `Int`, then the `Option` will be `Some(Int)`.
    */
  def toInt: Option[Int]
}
