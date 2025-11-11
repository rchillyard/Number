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
trait R {
  /**
    * Converts the current instance to a Double representation.
    * CONSIDER changing to maybeDouble returning Option[Double].
    *
    * @return the Double value corresponding to the current instance
    */
  def asDouble: Double

  /**
    * Creates an instance of `R` from the given `Rational` value.
    *
    * @param q the `Rational` value to be converted into an instance of `R`
    * @return an instance of `R` representing the specified `Rational` value
    */
  def maybeQ: Option[Q]
}

/**
  * Trait `Q` serves as a marker for entities that can be represented as a rational number.
  *
  * It provides a method to convert the implementing instance into a `Rational` representation.
  */
trait Q extends R {

  /**
    * Converts this instance of `Q` to its corresponding rational representation.
    *
    * @return a Rational instance representing the current value
    */
  def toRational: Rational

  /**
    * Retrieves an optional instance of `Z` if it exists.
    *
    * @return an `Option[Z]` that contains a `Z` instance if it's available, or `None` if not.
    */
  def maybeZ: Option[Z]
}

/**
  * Trait `Z` represents a structure that can potentially be converted to an integer representation.
  * This abstraction can be used in scenarios where values may or may not fit within the bounds of
  * a 32-bit signed integer.
  */
trait Z extends Q {
  /**
    * Converts this instance of `Z` to its corresponding int representation--if possible.
    *
    * @return an `Option[Int]` representing this value, providing that it can fit in an `Int`.
    *         If the value cannot fit in an `Int`, then the `Option` will be `None`.
    *         If the value can fit in an `Int`, then the `Option` will be `Some(Int)`.
    */
  def toInt: Int
}

/**
  * Trait `N` represents the Natural numbers, that's to say the integers from 0 to infinity.
  */
trait N extends Z

/**
  * Trait `Circle` extends the `R` trait and represents the "circle group."
  * Values are limited to the range of -pi to pi, inclusive.
  */
trait Circle extends R