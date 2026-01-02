/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * A trait representing the concept of exactness in a value or entity.
  *
  * The `Exactitude` trait provides a contract for determining whether
  * an entity is represented exactly or as an approximation. This trait
  * can be implemented by classes or objects that wish to encapsulate
  * the notion of exactness and approximate values.
  */
trait Exactitude {

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
  def isExact: Boolean
}