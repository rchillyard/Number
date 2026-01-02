/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * Trait `Zeroable` represents entities that are interesting in the area of zero.
  *
  * It provides methods to assess whether the value is zero and determine its sign.
  */
trait Zeroable {

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
  * A trait representing the concept of unity, providing an abstraction
  * for objects that can be tested for unity.
  *
  * Implementing classes or objects must define the behavior of the `isUnity` method.
  */
trait Unitary {

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean

}