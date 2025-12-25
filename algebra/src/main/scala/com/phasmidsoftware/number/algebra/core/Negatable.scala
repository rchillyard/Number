/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * Represents an entity that can be negated, producing a new instance that
  * is considered to be the logical or mathematical negation of the current instance.
  * This trait is designed to work with types such as `Monotone` that have well-defined
  * negation semantics.
  */
trait Negatable[T] {

  /**
    * Returns a new instance of `T` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    */
  def negate: T
}
