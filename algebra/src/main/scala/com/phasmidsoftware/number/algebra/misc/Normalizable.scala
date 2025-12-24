/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

import com.phasmidsoftware.number.algebra.Valuable

trait Normalizable[T] {

  /**
    * Normalizes this `T` to its simplest equivalent form where T is the supertype of all possible results.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest representation of this value that is a subtype of `T`.
    */
  def normalize: T
}
