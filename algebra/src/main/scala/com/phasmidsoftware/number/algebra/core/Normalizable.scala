/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import scala.util.{Success, Try}

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

/**
  * The `Normalizable` object provides utility methods related to normalizing values
  * of types that implement the `Normalizable` trait. Normalization refers to the process
  * of simplifying an object to its most basic or canonical representation.
  */
object Normalizable {
  def tryNormalize[S <: Normalizable[T], T >: S](s: S): T = Try(s.normalize) match {
    case Success(result: T) => result
    case _ => s
  }
}