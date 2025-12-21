/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

import scala.util.Try

/**
  * Trait representing fuzzy equality for a type `A`.
  *
  * Fuzzy equality allows determining whether two instances of type `A`
  * are approximately equal based on a given probability `p`.
  *
  * @tparam A The type for which fuzzy equality is defined.
  */
trait DyadicOperator[A]:
  def op[B <: A, Z](f: (A, B) => Try[Z])(x: A, y: B): Try[Z]
