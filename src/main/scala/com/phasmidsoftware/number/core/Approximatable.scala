/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * The `Approximatable` trait provides a mechanism to compute an approximate numerical value
  * of a numerical entity.
  * It is intended for cases where the magnitude is required, but it doesn't need to be exact.
  */
trait Approximatable {

  /**
    * Computes and returns an approximate numerical value for this expression.
    *
    * @return if possible, returns a `Real` representing the approximation of this expression.
    */
  def approximation: Option[Real]
}
