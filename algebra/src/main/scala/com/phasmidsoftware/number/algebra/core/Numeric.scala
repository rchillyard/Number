/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

/**
  * A trait representing an abstraction for optionally holding a numeric value.
  *
  * Classes or objects implementing this trait should provide a definition 
  * of how the optional numeric value is derived or maintained.
  */
trait Numeric {

  /**
    * If this is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double]
}
