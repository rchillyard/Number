/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

/**
  * Type class which adds fuzzy behavior to a type X.
  *
  * @tparam X the type to be made fuzzy.
  */
trait Fuzzy[X] {

  /**
    * Method to determine if x1 and x2 can be considered the same with a probability of p.
    *
    * @param p  a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless x1 actually is x2.
    * @param x1 a value of X.
    * @param x2 a value of X.
    * @return true if t1 and t2 are considered equal with probability p.
    */
  def same(p: Double)(x1: X, x2: X): Boolean
}
