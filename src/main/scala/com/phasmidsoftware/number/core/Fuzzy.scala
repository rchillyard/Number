package com.phasmidsoftware.number.core

/**
  * Type class which will add fuzzy behavior to a type T.
  *
  * @tparam T the type to be made fuzzy.
  */
trait Fuzzy[T] {

  /**
    * Method to determine if t1 and t2 can be considered the same with a probability of p.
    *
    * @param p  a probability between 0 and 1 -- 0 would always result in true; 1 will result in false unless t1 actually is t2.
    * @param t1 a value of T.
    * @param t2 a value of T.
    * @return true if t1 and t2 are considered equal with probability p.
    */
  def same(p: Double)(t1: T, t2: T): Boolean
}
