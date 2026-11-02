package com.phasmidsoftware.number.algebra.util

/**
 * Case class to model the concept of factors.
 *
 * @param f a factor.
 */
case class Factor(f: Int):
  /**
   * Determine whether f is a factor of x.
   *
   * @param x a potential multiple of f.
   * @return true if f is a factor of x.
   */
  def isMultiple(x: Int): Boolean = x % f == 0

  /**
   * Unapply method for this Factor.
   *
   * @param x a candidate multiple of f.
   * @return Some(quotient) if x is a multiple of f; otherwise, None.
   */
  def unapply(x: Int): Option[Int] = 
    Option.when(isMultiple(x))(x / f)

