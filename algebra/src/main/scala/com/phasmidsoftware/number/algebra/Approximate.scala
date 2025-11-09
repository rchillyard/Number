package com.phasmidsoftware.number.algebra

/**
  * Trait representing a structure that supports approximation.
  *
  * The `Approximate` trait provides a contract for classes that need
  * to define how their numerical representation can be approximated
  * in some way, typically to account for uncertainty, imprecision,
  * or computational limitations.
  */
trait Approximate {

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation(force: Boolean = false): Option[Real]
}
