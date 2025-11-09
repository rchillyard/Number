/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.misc

import scala.annotation.tailrec

/**
  * The `Factorial` object provides methods to compute the factorial of a given non-negative integer.
  * The calculation is implemented using tail recursion for improved performance and optimization.
  */
object Factorial {

  /**
    * Computes the factorial of a given non-negative integer using tail recursion.
    *
    * @param n The non-negative integer for which the factorial is to be computed.
    *          Must be a non-negative value; behavior is undefined for negative inputs.
    * @return The factorial of the input `n`, as a `BigInt`.
    */
  def apply(n: Int): BigInt = inner(1, n)

  /**
    * A tail-recursive helper function to compute the factorial of a non-negative integer.
    *
    * @param r The accumulated result of the factorial computation at the current recursion step.
    *          Initially, this should be set to 1.
    * @param n The current number for which the factorial is being computed. This value is decremented
    *          with each recursive step until it reaches 0.
    * @return The factorial of the input `n` as a `BigInt`.
    *         If `n` is 0, returns the accumulated result `r`.
    * @throws IllegalArgumentException if `n` is a negative value.
    */
  @tailrec private def inner(r: BigInt, n: Int): BigInt = n match {
    case 0 =>
      r
    case x if x > 0 =>
      inner(r * x, x - 1)
    case _ =>
      throw new IllegalArgumentException(s"Factorial of negative number $n")
  }
}
