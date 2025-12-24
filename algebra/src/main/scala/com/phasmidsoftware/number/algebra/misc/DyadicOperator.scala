/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.misc

import scala.util.Try

/**
  * A trait that represents a dyadic operator, which performs an operation
  * involving two operands of types related to `A` and produces a result
  * wrapped in a `Try`.
  *
  * NOTE the purpose of this trait is to deal with the hierarchy of types in one place.
  * However, we have instead implemented eqv and fuzzyEqv directly (not using this trait).
  * We can implement other operators using this trait if we wish, and maybe even back-propagate
  * the eqv and fuzzyEqv methods here also.
  *
  * @tparam A The base type of the operands, which acts as the upper bound
  *           for the types used in the operation.
  */
trait DyadicOperator[A]:
  /**
    * Applies a binary operation defined by the function `f` to the operands `x` and `y`.
    * The result of the operation is returned as a `Try` to safely handle potential failures.
    *
    * @param f A function that takes two operands of types `A` and `B` and performs
    *          an operation, producing a result of type `Z` wrapped in a `Try`.
    * @param x The first operand of the operation, of type `A`.
    * @param y The second operand of the operation, of type `B`, which is constrained
    *          to be a subtype of `A`.
    * @return The result of applying the function `f` to the operands `x` and `y`,
    *         wrapped in a `Try`.
    */
  def op[B <: A, Z](f: (A, B) => Try[Z])(x: A, y: B): Try[Z]
