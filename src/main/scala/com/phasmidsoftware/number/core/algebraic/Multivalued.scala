/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.algebraic

import com.phasmidsoftware.number.expression.Expression

/**
  * Trait representing a multivalued mathematical expression. These expressions have multiple
  * distinct solution branches, each of which can be evaluated independently.
  * Subtypes of this trait are expected to provide specific implementations for
  * solving different branches and determining the total number of branches.
  */
trait Multivalued extends (Int => Expression) {

  /**
    * Evaluates the specified branch of a multivalued mathematical expression.
    * A branch represents an independent solution or interpretation of the expression.
    *
    * @param branch the index of the solution branch to evaluate. Must be a valid branch index
    *               within the range `0` to `branches - 1` where `branches` represents the total
    *               number of available branches for this expression.
    * @return the `Expression` corresponding to the evaluated branch.
    */
  def apply(branch: Int): Expression

  /**
    * Returns the total number of distinct solution branches for the current multivalued expression.
    *
    * @return the number of solution branches as an integer
    */
  def branches: Int

}
