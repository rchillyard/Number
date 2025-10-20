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
    * Returns the total number of distinct solution branches for the current multivalued expression.
    *
    * @return the number of solution branches as an integer
    */
  def branches: Int

}
