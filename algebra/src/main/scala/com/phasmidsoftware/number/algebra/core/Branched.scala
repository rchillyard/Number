/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.core

import scala.language.implicitConversions

/**
  * Trait representing something that can branch into multiple branches.
  *
  * @tparam T the type of the elements contained in the branches.
  */
trait Branched[T] {
  /**
    * Returns the number of branches in `this`.
    *
    * `branches` corresponds to the number of possible solutions to some equation.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int

  /**
    * Retrieves the branch at the specified index.
    *
    * @param index the index of the branch to retrieve, where the index starts from 0.
    * @return the element of type `T` at the specified branch index.
    *         If the index is out of range, the behavior is implementation-specific.
    */
  def branched(index: Int): T
}
