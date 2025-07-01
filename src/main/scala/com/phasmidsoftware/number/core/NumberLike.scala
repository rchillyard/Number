/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.Factor

/**
  * Trait to define the behavior of things that are number-like.
  *
  * CONSIDER extending Ordered[NumberLike]
  */
trait NumberLike {

  /**
   * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
   * Unlike context, a `None` result is not permissive.
   *
   * @return an optional `Factor`.
   */
  def maybeFactor: Option[Factor]

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean

  /**
    * Method to represent this `NumberLike` object as an optional Number.
    * The result will be defined if:
    * (1) we do not lose precision and
    * (2) this object is not complex.
    *
    * NOTE: to force this as a Number, use convertToNumber in the companion Object.
    *
    * CONSIDER redefining this as Option[Field] or Option[Real].
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number]

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String

  /**
    * Method to determine the NumberSet, if any, to which this NumberLike object belongs.
    * NOTE that we don't yet support H, the quaternions.
    *
    * @return Some(numberSet) or None if it doesn't belong to any (for example it is fuzzy).
    */
  def memberOf: Option[NumberSet] =
    Seq(C, R, Q, Z, N).find(set => set.isMember(this))

  /**
    * Method to determine if this NumberLike object is a member of the given set.
    *
    * @param set the candidate NumberSet.
    * @return true if this is exact and belongs to set.
    */
  def memberOf(set: NumberSet): Boolean = set.isMember(this)
}
