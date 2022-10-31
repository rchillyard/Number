package com.phasmidsoftware.number.core

/**
  * Trait to define the behavior of things that are number-like.
  */
trait NumberLike {

  /**
    * Method to determine the NumberSet, if any, to which this NumberLike object belongs.
    *
    * @return Some(numberSet) or None if it doesn't belong to any (for example it is fuzzy).
    */
  def memberOf: Option[NumberSet] = Seq(C, R, Q, Z, N).find(set => set.isMember(this))

  /**
    * Method to determine if this NumberLike object can be evaluated exactly in the context of factor.
    *
    * @param maybeFactor the (optional) context in which we want to evaluate this Expression.
    *                    if factor is None then, the result will depend solely on whether this is exact.
    * @return true if this NumberLike object is exact in the context of factor, else false.
    */
  def isExact(maybeFactor: Option[Factor]): Boolean

  /**
    * Method to determine if this NumberLike object can be evaluated exactly in the context of no factor.
    * For instance, Number.pi is exact, although if you scaled it as a Scalar, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = isExact(None)

  /**
    * Method to determine if this NumberLike object is a member of the given set.
    *
    * @param set the candidate NumberSet.
    * @return true if this is exact and belongs to set.
    */
  def memberOf(set: NumberSet): Boolean = set.isMember(this)

  /**
    * Method to determine if this Field is actually a real Number (i.e. not complex).
    * NOTE: to force this as a Number, use convertToNumber in the companion Object.
    *
    * @return a Some(x) if this is a Number; otherwise return None.
    */
  def asNumber: Option[Number]

  /**
    * Method to render this Field in a presentable manner.
    *
    * @return a String
    */
  def render: String
}
