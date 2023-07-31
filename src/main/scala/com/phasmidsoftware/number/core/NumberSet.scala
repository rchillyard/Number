/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

/**
  * Trait to define the set in which a Number has membership.
  * Examples of sets are Z (the integers) and R (the real numbers).
  */
sealed trait NumberSet {
  /**
    * The super-set of this set, if any.
    */
  val maybeSuperSet: Option[NumberSet]

  /**
    * The method to be used when trying to determine if x belongs to this set.
    *
    * @param x a NumberLike object.
    * @return true if this is exact && it's a member of all super-sets && it belongs to this set.
    */
  def isMember(x: NumberLike): Boolean = x.isExact && isMemberOfSuperSet(x) && belongsToSetExclusively(x)

  /**
    * (Internal) method to determine if x is a member of all super-sets.
    *
    * @param x a NumberLike object.
    * @return true if it's a member of all super-sets.
    */
  def isMemberOfSuperSet(x: NumberLike): Boolean = maybeSuperSet forall (_.isMemberOfSuperSet(x))

  /**
    * (Internal) method to determine if x is a member of this set, and of no subsets.
    *
    * @param x a NumberLike object.
    * @return true if it's a member of this set.
    */
  def belongsToSetExclusively(x: NumberLike): Boolean
}

/**
  * The set of complex numbers.
  */
case object C extends NumberSet {
  val maybeSuperSet: Option[NumberSet] = None

  def belongsToSetExclusively(x: NumberLike): Boolean = x.asNumber exists (x => x.normalize.isComplex)
}

/**
  * The set of real numbers.
  */
case object R extends NumberSet {
  val maybeSuperSet: Option[NumberSet] = Some(C)

  def belongsToSetExclusively(x: NumberLike): Boolean = x.asNumber exists (x => !x.normalize.isComplex && !x.scale(Scalar).isRational && !x.scale(Scalar).isInteger)
}

/**
  * The set of Rational numbers.
  */
case object Q extends NumberSet {
  val maybeSuperSet: Option[NumberSet] = Some(R)

  def belongsToSetExclusively(x: NumberLike): Boolean = x.asNumber exists (_.isRational)
}

/**
  * The set of integers.
  */
case object Z extends NumberSet {
  val maybeSuperSet: Option[NumberSet] = Some(Q)

  def belongsToSetExclusively(x: NumberLike): Boolean = x.asNumber exists (x => x.isInteger && x <= Number(0))
}

/**
  * The set of natural numbers, aka the counting numbers.
  */
case object N extends NumberSet {
  val maybeSuperSet: Option[NumberSet] = Some(Z)

  def belongsToSetExclusively(x: NumberLike): Boolean = x.asNumber flatMap (_.toBigInt) exists (x => x > BigInt(0))
}
