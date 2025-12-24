package com.phasmidsoftware.number.algebra.core

///*
// * Copyright (c) 2023. Phasmid Software
// */
//
//package com.phasmidsoftware.number.algebra
//
//import com.phasmidsoftware.number.algebra.{RationalNumber, Structure}
//
///**
//  * Trait to define the set in which a Number has membership.
//  * Examples of sets are Z (the integers) and R (the real numbers).
//  */
//sealed trait NumberSet {
//  /**
//    * The super-set of this set, if any.
//    */
//  val maybeSuperSet: Option[NumberSet]
//
//  /**
//    * The method to be used when trying to determine if x belongs to this set.
//    *
//    * @param x a Structure object.
//    * @return true if this is exact && it's a member of all super-sets && it belongs to this set.
//    */
//  def isMember(x: Structure): Boolean = isMemberOfSuperSet(x) && belongsToSetExclusively(x)
//
//  /**
//    * (Internal) method to determine if x is a member of this set, and of no subsets.
//    *
//    * @param x a Structure object.
//    * @return true if it's a member of this set.
//    */
//  def belongsToSetExclusively(x: Structure): Boolean
//
//  /**
//    * (Internal) method to determine if x is a member of all super-sets.
//    *
//    * @param x a Structure object.
//    * @return true if it's a member of all super-sets.
//    */
//  private def isMemberOfSuperSet(x: Structure): Boolean = maybeSuperSet forall (_.isMemberOfSuperSet(x))
//}
//
///**
//  * The set of complex numbers.
//  */
//case object C extends NumberSet {
//  val maybeSuperSet: Option[NumberSet] = None
//
//  def belongsToSetExclusively(x: Structure): Boolean = ??? // TODO
//}
//
///**
//  * The set of real numbers.
//  */
//case object R extends NumberSet {
//  val maybeSuperSet: Option[NumberSet] = Some(C)
//
//  def belongsToSetExclusively(x: Structure): Boolean = ??? // TODO
//}
//
///**
//  * The set of Rational numbers.
//  */
//case object Q extends NumberSet {
//  val maybeSuperSet: Option[NumberSet] = Some(R)
//
//  def belongsToSetExclusively(x: Structure): Boolean = x.isInstanceOf[RationalNumber]
//}
//
///**
//  * The set of integers.
//  */
//case object Z extends NumberSet {
//  val maybeSuperSet: Option[NumberSet] = Some(Q)
//
//  def belongsToSetExclusively(x: Structure): Boolean = false // TODO
//}
//
///**
//  * The set of natural numbers, aka the counting numbers.
//  */
//case object N extends NumberSet {
//  val maybeSuperSet: Option[NumberSet] = Some(Z)
//
//  def belongsToSetExclusively(x: Structure): Boolean = false // TODO
//}
