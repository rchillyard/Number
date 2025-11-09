/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core

import java.math.BigInteger
import java.util.Random

/**
  * This class represents a stateful random number generator.
  *
  * @param r a random source of type java.util.Random.
  */
class RandomState(r: Random) {
  /**
    * Method to yield a random BigInt with the given number of bits.
    *
    * @param bits the number of bits desired.
    * @return a BigInt with <code>bits</code> bits.
    */
  def value(bits: Int): BigInt = new BigInteger(bits, r)

  /**
    * Method to yield a random BigInt modulo x.
    *
    * @param x a BigInt.
    * @return a random BigInt in the range 0...x-1
    */
  def value(x: BigInt): BigInt = value(x.bitLength) % x

  /**
    * Method to yield the next RandomState in the pseudo-random series.
    *
    * @return a RandomState seeded by r.nextLong().
    */
  def next: RandomState = RandomState(r.nextLong())
}

/**
  * Companion object to RandomState.
  */
object RandomState {
  /**
    * Construct a new RandomState with the given seed.
    *
    * @param seed a Long.
    * @return a RandomState.
    */
  def apply(seed: Long): RandomState = new RandomState(new Random(seed))

  /**
    * Method to generate a LazyList of RandomState instances, given the seed.
    *
    * @param seed a Long.
    * @return a LazyList[RandomState].
    */
  def lazyList(seed: Long): LazyList[RandomState] = {
    def z(r: RandomState): LazyList[RandomState] = r #:: z(r.next)

    z(apply(seed))
  }
}