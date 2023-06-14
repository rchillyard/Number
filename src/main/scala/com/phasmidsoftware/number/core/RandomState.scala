package com.phasmidsoftware.number.core

import java.math.BigInteger
import java.util.Random

class RandomState(r: Random) {
  def value(bits: Int): BigInt = new BigInteger(bits, r)

  def value(x: BigInt): BigInt = value(x.bitLength) % x

  def next: RandomState = RandomState(r.nextLong())
}

object RandomState {
  def apply(seed: Long): RandomState = new RandomState(new Random(seed))

  def lazyList(seed: Long): LazyList[RandomState] = {
    def z(r: RandomState): LazyList[RandomState] = r #:: z(r.next)

    z(apply(seed))
  }
}