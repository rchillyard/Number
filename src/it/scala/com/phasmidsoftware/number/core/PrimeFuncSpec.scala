package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Prime.createMersennePrime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.tagobjects.Slow

class PrimeFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"


  // CONSIDER removing parentheses (but I think CircleCI requires them).
  it should "check for Mersenne primes" taggedAs Slow in {
    createMersennePrime(0).isDefined shouldBe true
    createMersennePrime(1).isDefined shouldBe true
    createMersennePrime(2).isDefined shouldBe true
    createMersennePrime(3).isDefined shouldBe true
    createMersennePrime(4).isDefined shouldBe false
    createMersennePrime(5).isDefined shouldBe true
    createMersennePrime(6).isDefined shouldBe true
    createMersennePrime(7).isDefined shouldBe true
    createMersennePrime(8).isDefined shouldBe false
    createMersennePrime(9).isDefined shouldBe false
    // After here, it just takes too long to validate
    //    createMersennePrime(10).isDefined shouldBe true
    //    createMersennePrime(11).isDefined shouldBe false
    //    createMersennePrime(12).isDefined shouldBe false
    //    createMersennePrime(13).isDefined shouldBe false
    //    createMersennePrime(14).isDefined shouldBe false
    //    createMersennePrime(15).isDefined shouldBe false
    //    createMersennePrime(16).isDefined shouldBe false
    //    createMersennePrime(17).isDefined shouldBe true
  }
}
