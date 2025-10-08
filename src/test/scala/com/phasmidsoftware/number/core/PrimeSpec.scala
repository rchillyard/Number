package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Prime.{mersenneNumber, multiplicativeInverse}
import com.phasmidsoftware.number.core.Primes.allPrimes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimeSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Prime"

  it should "bits" in {
    Prime(7).bits shouldBe 3
    Prime(11).bits shouldBe 4
    Prime(17).bits shouldBe 5
    Prime(37).bits shouldBe 6
    Prime(71).bits shouldBe 7
    Prime(131).bits shouldBe 8
    Prime(257).bits shouldBe 9
    Prime(7919).bits shouldBe 13
  }
  it should "isProbablePrime" in {
    Prime.isProbablePrime(2) shouldBe true
    Prime.isProbablePrime(7) shouldBe true
    Prime.isProbablePrime(8) shouldBe false
    Prime.isProbablePrime(11) shouldBe true
    Prime.isProbablePrime(BigInt("35742549198872617291353508656626642567")) shouldBe true
  }
  it should "isProbableOddPrime" in {
    // It's OK to call this method on 2 (but not any other even number).
    Prime.isProbableOddPrime(2) shouldBe true
    Prime.isProbableOddPrime(7) shouldBe true
    Prime.isProbableOddPrime(11) shouldBe true
    Prime.isProbableOddPrime(7919) shouldBe true
    Prime.isProbableOddPrime(BigInt("35742549198872617291353508656626642567")) shouldBe true
  }
  /**
    * 1 (not actually a prime number)
    */
  private val p0: Prime = Prime(1) // Not actually a prime number
  /**
    * 2
    */
  private val p1: Prime = allPrimes.head
  /**
    * 3
    */
  private val p2: Prime = allPrimes(1)
  /**
    * 5
    */
  private val p3: Prime = allPrimes(2)
  /**
    * 7
    */
  private val p4: Prime = allPrimes(3)
  /**
    * 11
    */
  private val p5: Prime = allPrimes(4)
  /**
    * 13
    */
  private val p6: Prime = allPrimes(5)
  /**
    * 17
    */
  private val p7: Prime = allPrimes(6)
  /**
    * 23
    */
  private val p9: Prime = allPrimes(8)
  it should "fermat" in {
    p4.fermat(2) shouldBe 1
    Prime(71).fermat(9) shouldBe 1
  }
  it should "implement Lucas for 7 and 71" in {
    p4.Lucas shouldBe true
    Prime(71).Lucas shouldBe true
  }
  it should "implement primeFactors" in {
    Prime.primeFactors(23) shouldBe Seq(23).map(Prime(_))
    Prime.primeFactors(70) shouldBe Seq(2, 5, 7).map(Prime(_))
    Prime.primeFactors(70906) shouldBe Seq(2, 11, 11, 293).map(Prime(_))
    Prime.primeFactors(7894609062L).sorted shouldBe Seq(2, 3, 67, 1721, 11411).map(Prime(_))
  }
  it should "implement primeFactorMultiplicity" in {
    Prime.primeFactorMultiplicity(23) shouldBe Map(Prime(23) -> 1)
    Prime.primeFactorMultiplicity(70) shouldBe Map(Prime(2) -> 1, Prime(5) -> 1, Prime(7) -> 1)
    Prime.primeFactorMultiplicity(70906) shouldBe Map(Prime(2) -> 1, Prime(11) -> 2, Prime(293) -> 1)
    Prime.primeFactorMultiplicity(7894609062L) shouldBe Map(Prime(2) -> 1, Prime(11411) -> 1, Prime(3) -> 1, Prime(67) -> 1, Prime(1721) -> 1)
  }
  it should "implement Lucas()" in {
    val p = Prime(71)
    val pMinus1: BigInt = p.n - 1
    val factors = Prime.primeFactors(pMinus1)
    p.Lucas(pMinus1, factors)(17) shouldBe false
    p.Lucas(pMinus1, factors)(11) shouldBe true
  }
  it should "implement testPrimitiveRoot" in {
    p3.testPrimitiveRoot(2) shouldBe true
    p4.testPrimitiveRoot(2) shouldBe false
    p4.testPrimitiveRoot(3) shouldBe true
    p5.testPrimitiveRoot(2) shouldBe true
    p6.testPrimitiveRoot(2) shouldBe true
    p6.testPrimitiveRoot(6) shouldBe true
    p6.testPrimitiveRoot(7) shouldBe true
    p6.testPrimitiveRoot(11) shouldBe true
    p6.testPrimitiveRoot(3) shouldBe false
    p7.testPrimitiveRoot(5) shouldBe true
    p7.testPrimitiveRoot(7) shouldBe true
    p9.testPrimitiveRoot(2) shouldBe false
    p9.testPrimitiveRoot(3) shouldBe false
    p9.testPrimitiveRoot(5) shouldBe true
    p9.testPrimitiveRoot(7) shouldBe true
    p9.testPrimitiveRoot(10) shouldBe true
    p9.testPrimitiveRoot(15) shouldBe true
    p9.testPrimitiveRoot(17) shouldBe true
    p9.testPrimitiveRoot(20) shouldBe true
    p9.testPrimitiveRoot(21) shouldBe true
  }
  it should "implement primitiveRoot" in {
    p3.primitiveRoot shouldBe BigInt(2)
    p4.primitiveRoot shouldBe BigInt(3)
    // Why do we never get the other roots (6, 7, 11) here? Oh, duh, because it's less than 20 and so we go in sequence.
    p6.primitiveRoot shouldBe BigInt(2)
    val root23 = p9.primitiveRoot
    Seq(BigInt(5), BigInt(7), BigInt(10), BigInt(11), BigInt(14), BigInt(15), BigInt(17), BigInt(19), BigInt(20), BigInt(21)) contains root23 shouldBe true
  }
  it should "multiplicativeInverse" in {
    p5.multiplicativeInverse(3) shouldBe 4
    p5.multiplicativeInverse(4) shouldBe 3
    p7.multiplicativeInverse(10) shouldBe 12
    p7.multiplicativeInverse(12) shouldBe 10
    p9.multiplicativeInverse(18) shouldBe 9
    p9.multiplicativeInverse(9) shouldBe 18
  }
  it should "multiplicativeInverse1" in {
    multiplicativeInverse(3, BigInt(11)) shouldBe 4
    multiplicativeInverse(4, BigInt(11)) shouldBe 3
    multiplicativeInverse(10, BigInt(17)) shouldBe 12
    multiplicativeInverse(12, BigInt(17)) shouldBe 10
    multiplicativeInverse(18, BigInt(23)) shouldBe 9
    multiplicativeInverse(9, BigInt(23)) shouldBe 18
  }
  // NOTE this test needs double-checking.
  //  It derives originally (I think) from the Diffie-Hellman key exchange
  it should "multiplicativeInverse2" in {
    val g = 7
    val z = p7.modPow(g, 10)
    z shouldBe 2
    val y = BigInt(g).pow(10)
    y shouldBe BigInt(282475249L)
    val n = 17
    val q = y / n
    val r = y - q * n
    q shouldBe BigInt(16616191L)
    r shouldBe BigInt(2)
    y.mod(n) shouldBe 2
    p7.modPow(z, 12) shouldBe n - 1
    //Fermat's little theorem
    p7.modPow(z, n - 1) shouldBe 1
  }
  it should "validated" in {
    Prime(2).validated shouldBe true
    Prime(4).validated shouldBe false
    p4.validated shouldBe true
    Prime(120).validated shouldBe false
    Prime(7919).validated shouldBe true
  }
  it should "next" in {
    p0.next shouldBe p1
    p1.next shouldBe p2
    p2.next shouldBe p3
    p3.next shouldBe p4
    p4.next shouldBe p5
    p5.next shouldBe p6
    p6.next shouldBe p7
    val p19 = Prime(19)
    p7.next shouldBe p19
    p19.next shouldBe p9
  }
  it should "create primes from Mersenne numbers" in {
    val xs = for (i <- Seq(2, 3, 5, 7, 13, 17, 19, 31)) yield Prime.isProbablePrime(mersenneNumber(Prime(i)))
    xs.forall(_ == true) shouldBe true
  }
// 2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091, 756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917, 20996011, 24036583, 25964951, 30402457, 32582657, 37156667, 42643801, 43112609, 57885161
  it should "create (potential) Mersenne numbers" in {
    mersenneNumber(0) shouldBe 3 // 2∧2 - 1
    mersenneNumber(1) shouldBe 7 // 2∧3 - 1
    mersenneNumber(2) shouldBe 31 // 2∧5 - 1
    mersenneNumber(3) shouldBe 127 // 2∧7 - 1
    mersenneNumber(4) shouldBe 2047 // 2∧11 - 1 NOT a prime
    mersenneNumber(5) shouldBe 8191 // 2∧13 - 1
    mersenneNumber(6) shouldBe 131071 // 2∧17 - 1
    mersenneNumber(7) shouldBe 524287 // 2∧19 - 1
    mersenneNumber(8) shouldBe 8388607 // 2∧23 - 1 NOT a prime
    mersenneNumber(9) shouldBe 536870911 // 2∧29 - 1 NOT a prime
    mersenneNumber(10) shouldBe 2147483647 // 2∧31 - 1
    mersenneNumber(11) shouldBe 137438953471L // 2∧37 - 1 NOT a prime
    mersenneNumber(12) shouldBe 2199023255551L // 2∧41 - 1 NOT a prime
    mersenneNumber(13) shouldBe 8796093022207L // 2∧43 - 1 NOT a prime
    mersenneNumber(14) shouldBe 140737488355327L // 2∧47 - 1 NOT a prime
    mersenneNumber(15) shouldBe 9007199254740991L // 2∧53 - 1 NOT a prime
    mersenneNumber(16) shouldBe 576460752303423487L // 2∧59 - 1 NOT a prime
    mersenneNumber(17) shouldBe 2305843009213693951L // 2∧61 - 1
  }
  it should "get first 100 primes" in {
    val first100: Seq[Prime] = Primes.allPrimes.take(100).toList
    first100.last shouldBe Prime(541)
  }
  it should "get first 1000 primes" in {
    val first1000: Seq[Prime] = Primes.allPrimes.take(1000).toList
    first1000.last shouldBe Prime(7919)
  }
  it should "get primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.probablePrimes(_.n < 1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }
  it should "test MillerRabin" in {
    MillerRabin.millerRabinTester("test", "7919") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549881") shouldBe "PRIME"
    MillerRabin.millerRabinTester("test", "516119616549887") shouldBe "COMPOSITE"
  }
  it should "totient" in {
    Prime.totient(1) shouldBe 1
    Prime.totient(2) shouldBe 1
    Prime.totient(3) shouldBe 2
    Prime.totient(4) shouldBe 2
    Prime.totient(5) shouldBe 4
    Prime.totient(6) shouldBe 2
    Prime.totient(7) shouldBe 6
    Prime.totient(8) shouldBe 4
    Prime.totient(9) shouldBe 6
    Prime.totient(10) shouldBe 4
    Prime.totient(11) shouldBe 10
    Prime.totient(12) shouldBe 4
    Prime.totient(14) shouldBe 6
    Prime.totient(16) shouldBe 8
    Prime.totient(20) shouldBe 8
    Prime.totient(30) shouldBe 8
  }
  it should "form toString correctly" in {
    val mp17 = Prime(mersenneNumber(17))
    mp17.toString shouldBe "2,305,843,009,213,693,951"
  }
  it should "get the reciprocal period" in {
    Prime(2).reciprocalPeriod shouldBe Some(0)
    Prime(3).reciprocalPeriod shouldBe Some(1)
    Prime(5).reciprocalPeriod shouldBe Some(0)
    Prime(7).reciprocalPeriod shouldBe Some(6)
    Prime(11).reciprocalPeriod shouldBe Some(2)
    Prime(13).reciprocalPeriod shouldBe Some(6)
    Prime(17).reciprocalPeriod shouldBe Some(16)
    Prime(19).reciprocalPeriod shouldBe Some(18)
    Prime(541).reciprocalPeriod shouldBe None // CONSIDER we should get the correct number for this
    Prime(1).reciprocalPeriod shouldBe None
  }
}
