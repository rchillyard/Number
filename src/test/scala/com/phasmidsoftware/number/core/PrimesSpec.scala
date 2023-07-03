package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Primes.piApprox
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PrimesSpec extends AnyFlatSpec with should.Matchers {

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

  private val p1: Prime = Prime(1) // Not actually a prime number
  private val p2: Prime = Prime(2)
  private val p3: Prime = Prime(3)
  private val p5: Prime = Prime(5)
  private val p7: Prime = Prime(7)
  private val p11: Prime = Prime(11)
  private val p13: Prime = Prime(13)
  private val p17: Prime = Prime(17)
  private val p23: Prime = Prime(23)

  it should "fermat" in {
    p7.fermat(2) shouldBe 1
    Prime(71).fermat(9) shouldBe 1
  }

  it should "implement Lucas for 7 and 71" in {
    p7.Lucas shouldBe true
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
    Prime.primeFactorMultiplicity(827) shouldBe Map(Prime(827) -> 1)
    Prime.primeFactorMultiplicity(70906) shouldBe Map(Prime(2) -> 1, Prime(11) -> 2, Prime(293) -> 1)
    Prime.primeFactorMultiplicity(663168016) shouldBe Map(Prime(2) -> 4, Prime(7) -> 1, Prime(5987) -> 1, Prime(43) -> 1, Prime(23) -> 1)
    Prime.primeFactorMultiplicity(7894609062L) shouldBe Map(Prime(2) -> 1, Prime(11411) -> 1, Prime(3) -> 1, Prime(67) -> 1, Prime(1721) -> 1)
  }

  it should "implement Lucas()" in {
    val p = Prime(71)
    val pMinus1: BigInt = p.toBigInt - 1
    val factors = Prime.primeFactors(pMinus1)
    p.Lucas(pMinus1, factors)(17) shouldBe false
    p.Lucas(pMinus1, factors)(11) shouldBe true
  }

  it should "implement testPrimitiveRoot" in {
    p5.testPrimitiveRoot(2) shouldBe true
    p7.testPrimitiveRoot(2) shouldBe false
    p7.testPrimitiveRoot(3) shouldBe true
    p11.testPrimitiveRoot(2) shouldBe true
    p13.testPrimitiveRoot(2) shouldBe true
    p13.testPrimitiveRoot(6) shouldBe true
    p13.testPrimitiveRoot(7) shouldBe true
    p13.testPrimitiveRoot(11) shouldBe true
    p13.testPrimitiveRoot(3) shouldBe false
    p17.testPrimitiveRoot(5) shouldBe true
    p17.testPrimitiveRoot(7) shouldBe true
    p23.testPrimitiveRoot(2) shouldBe false
    p23.testPrimitiveRoot(3) shouldBe false
    p23.testPrimitiveRoot(5) shouldBe true
    p23.testPrimitiveRoot(7) shouldBe true
    p23.testPrimitiveRoot(10) shouldBe true
    p23.testPrimitiveRoot(15) shouldBe true
    p23.testPrimitiveRoot(17) shouldBe true
    p23.testPrimitiveRoot(20) shouldBe true
    p23.testPrimitiveRoot(21) shouldBe true
  }

  it should "implement primitiveRoot" in {
    p5.primitiveRoot shouldBe BigInt(2)
    p7.primitiveRoot shouldBe BigInt(3)
    // Why do we never get the other roots (6, 7, 11) here? Oh, duh, because it's less than 20 and so we go in sequence.
    p13.primitiveRoot shouldBe BigInt(2)
    val root23 = p23.primitiveRoot
    println(root23)
    Seq(BigInt(5), BigInt(7), BigInt(10), BigInt(11), BigInt(14), BigInt(15), BigInt(17), BigInt(19), BigInt(20), BigInt(21)) contains root23 shouldBe true
  }

  it should "multiplicativeInverse" in {
    p11.multiplicativeInverse(3) shouldBe 4
    p11.multiplicativeInverse(4) shouldBe 3
    p17.multiplicativeInverse(10) shouldBe 12
    p17.multiplicativeInverse(12) shouldBe 10
    p23.multiplicativeInverse(18) shouldBe 9
    p23.multiplicativeInverse(9) shouldBe 18
  }

  ignore should "multiplicativeInverse2" in {
    val g = 7
    val z = p17.modPow(g, 10)
    z shouldBe 2
    val y = BigInt(g).pow(10)
    y shouldBe BigInt(282475249L)
    val q = y / 17
    val r = y - q * 17
    q shouldBe BigInt(16616191L)
    r shouldBe BigInt(2)
    y.mod(17) shouldBe 2
    p17.modPow(z, 12) shouldBe g
  }

  it should "validate" in {
    Prime(2).validated shouldBe true
    Prime(4).validated shouldBe false
    p7.validated shouldBe true
    Prime(120).validated shouldBe false
    Prime(7919).validated shouldBe true
  }

  it should "next" in {
    p1.next shouldBe p2
    p2.next shouldBe p3
    p3.next shouldBe p5
    p5.next shouldBe p7
    p7.next shouldBe p11
    p11.next shouldBe p13
    p13.next shouldBe p17
    val p19 = Prime(19)
    p17.next shouldBe p19
    p19.next shouldBe p23
  }

  it should "isCarmichaelNumber1" in {
    Prime.isCarmichaelNumber(1) shouldBe false
    Prime.isCarmichaelNumber(2) shouldBe false
    Prime.isCarmichaelNumber(3) shouldBe false
    Prime.isCarmichaelNumber(41) shouldBe false
    Prime.isCarmichaelNumber(561) shouldBe true
    Prime.isCarmichaelNumber(1729) shouldBe true
  }

  it should "isCarmichaelNumber2" in {
    val tests = for (n <- Seq(561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881, 512461)) yield Prime.isCarmichaelNumber(n)
    tests.forall(p => p) shouldBe true
  }

  it should "isCarmichaelNumber3" in {
    Prime.isCarmichaelNumber(530881) shouldBe true
  }

  it should "create primes from Mersenne numbers" in {
    val xs = for (i <- Seq(2, 3, 5, 7, 13, 17, 19, 31)) yield Prime.isProbablePrime(Prime.mersenneNumber(Prime(i)))
    xs.forall(p => p) shouldBe true
  }

  it should "create Mersenne numbers" in {
    Prime.mersenneNumber(0) shouldBe 3 // 2^2 - 1
    Prime.mersenneNumber(1) shouldBe 7 // 2^3 - 1
    Prime.mersenneNumber(2) shouldBe 31 // 2^5 - 1
    Prime.mersenneNumber(3) shouldBe 127 // 2^7 - 1
    Prime.mersenneNumber(4) shouldBe 2047 // 2^11 - 1
    Prime.mersenneNumber(5) shouldBe 8191 // 2^13 - 1
    Prime.mersenneNumber(6) shouldBe 131071 // 2^17 - 1
    Prime.mersenneNumber(7) shouldBe 524287 // 2^19 - 1
    Prime.mersenneNumber(8) shouldBe 8388607
    Prime.mersenneNumber(9) shouldBe 536870911
    Prime.mersenneNumber(10) shouldBe 2147483647
    Prime.mersenneNumber(11) shouldBe 137438953471L
    Prime.mersenneNumber(12) shouldBe 2199023255551L
    Prime.mersenneNumber(13) shouldBe 8796093022207L
    Prime.mersenneNumber(14) shouldBe 140737488355327L
    Prime.mersenneNumber(15) shouldBe 9007199254740991L
    Prime.mersenneNumber(16) shouldBe 576460752303423487L
    Prime.mersenneNumber(17) shouldBe 2305843009213693951L
  }

  it should "create Mersenne prime" in {
    Prime.createMersennePrime(0) map (_.validated) shouldBe Some(true)
    Prime.createMersennePrime(1) map (_.validated) shouldBe Some(true)
    Prime.createMersennePrime(2) map (_.validated) shouldBe Some(true)
    Prime.createMersennePrime(3) map (_.validated) shouldBe Some(true)
    Prime.createMersennePrime(4) map (_.validated) shouldBe None
    Prime.createMersennePrime(5) map (_.validated) shouldBe Some(true)
    Prime.createMersennePrime(6) map (_.validated) shouldBe Some(true)
//    Prime.createMersennePrime(7) map (_.validate) shouldBe Some(true)
  }

  it should "get first 100 primes" in {
    val first100: Seq[Prime] = Primes.allPrimes.take(100).toList
    first100.last shouldBe Prime(541)
  }

  // SLOW
  it should "get first 1000 primes" in {
    val first1000: Seq[Prime] = Primes.allPrimes.take(1000).toList
    first1000.last shouldBe Prime(7919)
  }

  it should "get primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.probablePrimes(_.toBigInt < 1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  behavior of "piApprox"

  it should "be correct for specific values" in {
    // 5 (2 3 5 7 11)
    piApprox(11) shouldBe 5.0 +- 1
    // 9 (... 13 17 19 23)
    piApprox(23) shouldBe 9.0 +- 2
    // 11 (... 29 31)
    piApprox(31) shouldBe 11.0 +- 2
    // 13 (... 37 41)
    piApprox(41) shouldBe 13.0 +- 2
  }

  it should "be correct for 10^x" in {
    piApprox(100) shouldBe 25.0 +- 3.5
    piApprox(1000) shouldBe 168.0 +- 24
    piApprox(10000) shouldBe 1229.0 +- 144
  }

  behavior of "MillerRabin"

  it should "get small primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.smallPrimes(1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  it should "eSieve for primes < 1000" in {
    val lessThan1000: Seq[Prime] = Primes.eSieve(1000)
    lessThan1000.size shouldBe 168
    lessThan1000.last shouldBe Prime(997)
  }

  // SLOW
  it should "eSieve for primes < 10000" in {
    val lessThan1000: Seq[Prime] = Primes.eSieve(100000)
    lessThan1000.size shouldBe 9592
    lessThan1000.last shouldBe Prime(99991)
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
  // 1, 1, 2, 2, 4, 2, 6, 4, 6, 4,
  // 10, 4, 12, 6, 8, 8, 16, 6, 18, 8,
  // 12, 10, 22, 8, 20, 12, 18, 12, 28, 8,
  // 30, 16, 20, 16, 24, 12, 36, 18, 24, 16,
  // 40, 12, 42, 20, 24, 22, 46, 16, 42, 20,
  // 32, 24, 52, 18, 40, 24, 36, 28, 58, 16,
  // 60, 30, 36, 32, 48, 20, 66, 32, 44, 24
}
