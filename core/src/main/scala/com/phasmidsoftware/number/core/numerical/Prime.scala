/*
 * Copyright (c) 2023. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.misc.FP
import com.phasmidsoftware.number.core.misc.FP.readFromResource
import com.phasmidsoftware.number.core.numerical.Divides.IntDivides
import com.phasmidsoftware.number.core.numerical.Prime.{coprime, reciprocalPeriods}
import com.phasmidsoftware.number.core.numerical.Primes.*
import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time

import java.math.BigInteger
import scala.annotation.{tailrec, unused}
import scala.collection.SortedSet
import scala.util.{Failure, Try}

/**
  * Class to represent a (possible) prime number.
  * The `isProbablePrime` method can be invoked to test if this is probably prime (to a very high certainty).
  * The validate method can be invoked to test if it is definitely prime (but it's expensive!)
  *
  * NOTE: just because we have an instance of Prime does not mean that it is a prime number.
  *
  * NOTE: Prime is no longer an AnyVal because we want to have lazy val validated.
  *
  * @param n the value of the (possible) prime number (should be greater than zero, although this is not checked)
  */
case class Prime(n: BigInt) {

  /**
    * Method to evaluate Euler's Totient function for this Prime raised to power r.
    *
    * @param r the exponent (power) by which the prime is raised.
    * @return a BigInt, which represents the number of elements less than n which are relatively prime to n.
    *
    */
  def totient(r: Int): BigInt = n.pow(r - 1) * (n - 1)

  /**
    * Method to multiply this Prime by another to yield a BigInt.
    *
    * @param o the other prime.
    * @return a BigInt whose value is the product of the value of this and the value of o.
    */
  def *(o: Prime): BigInt = n * o.n

  /**
    * Get the length of this prime in bits.
    *
    * @return the length in bits.
    */
  lazy val bits: Int = n.bitLength

  /**
    * Determine the multiplicative inverse of a, modulo n.
    *
    * NOTE: when this is a prime number, the multiplicativeInverse is modPow(a, n - 2); for non-prime moduli, we use the modInverse method on BigInt.
    *
    * @param a the value whose multiplicative inverse we require.
    * @return a number z such that z a is congruent to 1 modulo n.
    */
  def multiplicativeInverse(a: BigInt): BigInt = if (isProbablePrime) modPow(a, n - 2) else Prime.multiplicativeInverse(a, n)

  /**
    * Method to determine if this is indeed a probably prime.
    *
    * NOTE: ideally, we should be able to substitute Lucas here.
    *
    * @return the result of Prime.isProbablePrime(n).
    */
  lazy val isProbablePrime: Boolean = Prime.isProbablePrime(n)

  /**
    * This yields the value of a.pow(n-1) mod n.
    *
    * @param a a BigInt which is not divisible by n.
    * @return a BigInt which, if this is prime, should be 1 for all 0 < a < n.
    */
  def fermat(a: BigInt): BigInt = modPow(a, n - 1)

  /**
    * Method to apply the Lucas test of primality.
    *
    * TODO determine why this is so slow compared with Miller-Rabin.
    *
    * @return true if for any random value a such that 0 < a < n - 1, Lucas(n-1, factors)(a) is true where factors are the prime factors of n - 1.
    */
  lazy val Lucas: Boolean = {
    val factors = Prime.primeFactors(n - 1)
    val as: Seq[BigInt] = Prime.getRandomValues(n)
    // NOTE that as will contain duplicates. We should try to eliminate the duplicates.
    as.exists(Lucas(n - 1, factors)(_))
  }

  /**
    * Method to apply the Lucas test of primality.
    *
    * @param c  a composite number (typically one less than this prime).
    * @param qs the prime factors of c.
    * @param a  an arbitrary BigInt such that 0 < a < c.
    * @return fermat(a) && doLucas(c, qs)(a)
    */
  def Lucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = fermat(a) == 1 && doLucas(c, qs)(a)

  /**
    * NOTE: this has been known to fail for 23
    *
    * NOTE: This will yield only one of the primitive roots, the one where k = 1.
    *
    * https://homepages.math.uic.edu/~leon/mcs425-s08/handouts/PrimitiveElements.pdf
    *
    * @return
    */
  lazy val primitiveRoot: BigInt = {
    val as: Seq[BigInt] = Prime.getRandomValues(n)
    as.find(a => fermat(a) == 1 && testPrimitiveRoot(a)) match {
      case Some(a) => a
      case None => throw PrimeException(s"primitiveRoot: failed to find primitive root for $this (are you sure it's prime?)")
    }
  }

  /**
    * Method to test whether a BigInt is a primitive root of this Prime.
    *
    * @param a a BigInt: the candidate primitive root.
    * @return true if a is a primitive root of this.
    */
  def testPrimitiveRoot(a: BigInt): Boolean = toIntOption match {
    case Some(p) => (2 until p - 1).forall(j => modPow(a, j) != 1)
    case None => throw PrimeException(s"testPrimitiveRoot: this prime is too big")
  }

  /**
    * True if this is a valid prime number,
    * As this is a lazy val, it will only be evaluated at most once and, often, not at all.
    */
  lazy val validated: Boolean = validate

  /**
    * Get the remainder from the division x/n.
    *
    * @param x a BigInt.
    * @return x % n
    */
  @unused
  def remainder(x: BigInt): BigInt = x % n

  /**
    * Get the value of this prime.
    *
    * @return n.
    */
  lazy val toBigInt: BigInt = n

  /**
    * Optionally get the value of this prime as an Int.
    *
    * @return Some(n) if it fits as an Int, otherwise None.
    */
  def toIntOption: Option[Int] = if (n.abs < Int.MaxValue) Some(n.toInt) else None

  /**
    * Get the next probable prime number after this one.
    *
    * Equivalent to Prime(n.bigInteger.nextProbablePrime())
    *
    * @return a probable prime which is greater than this.
    */
  lazy val next: Prime = {
    // Lazy list of numbers greater than n, that could conceivably be primes: viz. 2, 5, and all numbers ending with 1, 3, 7, or 9.
    val ys: LazyList[BigInt] = bigInts(n + 1).filter { x =>
      x == 2 || x == 5 || {
        val r = x % 10
        r == 1 || r == 3 || r == 7 || r == 9
      }
    }

    // Lazy list of probable primes larger than n.
    val xs = ys.filter(_.isProbablePrime(40))

    // Return the first probable prime.
    Prime(xs.head)
  }

  /**
    * Method to render this Prime number as a String with a delimiter every third place.
    *
    * CONSIDER get the delimiter from the Java i18n features.
    *
    * @return a String representing a Prime number.
    */
  override lazy val toString: String = Prime.formatWithCommas(n, ",")

  /**
    * Method to yield base raised to the power exponent modulo n (i.e., this prime number).
    *
    * @param base     a BigInt representing the base number.
    * @param exponent a BigInt representing the power to which a will be raised.
    * @return the value of base.pow(exponent) mod n.
    */
  def modPow(base: BigInt, exponent: BigInt): BigInt = base.modPow(exponent, n)

  /**
    * Method to determine if this Prime is coPrime to n.
    * Recall that this class doesn't only have true prime members.
    *
    * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
    *
    * @param x a BigInt.
    * @return true or false.
    */
  def isCoprimeTo(x: BigInt): Boolean = coprime(x, n)

  /**
    * Method to get the reciprocal period for decimal expansion for this Prime.
    * NOTE as of now, this is a simple lookup.
    * If None is returned, then either this is not actually prime or it is not in the first hundred primes or in the list of reciprocal primes.
    */
  lazy val reciprocalPeriod: Option[Int] =
    for (i <- FP.optional[Int](t => t >= 0)(hundredPrimes.toList.indexOf(this)); h <- reciprocalPeriods.drop(i).headOption) yield h

  /**
    * Validate whether this number really is prime.
    *
    * NOTE: This is a very expensive operation as it essentially performs an E-sieve on the given prime.
    * NOTE: this method definitely is used.
    *
    * @return true if this number is prime.
    */
  @unused
  private lazy val validate: Boolean = isProbablePrime && Prime.primeFactors(n).forall(_ == this)

  /**
    * Return a Boolean which is true if a.pow(c/q) != 1 mod n for all q where q is a prime factor of c.
    *
    * CONSIDER renaming this to something more descriptive.
    *
    * @param c  the value that is one less than a candidate prime number (i.e. a composite number).
    * @param qs the prime factors of c.
    * @param a  an arbitrary BigInt.
    * @return true if for all q, a.pow(c/q) != 1 mod n
    */
  private def doLucas(c: BigInt, qs: Seq[Prime])(a: BigInt): Boolean = qs.forall(q => {
    val compositeFactor = c / q.n
    modPow(a, compositeFactor) != 1
  })
}

/**
  * Companion object to Prime.
  * Represents a collection of mathematical utility methods and properties related to prime numbers.
  *
  * Fields:
  * - BigOne: Represents a constant BigInt value of 1. May be used in various calculations involving primes.
  * - reciprocalPeriods: Stores data related to the reciprocal periods of numbers.
  * - carmichaelFile: Used for maintaining or accessing data related to Carmichael numbers.
  * - optionalPrime: Represents an optional instance of a Prime, used where a Prime may or may not exist.
  *
  * Methods:
  * - coprime: Determines if two BigInt values are coprime.
  * - lcm: Computes the least common multiple of two BigInt values.
  * - totient: Computes Euler's Totient function (phi) for a given BigInt or prime power.
  * - reducedTotient: Computes the reduced (or Carmichael) Totient function for a given BigInt or prime power.
  * - primeCountingFunction: Counts the number of prime numbers less than a given BigInt.
  * - primeCountingFunctionExact: Precisely counts the number of primes less than a given BigInt.
  * - multiplicativeInverse: Computes the multiplicative inverse of a BigInt modulus n.
  * - primeFactors: Returns a sequence of prime factors (with repetition) of a given BigInt.
  * - primeFactorMultiplicity: Returns a Map with prime factors and their corresponding multiplicities.
  * - formatWithCommas: Formats a BigInt value with a specified delimiter string.
  * - apply: Constructs a new Prime instance if the value is positive. Throws an exception if the value is not positive.
  * - create: Creates an optional Prime instance from a BigInt or String.
  * - createMersennePrime: Creates an optional Mersenne Prime based on the ith prime exponent.
  * - mersenneNumber: Computes a Mersenne number for a given exponent or prime.
  * - hasSmallFactor: Checks if a number has any of the smaller prime factors. Assumes the number is odd.
  * - isProbableOddPrime: Tests if an odd BigInt is a probable prime using the Miller-Rabin test.
  * - isSmallPrime: Tests if a value is one of the first one hundred primes.
  * - isProbablePrime: Tests if a given BigInt is a probable prime using the Miller-Rabin test.
  * - isCarmichaelNumber: Determines if a BigInt is a Carmichael number.
  * - carmichaelTheoremApplies: A private method related to the application of Carmichael's theorem.
  * - fill: Creates a list containing multiple copies of a given value.
  * - getRandomValues: Generates random BigInt values based on the input BigInt.
  */
object Prime {

  implicit val ordering: Ordering[Prime] = Ordering.by(_.n)

  import Divides.*

  /**
    * Method to determine if two BigInts are coprime, i.e. relatively prime.
    *
    * @param x a BigInt.
    * @param p another BigInt.
    * @return true if the gcd of x and p is 1.
    */
  def coprime(x: BigInt, p: BigInt): Boolean = p.gcd(x) == 1

  /**
    * Method to yield the least common multiple of two numbers.
    *
    * @param x a BigInt.
    * @param y a BigInt.
    * @return a BigInt that is the least common multiple of x and y.
    */
  private def lcm(x: BigInt, y: BigInt): BigInt = x * y / x.gcd(y)

  /**
    * Method to yield the value of the Euler's Totient function (phi) for this Prime.
    *
    * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
    *
    * @param x a BigInt for which we require Euler's totient function.
    * @return a BigInt, which represents the number of elements less than n which are relatively prime to x.
    */
  def totient(x: BigInt): BigInt = primeFactorMultiplicity(x).foldLeft(BigInt(1)) { (phi, factor) => phi * totient(factor) }

  /**
    * Euler's totient function for a prime power.
    *
    * @param factor a tuple of Prime and exponent.
    * @return Euler's totient function for this Prime power.
    */
  def totient(factor: (Prime, Int)): BigInt = factor match {
    case (p, r) => p.totient(r)
  }

  /**
    * Method to calculate the Carmichael (or "reduced") totient for x.
    *
    * TESTME
    *
    * @param x a BigInt for which we require Carmichael's totient function.
    * @return a BigInt, which represents the Carmichael totient function.
    */
  def reducedTotient(x: BigInt): BigInt = primeFactorMultiplicity(x).foldLeft(BigInt(1)) { (lambda, factor) =>
    factor match {
      case (p, r) => Prime.lcm(lambda, reducedTotient(p -> r))
    }
  }

  /**
    * Carmichael's totient function for a prime power.
    * NOTE: often, this will be the same as Euler's totient function, but sometimes it will be one half.
    *
    * @param factor a tuple of Prime and exponent.
    * @return Carmichael's totient function for this Prime power.
    */
  private def reducedTotient(factor: (Prime, Int)): BigInt = factor match {
    case (p, r) =>
      val phi = totient(factor)
      if (p.n == 2 && r >= 3) phi / 2 else phi
  }

  /**
    * Method to determine how many prime numbers there are which are less than x.
    *
    * @param x a BigInt.
    * @return the number of primes that are less than x.
    */
  def primeCountingFunction(x: BigInt): Int = LazyList.from(1).map(BigInt(_)).takeWhile(_ < x).count(_.isProbablePrime(30))

  /**
    * Method to determine how many prime numbers there are which are less than x.
    *
    * @param x a BigInt.
    * @return the number of primes that are less than x.
    */
  def primeCountingFunctionExact(x: BigInt): Int = LazyList.from(1).map(Prime(_)).takeWhile(_.n < x).count(_.validate)

  /**
    * Get the multiplicativeInverse for a BigInt (a) modulus n.
    *
    * NOTE: if is assumed that n is not a prime number. Assuming that n is coprime to a, then we return a.modPow(totient(n)-1), otherwise a.modInverse(n).
    *
    * @param a the number for which we need the multiplicative inverse.
    * @param n the modulus.
    * @return a number x such that ax is congruent to 1, mod n.
    */
  def multiplicativeInverse(a: BigInt, n: BigInt): BigInt = if (coprime(n, a)) a.modPow(totient(n) - 1, n) else a.modInverse(n)

  /**
    * Method to yield the prime factors (with repeated elements).
    * NOTE that if x is prime, then the list returned consists of x (not sure why but that's how it's implemented in Scala 99).
    *
    * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
    *
    * @param x        a BigInt whose prime factors are to be determined.
    * @param maybeMax an optional maximum number of prime factors to consider. Defaults to None.
    * @return a sequence of Prime objects representing the prime factors of the input BigInt.
    */
  def primeFactors(x: BigInt, maybeMax: Option[Int] = None): Seq[Prime] =
    for {
      (k, v) <- primeFactorMultiplicity(x, maybeMax).toSeq
      z <- Prime.fill(v)(k)
    } yield z

  /**
    * Computes the prime factorization of the given number `x` and returns a map of prime factors with their respective
    * multiplicities (exponents). The computation can optionally be limited to a maximum number of primes through `maybeMax`.
    *
    * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
    *
    * @param x        the number to factorize.
    * @param maybeMax an optional parameter specifying the maximum number of primes to consider during factorization.
    *                 If not provided, all primes are considered.
    * @return a map where each key is a prime factor of `x` and the corresponding value is its exponent in the prime factorization.
    */
  def primeFactorMultiplicity(x: BigInt, maybeMax: Option[Int] = None): Map[Prime, Int] = {
    /**
      * Determine how many times p divides into n.
      * CONSIDER rename x
      *
      * @param p a prime which may be a divisor of n.
      * @param x a (presumptive) composite number.
      * @return a tuple of
      */
    def factorCount(p: Prime, x: BigInt): (Int, BigInt) = {
      @tailrec
      def inner(r: (Int, BigInt)): (Int, BigInt) = r match {
        case (c, d) if p.toBigInt |> d => inner(c + 1, d / p.toBigInt)
        case _ => r
      }

      inner(0 -> x)
    }

    /**
      * Tail-recursive method to get the prime factors of a composite number with their exponents.
      *
      * This is related to (fixed) Issue #85. We really should not call this method with very large n when all we're trying
      * to do is to format a Rational nicely.
      *
      * @param result the current version of the map.
      * @param n      a composite number.
      * @param ps     a list of candidate primes.
      * @return a Map of primes with their non-zero exponents.
      */
    @tailrec
    def factorsR(result: Map[Prime, Int], n: BigInt, ps: LazyList[Prime]): Map[Prime, Int] = (n, ps) match {
      case (BigOne, _: LazyList[Prime]) => result
      case (m, h #:: t) =>
        val (count, dividend) = factorCount(h, m)
        factorsR(result + (h -> count), dividend, t)
      //  NOTE the following case originally threw an exception here: throw new Exception(s"factorsR: logic error: $m, $xs")
      case _ => result
    }

    val candidates = maybeMax match {
      case Some(m) => allPrimes.take(m)
      case None => allPrimes
    }
    factorsR(Map(), x, candidates.filter(p => p.toBigInt |> x))
  }

  /**
    * Format a BigInt with a delimiter.
    *
    * NOTE that I tried to use new java.text.DecimalFormat("#,###") but it didn't work correctly!
    *
    * @param x a BigInt.
    * @return a String
    */
  def formatWithCommas(x: BigInt, delimiter: String): String =
    x.toString().reverse.grouped(3).toList.reverse.map(x => x.reverse).mkString(delimiter)

  /**
    * Method to construct a new Prime, provided that it is positive.
    *
    * NOTE there is no check at all as to whether the result is actually a prime number.
    *
    * @param p a positive BigInt.
    * @return a Prime whose value may or may not be a Prime number.
    * @note Throws PrimeException if n is not positive.
    */
  def apply(p: BigInt): Prime = if (p > 0) new Prime(p) else throw PrimeException(s"prime must be positive ($p)")

  /**
    * Method to create a (probable) Prime from a String.
    *
    * @param s the String.
    * @return an optional Prime whose value is the String and whose value is probably a prime.
    */
  def create(s: String): Option[Prime] = create(BigInt(s))

  /**
    * Create an (optional) instance of Prime such that n is a probable prime.
    *
    * @param p a BigInt.
    * @return an Option[Prime]
    */
  def create(p: BigInt): Option[Prime] = optionalPrime(Prime(p))

  /**
    * Create an (optional) Mersenne Prime of form (2 to the power of the ith prime) - 1.
    *
    * @param i the index of the exponent of two.
    * @return an Option[Prime]
    */
  def createMersennePrime(i: Int): Option[Prime] = create(mersenneNumber(i))

  /**
    * Method to yield a Mersenne number: (2 to the power of the ith prime) - 1.
    *
    * @param i the index of the exponent of two.
    * @return a BigInt.
    */
  def mersenneNumber(i: Int): BigInt = mersenneNumber(allPrimes(i))

  /**
    * Method to yield a Mersenne number: (2 to the power of n) - 1.
    *
    * @param p the prime number to generate the Mersenne prime.
    *          NOTE that no explicit check is made to ensure that n is prime.
    * @return a BigInt.
    */
  def mersenneNumber(p: Prime): BigInt = BigInt(2).pow(p.n.toInt) - 1

  /**
    * Method to detect if x has one of the smaller factors.
    * NOTE: we don't test for odd parity because we assume that x is odd.
    *
    * @param x an odd BigInt
    * @return true if 3 or 5 or 7 divides x.
    */
  def hasSmallFactor(x: BigInt): Boolean = (3 |> x) || (5 |> x) || (7 |> x)

  /**
    * Method to determine if n is a probable prime.
    * We use the MillerRabin test on n.
    * NOTE: we assume that n is odd.
    *
    * More or less the equivalent of n.isProbablePrime(100).
    * The performance appears to be similar.
    *
    * NOTE: you may be tempted to replace carmichael.contains(p) by isCarmichaelNumber(p) but tread very carefully if you do that!
    *
    * @param p an odd BigInt.
    * @return true if n is probably prime.
    */
  def isProbableOddPrime(p: BigInt): Boolean =
    isSmallPrime(p) || (p <= 7 || !hasSmallFactor(p)) && !carmichael.contains(p) && MillerRabin.isProbablePrime(p)

  /**
    * Method to determine if p is one of the first one hundred primes.
    *
    * @param p a candidate BigInt
    * @return true if p is in hundredPrimes.
    */
  def isSmallPrime(p: BigInt): Boolean = hundredPrimes.contains(Prime(p))

  /**
    * Method to determine if n is a probable prime.
    * We use the MillerRabin test on n.
    *
    * @param p a BigInt.
    * @return true if n is probably prime.
    */
  def isProbablePrime(p: BigInt): Boolean = (p == 2 || !(2 |> p)) && isProbableOddPrime(p)

  /**
    * Test n to determine if it is a Carmichael Number.
    *
    * @param n a BigInt to be tested.
    * @return true if n is a Carmichael Number.
    */
  def isCarmichaelNumber(n: BigInt): Boolean =
    carmichael.contains(n) || carmichaelFile.contains(n) || !isSmallPrime(n) && n != 1 && !(2 |> n) && carmichaelTheoremApplies(n)

  /**
    * This is the sequence of periods of decimal expansions of reciprocals of Prime numbers, starting with 2.
    * The values for 2 and 5 (factors of 10) are 0.
    * In general, for a prime p, the period is p-1 or a factor of p-1.
    *
    * The source of this list is [[https://oeis.org/A002371]].
    *
    * @return the length of the sequence for each successive prime reciprocal.
    */
  private val reciprocalPeriods: Seq[Int] = Seq(0, 1, 0, 6, 2, 6, 16, 18, 22, 28, 15, 3, 5, 21, 46, 13, 58, 60, 33, 35, 8, 13, 41, 44, 96, 4, 34, 53, 108, 112, 42, 130, 8, 46, 148, 75, 78, 81, 166, 43, 178, 180, 95, 192, 98, 99, 30, 222, 113, 228, 232, 7, 30, 50, 256, 262, 268, 5, 69, 28, 141, 146, 153, 155, 312, 79, 110)

  /**
    * If necessary, we do a look up in the first 10,000 Carmichael numbers.
    *
    * TODO determine whether this is faster or slower than checking the rest of the conditions for isCarmichaelNumber.
    */
  private lazy val carmichaelFile: Seq[BigInt] =
    readFromResource("/carmichael.txt", wa => wa.lastOption).getOrElse(Nil)

  /**
    * Determines if Carmichael's theorem applies to the given number `n`.
    * Carmichael's theorem states that a composite number `n` is a Carmichael number
    * if it satisfies certain mathematical conditions related to its prime factors.
    *
    * @param n The number being tested, represented as a `BigInt`.
    * @return A boolean value indicating whether Carmichael's theorem applies to the input number `n`.
    */
  private def carmichaelTheoremApplies(n: BigInt) = {
    val factors: Map[Prime, Int] = primeFactorMultiplicity(n)
    val tests: Iterable[Boolean] = for ((p, r) <- factors) yield r == 1 && (n - 1) % (p.n - 1) == 0
    factors.size > 2 && tests.forall(p => p)
  }

  /**
    * XXX Adapted from Scala 99: http://aperiodic.net/phil/scala/s-99/
    *
    * @param n the number of copies to make.
    * @param x the value to be copied.
    * @tparam X the underlying type of the result.
    * @return a List[X].
    */
  private def fill[X](n: Int)(x: X): List[X] = {
    @tailrec
    def inner(r: List[X], l: Int): List[X] = if (l <= 0) r else inner(r :+ x, l - 1)

    inner(Nil, n)
  }

  /**
    * Generates a sequence of random values based on the given prime number.
    *
    * @param p A prime number used as the upper bound for generating random values.
    * @return A sequence of random `BigInt` values derived from the provided prime number.
    */
  private def getRandomValues(p: BigInt): Seq[BigInt] = {
    val pMinus1 = p - 1
    val n = 20
    if (pMinus1 < n) Range(2, p.toInt).map(BigInt(_))
    else RandomState.lazyList(System.nanoTime()).map(_.value(pMinus1 - 1) + 2) take n
  }

  /**
    * Function to lift a Prime to an Option[Prime] whose values depends on the result of checking validated.
    */
  private val optionalPrime: Prime => Option[Prime] = FP.optional[Prime](_.validated)
  private lazy val BigOne: BigInt = BigInt(1)
}

/**
  * Object providing utility methods related to prime numbers, including generating primes, 
  * checking primality, applying mathematical functions, and working with predefined sets 
  * of prime numbers and special cases such as Carmichael numbers.
  */
object Primes {

  /**
    * Method to yield the "prime counting function" aka "piApprox" for a given number.
    * The result is an approximation to the number of primes not greater than x.
    *
    * @param x the ordinal position in the list of primes of the prime number required.
    * @return an approximation to the kth prime number.
    */
  def piApprox(x: BigInt): Double = x.toDouble / math.log(x.toDouble)

  /**
    * Create a lazy list of BigInts starting with x.
    *
    * @param x the first BigInt that is required.
    * @return a LazyList[BigInt] whose head is x.
    */
  def bigInts(x: BigInt): LazyList[BigInt] = x #:: bigInts(x + 1)

  /**
    * Method to generate a random prime of size bits bits.
    *
    * @param bits the size of the resulting Prime in bits.
    * @return a Prime number with certainly 2.pow(-100).
    */
  def randomPrime(bits: Int): Prime = Prime(new BigInteger(bits, CERTAINTY, random))

  /**
    * Filters and returns a finite list of prime numbers based on a given predicate.
    * Throws a `PrimeException` if the filter does not yield a finite list of primes.
    *
    * @param f the predicate function applied to candidate primes. 
    *          Only primes for which this function returns `true` will be included in the result.
    * @return a finite list of primes that satisfy the given predicate.
    * @note Throws PrimeException if the predicate results in an infinite list of primes.
    */
  def probablePrimes(f: Prime => Boolean): List[Prime] = {
    val result = probablePrimesLazy(f)
    if (result.knownSize == -1) result.toList
    else throw PrimeException("probablyPrimes: filter does not yield finite list")
  }

  /**
    * Generates a list of prime numbers less than or equal to a given upper bound using the Sieve of Eratosthenes algorithm.
    *
    * @param m the upper bound (inclusive) for the prime numbers to be generated.
    * @return a list of prime numbers represented as `Prime` objects, where each prime is less than or equal to the given upper bound.
    */
  def eSieve(m: Int): List[Prime] = {
    val isComposite = new Array[Boolean](m + 1)
    val limit = math.sqrt(m).toInt

    for {
      p <- 2 to limit
      if !isComposite(p)
      multiple <- (p * p) to m by p
    } isComposite(multiple) = true

    isComposite.indices.drop(2)
        .filterNot(isComposite)
        .map(n => Prime(n))
        .toList
  }

  /**
    * Method to yield a lazy list of all probable primes.
    *
    * @return a LazyList[Prime].
    */
  lazy val allPrimes: LazyList[Prime] = probablePrimesLazy(_ => true)

  /**
    * Method to yield a lazy list of all probable primes smaller than x.
    *
    * @return a LazyList[Prime].
    */
  def smallPrimes(x: BigInt): LazyList[Prime] = allPrimes takeWhile {
    _.n < x
  }

  /**
    * The first Carmichael numbers, i.e numbers which satisfy Fermat's little theorem but are composite.
    */
  val carmichael: SortedSet[BigInt] =
    SortedSet(561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841, 29341, 41041, 46657, 52633, 62745, 63973, 75361, 101101, 115921, 126217, 162401, 172081, 188461, 252601, 278545, 294409, 314821, 334153, 340561, 399001, 410041, 449065, 488881, 512461).map(BigInt(_))

  /**
    * The first 100 true primes.
    */
  val hundredPrimes: SortedSet[Prime] =
    SortedSet(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281,
      283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541).map(Prime(_))

  private val prime101 = 547

  /**
    * The measure of certainty that we use.
    * Probability of a false positive prime is 2.pow(-100).
    */
  private val CERTAINTY = 100

  /**
    * Random source.
    * NOTE: this is not a secure (unpredictable) random source but it's good enough for our purposes.
    */
  private val random: java.util.Random = new java.util.Random()

  /**
    * Method to yield a lazy list of probable primes as long as they satisfy the predicate f.
    * As soon as f returns false, the lazy list terminates.
    *
    * @param f the predicate to be applied to each candidate prime.
    * @return a LazyList[Prime] where each element satisfies the predicate f.
    */
  private def probablePrimesLazy(f: Prime => Boolean): LazyList[Prime] = {
    def inner(p: Prime): LazyList[Prime] = if (f(p)) p #:: inner(p.next) else LazyList.empty

    hundredPrimes.to(LazyList).filter(f) ++ inner(Prime(prime101))
  }
}

/**
  * This code is attributed to: [[https://www.literateprograms.org/miller-rabin_primality_test__scala_.html]]
  *
  * It's not idiomatic Scala but I will clean it up as we go forward.
  */
object MillerRabin {

  /**
    * Method (originally called miller_rabin) from "literateprograms" with slight improvements for elegance.
    *
    * NOTE: this is equivalent (I believe) to n.isProbablePrime(40),
    * i.e. there's a one-in-a-trillion chance that we get false positive.
    *
    * @param n a BigInt.
    * @return true if n is a probable prime with certainty approximately 2.pow(-40)
    */
  def isProbablePrime(n: BigInt): Boolean = {
    val iterations = 20
    val random = new scala.util.Random()

    @tailrec
    def nextRandomA(): BigInt = {
      // Generate a random candidate in the range [1, n-1]
      val candidate = BigInt(n.bitLength, random) % n
      if (candidate > 0) candidate else nextRandomA()
    }

    (1 to iterations).forall { _ =>
      val a = nextRandomA()
      millerRabinPass(a, n)
    }
  }

  /**
    * Executes a Miller-Rabin primality test or generates a probable prime number based on the specified action.
    * If the action is "test", it determines whether the provided number is a prime.
    * If the action is "genprime", it generates a probable prime number with the specified bit length.
    *
    * @param action A string specifying the operation to perform. 
    *               Valid values are "test" for performing a primality test or "genprime" for prime number generation.
    *
    * @param number A string representation of the number to test or the bit length for prime generation.
    *               For the "test" action, this should be the number to be tested for primality.
    *               For the "genprime" action, this should specify the number of bits for the generated prime.
    *
    * @return A string result.
    *         If the action is "test", returns "PRIME" if the number is a probable prime or "COMPOSITE" otherwise.
    *         If the action is "genprime", returns the generated probable prime as a string.
    *         If the action is invalid, returns a message indicating the invalid action.
    */
  def millerRabinTester(action: String, number: String): String =
    if (action == "test")
      if (isProbablePrime(new BigInt(new BigInteger(number)))) "PRIME"
      else "COMPOSITE"
    else if (action == "genprime") {
      var nbits: BigInt = 0
      var p: BigInt = 0
      nbits = new BigInt(new BigInteger(number))
      var rand: java.util.Random = new java.util.Random(System.currentTimeMillis())
      p = new BigInt(new BigInteger(nbits.intValue, rand))
      while (!isProbablePrime(p) || (2 |> p) || (3 |> p) || (5 |> p) || (7 |> p)) {
        rand = new java.util.Random(System.currentTimeMillis())
        p = new BigInt(new BigInteger(nbits.intValue, rand))
      }
      p.toString()
    }
    else s"invalid action: $action"

  /**
    * Performs a single iteration of the Miller-Rabin primality test for a given base `a` and number `n`.
    * This test checks whether the number `n` is likely to be a prime.
    *
    * @param a The base used for the primality test. Must be a positive `BigInt`.
    * @param n The candidate number being tested for primality. Must be a positive `BigInt`.
    * @return `true` if the test passes, indicating `n` is likely a prime; 
    *         `false` if it fails, indicating `n` is composite.
    */
  private def millerRabinPass(a: BigInt, n: BigInt): Boolean = {
    val (d, s) = decompose(n)
    val initialPower = a.modPow(d, n)

    @tailrec
    def check(aToPower: BigInt, count: Int): Boolean = {
      if (aToPower == n - 1) true
      else if (count >= s) false
      else check((aToPower * aToPower) % n, count + 1)
    }

    if (initialPower == 1) true
    else check(initialPower, 1)
  }

  /**
    * Decomposes the given number into the form (d, s) such that n - 1 = d * 2^s, 
    * where d is an odd number and s is a non-negative integer.
    *
    * @param n The input number to be decomposed. Must be a positive `BigInt`.
    * @return A tuple `(d, s)` where `d` is an odd number and `s` is the largest 
    *         exponent such that `n - 1 = d * 2^s`.
    */
  private def decompose(n: BigInt) = {
    var d: BigInt = n - 1
    var s: Int = 0
    while (2 |> d) {
      d >>= 1
      s += 1
    }
    (d, s)
  }
}

/**
  * Object providing utility methods related to Goldbach's conjecture.
  */
object Goldbach {

  /**
    * Method to get a pair of primes which sum to a number.
    *
    * @param x an even number greater than 2.
    * @return a Try of a tuple of (p1, p2) where p1, p2 are primes such that p1 + p2 = x.
    */
  def goldbach(x: BigInt): Try[(Prime, Prime)] =
    if (x > 2 && x % 2 == 0) Try(doGoldbachEven(x))
    else Failure(new IllegalArgumentException("goldbach: input must be positive and even"))

  /**
    * Must be called with x>2 and even.
    *
    * @param x the number.
    * @return a tuple of (p1, p2) where p1, p2 are primes such that p1 + p2 = x.
    */
  private def doGoldbachEven(x: BigInt) =
    possiblePairs(x) find (_._2.isProbablePrime) match {
      case Some((p1, p2)) => p1 -> p2
      case None => throw new IllegalArgumentException
    }

  /**
    * Generates possible pairs of primes where the sum of the pair equals the input number.
    *
    * @param x a positive even number greater than 2 for which the prime pairs will be calculated.
    * @return a sequence of tuples where each tuple contains a prime and a corresponding complement prime.
    */
  private def possiblePairs(x: BigInt) = for (p <- smallPrimes(x)) yield p -> Prime(x - p.n)
}

/**
  * A custom exception class that represents errors related to prime number calculations or validations.
  *
  * @param str The message or description of the error.
  */
case class PrimeException(str: String) extends Exception(str)

/**
  * The Main object serves as the entry point of the application. It extends the `App` trait,
  * which eliminates the need for an explicit main method.
  *
  * CONSIDER eliminating this main program.
  *
  * This object previously contained functionality to benchmark the performance of the
  * Eratosthenes sieve algorithm for prime number calculation. The benchmark logic, which
  * leverages the `Benchmark` class and the `Primes.eSieve` method, is currently commented out.
  *
  * The remaining functionality outputs a message indicating the time taken by the
  * Eratosthenes sieve for a specific range of values.
  */
object Main extends App {

  //  val benchmark: Benchmark[BigInteger] = new Benchmark[BigInteger]("Eratosthenes", null, (t: BigInteger) => Primes.eSieve(t.intValue()), null)
  //  val time: Double = benchmark.run(BigInteger.valueOf(1000000), 10)
  println(s"Eratosthenes Sieve for 1000000 takes $time millisecs")
}