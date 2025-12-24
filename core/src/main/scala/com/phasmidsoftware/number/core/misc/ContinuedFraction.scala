package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.misc.ContinuedFraction.Hurwitz
import com.phasmidsoftware.number.core.numerical.CoreException
import scala.annotation.tailrec
import scala.util.Try

/**
  * This Class defines a Continued Fraction. See https://en.wikipedia.org/wiki/Continued_fraction.
  *
  * Continued fractions are an interesting way of defining irrational (or rational) numbers.
  * There are two distinct types:
  * <dl>
  * <dt>Finite</dt><dd>Gives rise to a rational number or an approximation to an irrational number</dd>
  * <dt>Infinite</dt><dd>Gives rise to an irrational number</dd>
  * </dl>
  *
  * @param cf       the ConFrac that defines this ContinuedFraction.
  * @param infinite whether this ContinuedFraction is infinite (defaults to true).
  * @param markov   the Markov constant for this particular ContinuedFraction
  *                 (defaults to Hurwitz, i.e. the square root of 5)
  */
case class ContinuedFraction(cf: ConFrac, infinite: Boolean = true, markov: Double = Hurwitz) {

  /**
    * Method to prepend a pair to the start of a continued fraction.
    *
    * @param p the coefficient to prepend.
    * @return a new ContinuedFraction with x prepended.
    */
  def +:(p: Pair): ContinuedFraction =
    ContinuedFraction(p +: cf, infinite, markov)

  /**
    * Yield a finite ContinuedFraction from this by considering only a prefix of the coefficients.
    *
    * @param n the number of coefficients (terms) to consider.
    *          If n = 0, we get the number 1, if n = 1, we get a sub 0. and so on.
    * @return a ContinuedFraction which is a prefix of this.
    */
  def prefix(n: Int): ContinuedFraction =
    ContinuedFraction(cf.take(n), infinite = false, markov)

  /**
    * Method to get the coefficient pairs.
    *
    * @see ConFrac#coefficients.
    * @return a LazyList of Pairs.
    */
  def coefficients: LazyList[Pair] =
    cf.coefficients

  /**
    * Method to get the convergents.
    *
    * @see ConFrac#convergents.
    * @return a LazyList of Rationals.
    */
  def convergents: LazyList[Rational] =
    cf.convergents

  /**
    * Method to get the coefficients in reverse order, providing that this ContinuedFraction is not infinite.
    *
    * NOTE: this is never currently used.
    *
    * @return the coefficients in reverse order
    * @throws ConFracException if cf is infinite.
    */
  def reverseCoefficients: LazyList[Pair] =
    if (!infinite)
      cf.reverseCoefficients
    else
      throw ConFracException("cannot get coefficients for infinite ConFrac")

  /**
    * Method to yield a Rational from the first n elements of this ContinuedFraction.
    *
    * @param n the number of elements (coefficient pairs) to use in the evaluation.
    * @return a Rational.
    */
  def toRational(n: Int): Rational = cf.take(n).toRational

  /**
    * Method to approximate this ContinuedFraction according to the value of epsilon.
    * The result should be close to the value of the represented irrational by less than 1/d/d/markov.
    *
    * @param epsilon a small positive number.
    * @return an optional Rational approximation to the value of the infinite series.
    */
  def toRational(epsilon: Double): Option[Rational] = cf.toRational(epsilon)(markov)

  /**
    * Method to approximate this ContinuedFraction according to the value of epsilon.
    * The result should be close to the value of the represented irrational by less than 1/d/d/markov.
    *
    * @param epsilon a small positive number.
    * @return an optional Double approximation to the value of the infinite series.
    */
  def toDouble(epsilon: Double): Option[Double] = cf.toDouble(epsilon, markov)
}

/**
  * This object represents utility methods and predefined constants for working
  * with continued fractions. It provides functionality to construct, transform,
  * and manipulate ContinuedFraction instances.
  */
object ContinuedFraction {
  /**
    * Construct a ContinuedFraction based on a LazyList of Pairs.
    *
    * @param ps       a lazy list of Pairs.
    * @param infinite true if xs is infinite (defaults to true).
    * @param markov   the value of Markov constant (defaults to Hurwitz).
    * @return a ContinuedFraction
    */
  def apply(ps: LazyList[Pair], infinite: Boolean, markov: Double): ContinuedFraction =
    ContinuedFraction(ConFrac(ps), infinite, markov)

  /**
    * Construct a ContinuedFraction based on a function.
    *
    * CONSIDER changing the f to be Int => Pair.
    *
    * @param f        a function which yields a LazyList of Pairs..
    * @param infinite true if xs is infinite (defaults to true).
    * @param markov   the value of Markov constant (defaults to Hurwitz).
    * @return a ContinuedFraction
    */
  def apply(f: () => LazyList[Pair], infinite: Boolean, markov: Double): ContinuedFraction =
    ContinuedFraction(f(), infinite, markov)

  /**
    * The Markov constant for all irrational numbers.
    * Many individual irrationals can use a smaller value for their Markov number.
    * See https://en.wikipedia.org/wiki/Hurwitz%27s_theorem_(number_theory)
    * See https://en.wikipedia.org/wiki/Markov_constant
    */
  val Hurwitz: Double = math.sqrt(5)

  /**
    * Construct a ContinuedFraction based on an initial pair sequence and a transforming function.
    *
    * @param sequence a sequence of Pairs which will be form the initial pattern.
    * @param f        a function which repeatedly transforms the current sequence.
    * @return a ContinuedFraction.
    */
  def createInfinite(sequence: List[Pair], f: List[Pair] => List[Pair]): ContinuedFraction =
    createInfiniteWithMarkov(sequence, f, Hurwitz)

  /**
    * Construct a ContinuedFraction based on an initial pattern and a transforming function.
    *
    * @param start a starting Pair value.
    * @param f     a function which repeatedly transforms the current Pair value.
    * @return a ContinuedFraction
    */
  def createInfinite(start: Pair, f: Pair => Pair): ContinuedFraction =
    createInfiniteWithMarkov(start, f, Hurwitz)

  /**
    * Construct a ContinuedFraction based on a function.
    *
    * @param sequence a sequence of Pairs which will be repeated.
    * @return a ContinuedFraction
    */
  def createInfinite(sequence: List[Pair]): ContinuedFraction =
    createInfiniteWithMarkov(sequence, Hurwitz)

  /**
    * Construct a ContinuedFraction based on a repeated pair.
    *
    * @param p a Pair which will be repeated continually to create the coefficients of a ContinueFraction.
    * @return a ContinuedFraction
    */
  def createInfinite(p: Pair): ContinuedFraction =
    ContinuedFraction(LazyList.continually(p), infinite = true, Hurwitz)

  /**
    * Construct a simple ContinuedFraction based on a repeated constant.
    * Basically, this creates the series for phi.
    *
    * @param x a Long value which will be repeated continually to create the simple ContinuedFraction.
    * @return a (simple) ContinuedFraction.
    */
  def createInfinite(x: Long): ContinuedFraction =
    createInfinite(Pair(x))

  /**
    * Construct a ContinuedFraction based on an initial pattern and a transforming function.
    *
    * @param sequence a sequence of Pairs which will be form the initial pattern.
    * @param f        a function which repeatedly transforms the current sequence.
    * @param markov   the value of Markov constant (defaults to Hurwitz).
    * @return a ContinuedFraction
    */
  def createInfiniteWithMarkov(sequence: List[Pair], f: List[Pair] => List[Pair], markov: Double): ContinuedFraction =
    ContinuedFraction(LazyList.iterate(sequence)(f).flatten, infinite = true, markov)

  /**
    * Construct a ContinuedFraction based on an initial pattern and a transforming function.
    *
    * @param start  a starting Pair value.
    * @param f      a function which repeatedly transforms the current Pair value.
    * @param markov the value of Markov constant (defaults to Hurwitz).
    * @return a ContinuedFraction
    */
  def createInfiniteWithMarkov(start: Pair, f: Pair => Pair, markov: Double): ContinuedFraction =
    ContinuedFraction(LazyList.iterate(start)(f), infinite = true, markov)

  /**
    * Construct a ContinuedFraction based on a repeated sequence.
    *
    * @param sequence a sequence of Pairs which will be repeated.
    * @param markov   the value of Markov constant (defaults to Hurwitz).
    * @return a ContinuedFraction
    */
  def createInfiniteWithMarkov(sequence: List[Pair], markov: Double): ContinuedFraction =
    ContinuedFraction(LazyList.continually(sequence).flatten, infinite = true, markov)

  /**
    * ContinuedFaction representing the square root of 19.
    */
  val root19: ContinuedFraction = Pair(4) +: ContinuedFraction.createInfinite(Pair.list(List(2, 1, 3, 1, 2, 8)))

  /**
    * ContinuedFaction representing the square root of 2.
    */
  val root2: ContinuedFraction = Pair(1) +: ContinuedFraction.createInfinite(Pair(2))

  /**
    * ContinuedFaction representing the square root of 3.
    */
  val root3: ContinuedFraction = Pair(1) +: ContinuedFraction.createInfinite(Pair.list(List(1, 2)))

  /**
    * ContinuedFaction representing the golden ratio (phi).
    */
  val phi: ContinuedFraction = ContinuedFraction.createInfinite(Pair(1))

  /**
    * ContinuedFaction representing e.
    */
  val E: ContinuedFraction = Pair(2) +: ContinuedFraction.createInfinite(Pair.list(List(1, 2, 1)), eFunction _)

  /**
    * ContinuedFaction representing pi.
    */
  val PiSimple: ContinuedFraction = ContinuedFraction(ConFrac.PiSimple, infinite = true, Hurwitz)

  /**
    * This is the Leibniz formula for pi.
    */
  val fPiBy4Leibniz: LazyList[Pair] = Pair.zip(1L +: LazyList.continually(2L), LazyList.from(0).map(2L * _ + 1).map(x => x * x))
  /**
    * Represents the continued fraction for 4 divided by Pi, derived using the Leibniz series.
    *
    * This is an infinite continued fraction built using the function `fPiBy4Leibniz`.
    * The default Markov constant, `Hurwitz`, is used.
    */
  val FourOverPiLeibniz: ContinuedFraction = ContinuedFraction(fPiBy4Leibniz, infinite = true, Hurwitz)

  /**
    * This is the Somayaji formula for pi.
    */
  private val fPiSomayaji: LazyList[Pair] = Pair.zip(3L +: LazyList.continually(6L), LazyList.from(0).map(2L * _ + 1).map(x => x * x))
  /**
    * Represents an infinite continued fraction of Pi based on the fPiSomayaji function.
    *
    * This specific continued fraction is constructed using the fPiSomayaji generator function,
    * designating it to be infinite and associated with the Hurwitz Markov constant.
    */
  val PiSomayaji: ContinuedFraction = ContinuedFraction(fPiSomayaji, infinite = true, Hurwitz)

  /**
    * This is the most direct and fastest-converging formula for pi (I think).
    */
  private val fPi: LazyList[Pair] = Pair.zip(0L +: LazyList.from(0).map(2L * _ + 1), 4L +: LazyList.from(1).map(x => 1L * x * x))
  /**
    * Represents the continued fraction representation of π (Pi) using a specific functional series,
    * denoting an infinite continued fraction approximation of the mathematical constant π.
    *
    * This value is built using the `fPi` function as its series function and specifies that the
    * continued fraction is infinite. The Markov constant used in this instance is `Hurwitz`.
    */
  val PiA: ContinuedFraction = ContinuedFraction(fPi, infinite = true, Hurwitz)

  /**
    * NOTE: match may not be exhaustive.
    *
    * @param xs a list of pairs.
    * @return a list of pairs.
    */
  private def eFunction(xs: List[Pair]): List[Pair] = xs match {
    case List(x, y, z) => List(x, Pair(y.b + 2, y.a), z)
    case _ => throw CoreException("logic error")
  }
}

/**
  * This Class defines a lazily-evaluated Generalized Continued Fraction, somewhat like a LazyList.
  *
  * @param b  the (additive) coefficient, represented on the Wikipedia page (Generalized CF) as b.
  * @param co an optional reference to a ConFrac.
  */
class ConFrac(val b: Long, co: => Option[CF]) extends Evaluatable with Takeable {

  /**
    * Alias method because we can't make co a val.
    *
    * @return the value of co.
    */
  lazy val tailOption: Option[CF] = co

  /**
    * Method to prepend an (b, a) pair of coefficients to this ConFrac.
    * This is particularly useful if the value before the semi-colon (in the standard representation)
    * is not part of a repeating sequence.
    *
    * @param p the coefficient pair to prepend before this ConFrac. Typically, this will be (b, 1).
    * @return a new ConFrac whose initial value is y and whose co value is Some(this).
    */
  def +:(p: Pair): ConFrac =
    new ConFrac(p.b, Some(CF(p.a, this)))

  /**
    * Method to convert an infinite continued fraction into a finite continued fraction.
    *
    * NOTE: this method is not tail-recursive, thus you should limit n to about 100
    *
    * @param n the number of elements to take.
    * @return a ConFrac which starts out the same as this, but after n elements, the tail (co) will be None.
    */
  def take(n: Int): ConFrac =
    if (n <= 0)
      new ConFrac(b, None)
    else
      co match {
        case None => this
        case Some(cf) => new ConFrac(b, Some(CF(cf.a, cf.c.take(n - 1))))
      }

  /**
    * Method to convert an infinite continued fraction into a finite continued fraction.
    *
    * NOTE: this method is not tail-recursive.
    *
    * @param predicate the predicate which operates on the current rational approximation, such that,
    *                  as long as true is returned, we continue to take elements.
    * @return a ConFrac which starts out the same as this but, once the predicate has failed on this, the tail (co) will be None.
    */
  def takeWhile(predicate: Rational => Boolean): ConFrac =
    ConFrac.innerTakeWhile(predicate, this, Rational.infinity)

  /**
    * Method to yield the "convergents" from this ConFrac.
    *
    * @return a lazy list of Rationals.
    */
  def convergents: LazyList[Rational] = {
    def inner(an_2: BigInt, an_1: BigInt, bn_2: BigInt, bn_1: BigInt, w: => LazyList[Pair]): LazyList[Rational] = w match {
      case LazyList() => LazyList()
      case p #:: tail =>
        val an = p.b * an_1 + p.a * an_2
        val bn = p.b * bn_1 + p.a * bn_2
        Rational(an, bn) #:: inner(an_1, an, bn_1, bn, tail)
    }

    // NOTE: coefficients always returns a lazy list of at least one pair.
    val h #:: z = coefficients
    Rational(h.b) #:: inner(1, h.b, 0, 1, z)
  }

  /**
    * Method to get the coefficients.
    *
    * NOTE: this method is not tail-recursive but it will only recurse as deeply
    * as required by the number of elements which must be evaluated.
    *
    * @return a LazyList[Pair] which can never be empty.
    */
  def coefficients: LazyList[Pair] = {
    def inner(_b: Long, a: Long, co: Option[CF]): LazyList[Pair] =
      Pair(_b, a) #:: {
        co match {
          case None => LazyList()
          case Some(cf) => inner(cf.c.b, cf.a, cf.c.tailOption)
        }
      }

    inner(b, 0, tailOption)
  }

  /**
    * Convert this ConFrac into a (lazy) list of coefficients.
    *
    * NOTE: currently, this does not support infinite ConFrac objects.
    * However, we don't currently use this method, other than in the Spec file.
    *
    * @return a lazy list of the coefficients, ending with x.
    */
  def reverseCoefficients: LazyList[Pair] = {
    @tailrec def inner(r: LazyList[Pair], w: Option[CF]): LazyList[Pair] = w match {
      case Some(x) => inner(Pair(x.c.b, x.a) +: r, x.c.tailOption)
      case None => r
    }

    inner(LazyList(), co) :+ Pair(b)
  }

  /**
    * This method is solely for evaluating FINITE continued fractions.
    * You will throw a Stack exception if you attempt this with an infinite (continuous) fraction.
    *
    * @return the Rational equivalent of this finite continued fraction.
    */
  def toRational: Rational = {
    def inner(w: LazyList[Pair]): Rational = w match {
      case LazyList() => Rational.zero
      case p #:: tail => p.a / (p.b + inner(tail))
    }

    val h #:: z = coefficients
    inner(z) + h.b
  }

  /**
    * This method is solely for evaluating continued fractions according to a numerical criterion (z).
    *
    * NOTE: this method is not tail-recursive.
    *
    * @return the Rational equivalent of this continued fraction.
    */
  def toRationalOption(z: Rational): Option[Rational] = co match {
    case Some(c) =>
      val r = z.invert / (c.c.b + c.a)
      if (r.compare(Rational.one) > 0) c.c.toRationalOption(r.invert * c.a) match {
        case Some(cc) => Some(cc.invert * c.a + c.c.b)
        case None => None
      }
      else Some(c.c.b)
    case None => None
  }

  /**
    * Method to convert this ConFrac to an (optional) Rational, according to the epsilon value.
    *
    * Note that, if epsilon is too small, the result may be None.
    *
    * NOTE: this method invokes toRationalOption, which is not tail-recursive.
    *
    * @param epsilon the desired tolerance from the ideal values.
    * @param markov  the Markov constant for this ConFrac.
    * @return either Some(r), where r approximates this ConFrac, or None if the tolerance value could not be achieved.
    */
  def toRational(epsilon: Double)(markov: Double): Option[Rational] =
    Try(takeWhile(ConFrac.hurwitz(epsilon, markov)).toRational).toOption

  /**
    * Method to convert this ConFrac to an (optional) Double, according to the epsilon value.
    *
    * NOTE: this method invokes toNominalRational(Double), which is not tail-recursive.
    *
    * @param epsilon the maximum allowable error.
    * @return an optional approximate value.
    */
  def toDouble(epsilon: Double, markov: Double): Option[Double] =
    toRational(epsilon)(markov).map(_.toDouble)
}

/**
  * Companion object for ConFrac.
  *
  * This object defines an apply method which takes a LazyList of Pairs and returns a ConFrac.
  * This object defines a "simple" method which takes a LazyList of Int and returns a ConFrac.
  *
  * In addition, several ConFrac objects are defined, one each for pi, e, phi, and sqrt(19).
  *
  */
object ConFrac {
  /**
    * Method to construct a General ConFrac from a lazy list of Pair elements.
    *
    * @param ps a lazy list of Pairs.
    * @return a ConFrac where the a and b coefficients derive from successive Pairs.
    */
  def apply(ps: LazyList[Pair]): ConFrac = ps match {
    case p #:: LazyList() =>
      new ConFrac(p.b, None)
    case p #:: tail =>
      new ConFrac(p.b, Some(CF(p.a, ConFrac(tail))))
  }

  /**
    * Method to construct a Simple ConFrac from a lazy list of Int elements.
    *
    * @param xs a lazy list of Ints.
    * @return a ConFrac where all the "a" coefficients (the ones on top) are 1.
    */
  def simple(xs: LazyList[Long]): ConFrac =
    ConFrac(Pair.zip(xs, LazyList.continually(1L)))

  /**
    * Utility method to construct a LazyList[Long] of increasing values, starting at m.
    *
    * @param n the starting value (an Int).
    * @return a LazyList[Long] whose first value is n as a Long.
    */
  def LongLazyListFrom(n: Int): LazyList[Long] =
    LazyList.from(n).map(_.toLong)

  /**
    * Method to evaluate a ConFrac until either it encounters a None for the tailOption (in which case an exception is thrown);
    * or the value of z fails the predicate.
    *
    * @param predicate the predicate which evaluates a Rational.
    * @param cf        a ConFrac.
    * @param r         a Rational.
    * @return a newly constructed ConFrac.
    * @throws ConFracException if the predicate never fails (typically implies that cf is too short for the required precision).
    */
  private def innerTakeWhile(predicate: Rational => Boolean, cf: ConFrac, r: Rational): ConFrac =
    cf.tailOption match {
      case None =>
        throw ConFracException("predicate is never fails")
      case Some(c) =>
        val z = r.invert * c.a + cf.b
        if (predicate(z)) new ConFrac(cf.b, Some(CF(c.a, innerTakeWhile(predicate, c.c, z))))
        else new ConFrac(cf.b, None)
    }

  /**
    * Unapply method for ConFrac.
    *
    * @param x any object.
    * @return Some((Long), Option[CF]) is x is a ConFrac, otherwise, None.
    */
  def unapply(x: Any): Option[(Long, Option[CF])] = x match {
    case c: ConFrac =>
      Some(c.b, c.tailOption)
    case _ =>
      None
  }

  /**
    * Method to measure the Hurwitz factor.
    *
    * CONSIDER factoring the markov constant into the epsilon so that we could eliminate one parameter.
    *
    * @param epsilon the allowable error (must be positive).
    * @param markov  the Markov constant to be considered.
    * @param r       the Rational value to be tested (we only look at the denominator).
    * @return true if the error estimate is greater than the epsilon value.
    */
  def hurwitz(epsilon: Double, markov: Double)(r: Rational): Boolean =
    1.0 / r.d.toDouble / r.d.toDouble / markov > epsilon

  /**
    * CONSIDER most if not all of the following definitions should be in the Spec file (they are duplicated in ContinuousFraction).
    */
  private val lFib: LazyList[Long] =
    1L #:: lFib.scanLeft(1L)(_ + _)

  /**
    * A LazyList of Long values representing an infinite sequence where each value is `1L`.
    * This sequence is continually repeated and is lazily evaluated.
    */
  private val lPhi: LazyList[Long] =
    LazyList.continually(1L)

  /**
    * A lazy list that generates Euler's number continued fraction representation.
    *
    * The sequence starts with 2L and continues by iterating over nested sequences
    * of Long values, which are flattened into a single LazyList of Long.
    * Each iteration generates a sequence of three values: x, y + 2, and z,
    * where x, y, and z are elements from the previous sequence.
    */
  private val lE: LazyList[Long] =
    2L #:: LazyList.iterate(Seq(1L, 2, 1)) { case Seq(x, y, z) => Seq(x, y + 2, z) }.flatten

  /**
    * Continued Fraction for Pi from the sequence [[https://oeis.org/A001203]].
    * NOTE: there is no repeating sequence.
    */
  val lPi: LazyList[Long] =
    LazyList(3L, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1, 1, 2, 2, 2, 2, 1, 84, 2, 1, 1, 15, 3, 13, 1, 4, 2, 6, 6, 99, 1, 2, 2, 6, 3, 5, 1, 1, 6, 8, 1, 7, 1, 2, 3, 7, 1, 2, 1, 1, 12, 1, 1, 1, 3, 1, 1, 8, 1, 1, 2, 1, 6, 1, 1, 5, 2, 2, 3, 1, 2, 4, 4, 16, 1, 161, 45, 1, 22, 1, 2, 2, 1, 4, 1, 2, 24, 1, 2, 1, 3, 1, 2, 1)

  /**
    * A LazyList representing the continued fraction coefficients for the square root of 19.
    * The sequence is constructed with an initial term of 4 followed by a repeating cycle of [2, 1, 3, 1, 2, 8].
    */
  val lRoot19: LazyList[Long] =
    4L #:: LazyList.continually(Seq(2L, 1, 3, 1, 2, 8)).flatten

  /**
    * A LazyList of Long values starting with 1 and followed by a repeating pattern of values 1 and 2.
    *
    * This structure is formed by prepending the value 1 (`1L`) to a continuously generated LazyList
    * of alternating values 1 (`1L`) and 2 (`2`). The `LazyList.continually` creates an infinite sequence
    * of a given pattern, and the `.flatten` ensures the sequence elements are expanded into the resulting LazyList.
    *
    * The sequence is intended for generating elements of a continued fraction or a related numerical sequence.
    */
  val lRoot3: LazyList[Long] =
    1L #:: LazyList.continually(Seq(1L, 2)).flatten

  /**
    * A LazyList representing the continued fraction coefficients for the square root of 2.
    * The first element is 1, followed by a repeated sequence of 2.
    */
  val lRoot2: LazyList[Long] =
    1L #:: LazyList.continually(2L)

  /**
    * Represents the continued fraction expansion of the square root of 19 as a simple continued fraction.
    * It is constructed using the `ConFrac.simple` method with a predefined lazy list of coefficients (`lRoot19`).
    */
  val root19: ConFrac = ConFrac.simple(lRoot19)

  /**
    * A constant Continued Fraction representation of the square root of 2.
    *
    * Constructs a simple Continued Fraction (ConFrac) using a predefined
    * lazy list `lRoot2` representing the sequence of coefficients providing
    * the continued fraction expansion for the square root of 2.
    */
  val root2: ConFrac = ConFrac.simple(lRoot2)

  /**
    * A constant `ConFrac` representing the continued fraction approximation for the square root of 3.
    * Constructed as a simple continued fraction using a lazy list of integers.
    */
  val root3: ConFrac = ConFrac.simple(lRoot3)

  /**
    * Represents the golden ratio (φ) as a continued fraction.
    * This value is constructed using the `ConFrac.simple` method applied
    * to the predefined lazy list `lPhi`.
    */
  val phi: ConFrac = ConFrac.simple(lPhi)

  /**
    * A constant value representing the continued fraction for the mathematical constant *e*.
    * Defined using the `simple` construction method of the `ConFrac` class,
    * which produces a continued fraction with all "a" coefficients set to 1.
    */
  val E: ConFrac = ConFrac.simple(lE)

  /**
    * Represents a simplified continued fraction for Pi using the `simple` method of `ConFrac`.
    * Constructs a continued fraction where all the "a" coefficients (numerators) are 1,
    * utilizing the predefined lazy list `lPi`.
    */
  val PiSimple: ConFrac = ConFrac.simple(lPi)

}

/**
  * Scaled ConFrac.  The "value" of the ConFrac is "divided" by a.
  *
  * CONSIDER making CF a trait.
  *
  * @param a the a coefficient to apply to c.
  * @param c the ConFrac.
  */
case class CF(a: Long, c: ConFrac)

/**
  * A pair of coefficients.
  * The a value is always 1 in the case of a simple Continued Fraction.
  * The b value is always positive (except for the b which precedes a Continue Fraction, which may be negative).
  *
  * CONSIDER representing this by a Rational. However, these coefficients are really independent entities.
  *
  * @param b the b value.
  * @param a the a value.
  */
case class Pair(b: Long, a: Long) {
  /**
    * This method returns a Rational defined as a divided by b.
    * It isn't really useful generally when evaluating continued fractions because we never really need this result
    *
    * @return a/b as a Rational.
    */
  def toRational: Rational = Rational(a, b)
}

/**
  * Companion object for constructing and operating on Pair objects.
  */
object Pair {
  /**
    * Method to construct a Pair from a single Long value (useful for simple continued fractions).
    *
    * @param b the coefficient.
    * @return a Pair of form Pair(b, 1)
    */
  def apply(b: Long): Pair = Pair(b, 1)

  /**
    * Utility method to convert a List[Long] to a List[Pair].
    *
    * @param xs a list of Longs.
    * @return a list of Pairs.
    */
  def list(xs: List[Long]): List[Pair] = xs map (Pair(_))

  /**
    * Method to zip together two LazyLists of Long such that the result is a LazyList of Pair.
    *
    * @param bs the b coefficients (according to the figure on Wikipedia describing Generalized ContinuedFraction).
    * @param as the a coefficients (according to the figure on Wikipedia describing Generalized ContinuedFraction).
    * @return a LazyList of Longs.
    */
  def zip(bs: LazyList[Long], as: LazyList[Long]): LazyList[Pair] = bs zip as map (t => Pair(t._1, t._2))
}

/**
  * This trait is, essentially, a subset of IterableOnce.
  */
trait Takeable {

  /**
    * Method to convert an infinite continued fraction into a finite continued fraction.
    *
    * @param n the number of elements to take.
    * @return a Takeable which starts out the same as this, but after n elements, the tail (co) will be None.
    */
  def take(n: Int): Takeable

  /**
    * Method to convert an infinite continued fraction into a finite continued fraction.
    *
    * @param p the predicate which operates on the current rational approximation, such that,
    *          as long as true is returned, we continue to take elements.
    * @return a Takeable which starts out the same as this but,
    *         once the predicate has failed on this, the tail (co) will be None.
    */
  def takeWhile(p: Rational => Boolean): Takeable
}

/**
  * A trait representing an entity that can be evaluated for an approximate double value.
  */
trait Evaluatable {
  /**
    * Evaluate this Evaluatable object as a Double such that the absolute error between the true value and the result
    * * is less than epsilon.
    *
    * CONSIDER removing the markov parameter.
    *
    * @param epsilon the maximum allowable error.
    * @param markov  the Markov constant for this Evaluatable.
    * @return an optional approximate value.
    */
  def toDouble(epsilon: Double, markov: Double): Option[Double]
}

/**
  * The `Approximate` trait provides a mechanism to compute approximate representations of values of type `X` as `Double`.
  * Implementations of this trait define how to compute this approximation while adhering to a specified maximum allowable error.
  *
  * This is particularly useful for applications where an approximation with controlled precision is sufficient and can improve performance.
  *
  * It operates based on an implicit `epsilon` value, which determines the acceptable error tolerance between the true value and the approximate result.
  *
  * @tparam X The type of the input object to be approximated.
  */
trait Approximate[X] {
  /**
    * Evaluate this Approximatable object as a Double such that the absolute error between the true value and the result
    * is less than epsilon.
    *
    * @param x       the X value.
    * @param epsilon the maximum allowable error.
    * @return the approximate value.
    */
  def toDouble(x: X)(implicit epsilon: Double): Double
}

/**
  * Represents an exception that is specifically thrown in the context of
  * continued fraction computations or related operations.
  *
  * @constructor Creates a new instance of ConFracException with the
  *              provided error message.
  * @param str The error message associated with this exception.
  */
case class ConFracException(str: String) extends RuntimeException(str)