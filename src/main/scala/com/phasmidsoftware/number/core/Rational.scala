package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Rational.{bigZero, narrow}
import com.phasmidsoftware.number.parse.{RationalParser, RationalParserException}

import java.lang.Math._
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Rational.
  *
  * This case class represents Rational numbers by a BigInt numerator and a BigInt denominator.
  * The numerator (n) and the denominator (d) may never share a common factor: if you try to construct a Rational with "new" where there is
  * a common factor, then an exception will be thrown. However, all of the apply methods ensure valid Rational instances by factoring out any such common factors.
  * Similarly, the denominator may not be negative: again, the apply methods will take care of this situation.
  *
  * The domain of Rational includes values with 0 denominator and any numerator (either -ve or +ve infinity) as well as
  * the value with 0 numerator and denominator (NaN).
  *
  * @author scalaprof
  */
case class Rational(n: BigInt, d: BigInt) {

  // Pre-conditions

  // NOTE: ensure that the denominator is positive.
  require(d.signum >= 0, s"Rational denominator is negative: $d")

  // NOTE: ensure that the numerator and denominator are relatively prime.
  require(n == bigZero && d == bigZero || n.gcd(d) == 1, s"Rational($n,$d): arguments have common factor: ${n.gcd(d)}")

  // Operators
  def +(that: Rational): Rational = Rational.plus(this, that)

  def +(that: BigInt): Rational = this + Rational(that)

  def +(that: Long): Rational = this + Rational(that)

  def -(that: Rational): Rational = Rational.minus(this, that)

  def -(that: BigInt): Rational = this - Rational(that)

  lazy val unary_- : Rational = negate

  lazy val negate: Rational = Rational.negate(this)

  def *(that: Rational): Rational = Rational.times(this, that)

  def *(that: BigInt): Rational = this * Rational(that)

  def *(that: Long): Rational = Rational.times(this, that)

  def *(that: Short): Rational = Rational.times(this, that.toLong)

  def /(that: Rational): Rational = this * that.invert

  def /(that: Long): Rational = this / Rational(that)

  def ^(that: Int): Rational = power(that)

  def ^(that: Rational): Try[Rational] = power(that)

  lazy val sqrt: Try[Rational] = power(Rational.half)

  // Other methods appropriate to Rational
  lazy val signum: Int = n.signum

  lazy val invert: Rational = Rational(d, n)

  lazy val isWhole: Boolean = d == 1L

  lazy val isZero: Boolean = n == 0L

  lazy val isUnity: Boolean = n == 1L && isWhole

  lazy val isInfinity: Boolean = d == 0L

  lazy val isNaN: Boolean = isZero && isInfinity

  // NOTE this will throw an exception if the value is too large for an Int (like math.toIntExact)
  lazy val toInt: Int = Rational.toInt(this).get

  // NOTE this will throw an exception if the value is too large for an Int (like math.toIntExact)
  lazy val toLong: Long = Rational.toLong(this).get

  // NOTE this will throw an exception if the value is too large for an Int (like math.toIntExact)
  lazy val toBigInt: BigInt = Rational.toBigInt(this).get

  lazy val toFloat: Float = Rational.toFloat(this)

  lazy val toDouble: Double = Rational.toDouble(this)

  def power(x: Rational): Try[Rational] = {
    val maybeNum: Try[Int] = narrow(x.n, Int.MinValue, Int.MaxValue) map (_.toInt)
    val maybeDenom: Try[Int] = narrow(x.d, Int.MinValue, Int.MaxValue) map (_.toInt)
    for (p <- maybeNum; r <- maybeDenom; z <- FP.toTryWithThrowable(this.power(p).root(r), RationalException("can't get exact root"))) yield z
  }

  def power(x: Int): Rational = {
    @tailrec def inner(r: Rational, x: Int): Rational = if (x == 0) r else inner(r * this, x - 1)

    if (x == 0) Rational.one
    else {
      val rational = inner(Rational.one, math.abs(x))
      if (x > 0) rational
      else rational.invert
    }
  }

  /**
    * Method to get the xth root of this Rational.
    * Note that it is not guaranteed to result in an exact value.
    *
    * @param x the root to be taken, for example, for the cube root, we set x = 3.
    * @return an (optional) Rational result which is the exact root.
    *         In the event that it's not possible to get the exact root, then None is returned.
    */
  def root(x: Int): Option[Rational] =
    for (a <- Rational.root(n, x); b <- Rational.root(d, x)) yield Rational(a, b)

  lazy val toBigDecimal: BigDecimal = BigDecimal(n) / BigDecimal(d)

  def compare(other: Rational): Int = Rational.compare(this, other)

  lazy val toRationalString = s"$n/$d"

  lazy val isExactDouble: Boolean = toBigDecimal.isExactDouble // Only work with Scala 2.11 or above

  def applySign(negative: Boolean): Rational = if (negative) negate else this

  def applyExponent(exponent: Int): Rational = this * Rational.exponent(exponent)

  /**
    * Method to determine the Mediant of two rational numbers.
    * See Wikipedia: Mediant (Mathematics).
    * The result will always be intermediate in value to this and other.
    * The Mediant plays a role in the Farey Sequence (see Wikipedia) and thus can be used to approximate
    * an irrational number as a rational number.
    *
    * @param other the other number.
    * @return the mediant of this and other.
    */
  def mediant(other: Rational): Rational = if (signum >= 0 && other.signum >= 0 && !isInfinity && !other.isInfinity)
    Rational(n + other.n, d + other.d)
  else Rational.NaN

  override def toString: String =
    if (isNaN) "NaN"
    else if (isInfinity) (if (n > 0) "+ve" else "-ve") + " infinity"
    else if (isWhole) toBigInt.toString
    else if (d > 100000L || isExactDouble) toDouble.toString
    else toRationalString
}

object Rational {

  /**
    * Implicit class RationalOps to allow an Int to be treated as a Rational for the purpose
    * of allowing operations with right-hand Rational arguments or for division by an Int.
    * A particular case is rational division where we must convert the numerator to Rational so that
    * we don't end up with 0 as the result of an Int division.
    *
    * @param x an Int
    */
  implicit class RationalOps(x: Int) {

    /**
      * Method to increment x by r.
      *
      * @param r the addend.
      * @return the result of x + r.
      */
    def +(r: Rational): Rational = Rational(x) + r

    /**
      * Method to multiply x by r.
      *
      * @param r the multiplicand.
      * @return the result of x * r.
      */
    def *(r: Rational): Rational = Rational(x) * r

    /**
      * Method to divide by Rational.
      * We cannot just use "/" as that will bind to Int and result in 0.
      * Therefore, we use the special ":/" operator which will work as long as RationalOps is imported.
      *
      * @param denominator the denominator of the resulting Rational. Any Int will be implicitly converted to a Long.
      * @return the Rational whose value is x/y
      */
    def :/(denominator: Long): Rational = Rational(x, denominator)
  }

  /**
    * Implicit class RationalHelper to allow definition of Rationals by Strings of the form r"n/d".
    *
    * @param sc a StringContext.
    */
  implicit class RationalHelper(val sc: StringContext) extends AnyVal {
    def r(args: Any*): Rational = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val sb = new StringBuffer()
      while (strings.hasNext) {
        val s = strings.next()
        if (s.isEmpty) {
          if (expressions.hasNext)
            sb.append(expressions.next())
          else
            throw RationalException("r: logic error: missing expression")
        }
        else
          sb.append(s)
      }
      if (expressions.hasNext)
        throw RationalException(s"r: ignored: ${expressions.next()}")
      else
        Rational(sb.toString)
    }
  }

  /**
    * Method to approximate an irrational number as a Rational.
    * Application code should always call this method or, preferably, Rational.apply(Double).
    *
    * NOTE: this method is designed for true Doubles, not floating-point representations of decimal numbers.
    * Such decimal numbers should be converted to BigDecimal first using BigDecimal.valueOf(x).
    *
    * TODO what we need here is to pass in a function and a desired Rational result,
    * and perhaps even the first derivative of the function, so that we can approximate the solution (root) of the function.
    *
    * @param x       the value to approximate.
    * @param epsilon (implicit) the tolerance.
    * @return a Rational such that the difference between the result and x is less than epsilon.
    */
  def approximateAny(x: Double)(implicit epsilon: Tolerance): Rational =
    if (x == Double.NaN) NaN
    else if (x.isPosInfinity) infinity
    else if (x.isNegInfinity) infinity.negate
    else if (x > 0) approximatePositive(x)
    else approximatePositive(-x).negate

  /**
    * Method to take a Double in the range 0 thru 1 and approximate it by a Rational number
    * to within the given tolerance (epsilon).
    *
    * NOTE: this Farey sequence converges very slowly. In order to put an absolute limit on the recursion
    * (which otherwise might never terminate), we limit the size of the denominator of r1 to be 10E12.
    * It shouldn't matter much whether we test r1 or r2 in this context.
    *
    * @param x       the value to approximate (should be between 0 and 1).
    * @param epsilon (implicit) the tolerance.
    * @return a Rational such that the difference between the result and x is less than epsilon.
    * @throws IllegalArgumentException if x is not between 0 and 1.
    */
  def approximate(x: Double)(implicit epsilon: Tolerance): Rational = {
    require(x >= 0 && x <= 1, "Call convertDouble instead of approximate")

    @scala.annotation.tailrec
    def inner(r1: Rational, r2: Rational): Rational =
      if (r1.d > epsilon.maxDenom || math.abs(r1.toDouble - x) < epsilon.x) r1
      else {
        val mediant: Rational = r1 mediant r2
        if (mediant.toDouble > x) inner(r1, mediant)
        else inner(mediant, r2)
      }

    inner(Rational.zero, Rational.one)
  }

  val bigZero: BigInt = BigInt(0)
  val bigOne: BigInt = BigInt(1)
  val zero: Rational = Rational(0)
  lazy val infinity: Rational = zero.invert
  val one: Rational = Rational(1)
  val ten: Rational = Rational(10)
  val two: Rational = Rational(2)
  lazy val half: Rational = two.invert
  lazy val NaN = new Rational(0, 0)

  /**
    * Method to construct a Rational from two BigInt values.
    * NOTE: the this method ensures that the numerator and denominator are normalized.
    *
    * @param n the numerator.
    * @param d the denominator.
    * @return a Rational with the same ratio as n/d.
    */
  def apply(n: BigInt, d: BigInt): Rational = normalize(n, d)

  /**
    * Method to construct a Rational from a BigInt numerator and a Long denominator.
    * NOTE: the this method ensures that the numerator and denominator are normalized.
    *
    * @param n the numerator.
    * @param d the denominator.
    * @return a Rational with the same ratio as n/d.
    */
  def apply(n: BigInt, d: Long): Rational = apply(n, BigInt(d))

  /**
    * Method to construct a Rational from a BigInt.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: BigInt): Rational = apply(n, bigOne)

  /**
    * Method to construct a Rational from a BigInt with sign defined by "negative".
    *
    * @param n        the value.
    * @param negative if true then the sign of the result will be flipped.
    * @return a Rational with the same value as n.
    */
  def apply(n: BigInt, negative: Boolean): Rational = apply(n).applySign(negative)

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Long): Rational = apply(BigInt(n))

  /**
    * Method to construct a Rational from a Long.
    *
    * @param n the value.
    * @return a Rational with the same value as n.
    */
  def apply(n: Int): Rational = apply(BigInt(n))

  /**
    * Method to construct a Rational based on a Double.
    * Since expressing a Double with a literal requires a decimal representation,
    * our first option is to convert the Double to a BigDecimal and use that.
    * If that fails for some reason, we try instead to use approximateAny.
    *
    * The tolerance (epsilon) for approximateAny is determined by the implicit value defined in Tolerance.
    * NOTE: if you want to specify the epsilon yourself, then invoke approximateAny directly.
    *
    * @param x the Double value.
    * @return a Rational which closely approximates x.
    */
  def apply(x: Double): Rational = Try(apply(BigDecimal.valueOf(x))).getOrElse(approximateAny(x))

  /**
    * Method to construct a Rational based on a BigDecimal.
    *
    * @param x the BigDecimal to convert.
    * @return a Rational which is equal to x.
    * @throws RationalException if x cannot be represented as a Rational.
    */
  def apply(x: BigDecimal): Rational =
    if (x.scale >= 0) {
      val e = BigDecimal.apply(10).pow(x.scale)
      (for (n <- (x * e).toBigIntExact; d <- e.toBigIntExact) yield Rational(n, d)) match {
        case Some(r) => r
        case None => throw RationalException(s"Rational.apply(BigDecimal): cannot represent $x as a Rational")
      }
    }
    else x.toBigIntExact match {
      case Some(b) => Rational(b)
      case None => throw RationalException(s"cannot get value from BigDecimal $x")
    }

  /**
    * Method to construct a Rational based on a String.
    * NOTE: this method is NOT safe. It is much better to invoke parse(w).
    *
    * @param w the String value.
    * @return a Rational corresponding to the value given by w.
    * @throws RationalParserException if w is malformed.
    */
  def apply(w: String): Rational = parse(w).get

  /**
    * Method to construct a Try[Rational] based on a String.
    *
    * @param w the String value.
    * @return either Success(Rational) with value corresponding to the value given by w
    *         or Failure(RationalParserException) if w is malformed.
    */
  def parse(w: String): Try[Rational] = RationalParser.parse(w)

  /**
    * Method to yield a Rational exponent (in the sense of the a literal Double: for example 1.0Ex).
    *
    * @param x the power to which we should raise 10.
    * @return 10 raised to the power x, expressed as a Rational.
    */
  def exponent(x: Int): Rational = ten.power(x)

  /**
    * Implicit object which bestows all of the Ordering, Numeric, etc. functionality to a Rational.
    */
  implicit object RationalFractional extends RationalIsFractional

  /**
    * Implicit converter from Double to Rational.
    *
    * @param x the value.
    * @return a Rational equal to or approximately equal to x.
    */
  implicit def convertDouble(x: Double): Rational = Rational(x)

  /**
    * Implicit converter from Long to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def convertLong(x: Long): Rational = Rational(x)

  /**
    * Implicit converter from BigInt to Rational.
    *
    * @param x the value.
    * @return a Rational equal to x.
    */
  implicit def convertBigInt(x: BigInt): Rational = Rational(x)

  /**
    * Trait defined to support the methods of Ordering[Rational].
    */
  trait RationalIsOrdering extends Ordering[Rational] {
    def compare(x: Rational, y: Rational): Int = x.compare(y)
  }

  /**
    * Trait defined to support the methods of Numeric[Rational].
    */
  trait RationalIsNumeric extends RationalIsOrdering with Numeric[Rational] {
    def plus(x: Rational, y: Rational): Rational = x + y

    def minus(x: Rational, y: Rational): Rational = x - y

    def times(x: Rational, y: Rational): Rational = x * y

    def negate(x: Rational): Rational = Rational(-x.n, x.d)

    def fromInt(x: Int): Rational = Rational(x)

    def parseString(str: String): Option[Rational] = Rational.parse(str).toOption

    def toInt(x: Rational): Int = x.toInt

    def toLong(x: Rational): Long = x.toLong

    def toFloat(x: Rational): Float = x.toFloat

    def toDouble(x: Rational): Double = x.toDouble
  }

  /**
    * Trait defined to support the methods of Fractional[Rational].
    */
  trait RationalIsFractional extends RationalIsNumeric with Fractional[Rational] {
    def div(x: Rational, y: Rational): Rational = Rational.div(x, y)
  }

  // CONSIDER making this private or moving back into RationalSpec
  def hasCorrectRatio(r: Rational, top: BigInt, bottom: BigInt): Boolean = {
    val _a = r * bottom
    val result = bottom == 0 || _a.isInfinity || (_a.isWhole && _a.toBigInt == top)
    if (!result) throw RationalException(s"incorrect ratio: r=${r.n}/${r.d}, top=$top, bottom=$bottom, _a=${_a}, gcd=${top.gcd(bottom)}")
    result
  }

  /**
    * Method to get the (integral) xth root of b.
    *
    * @param b a BigInt.
    * @param x an Int.
    * @return an optional BigInt which, when raised to the power of x, equals b.
    */
  def root(b: BigInt, x: Int): Option[BigInt] =
    Try(BigInt(math.round(math.pow(b.toDouble, 1.0 / x)))) match {
      case Success(z) if z.pow(x) == b => Some(z)
      case _ => None
    }

  // CONSIDER eliminating this, but it is currently employed by Operations.
  val squareRoots = Map(1 -> 1, 4 -> 2, 9 -> 3, 16 -> 4, 25 -> 5, 36 -> 6, 49 -> 7, 64 -> 8, 81 -> 9, 100 -> 10, 256 -> 16, 1024 -> 32, 4096 -> 64, 10000 -> 100)

  /**
    * Method to process the numerator and denominator to ensure that the denominator is never zero and never shares a common factor with the numerator.
    *
    * @param n the numerator
    * @param d the denominator
    * @return a Rational formed from n and d.
    */
  @scala.annotation.tailrec
  private def normalize(n: BigInt, d: BigInt): Rational =
    if (d < 0) normalize(-n, -d) else {
      // CONSIDER that we weren't previously taking the abs of d (which should always be positive)
      val g = n.gcd(d)
      g.signum match {
        case 0 => Rational.NaN
        case _ => new Rational(n / g, d / g)
      }
    }

  private def minus(x: Rational, y: Rational): Rational = plus(x, negate(y))

  private def negate(x: Rational): Rational = Rational(-x.n, x.d)

  private def plus(x: Rational, y: Rational): Rational = Rational((x.n * y.d) + (y.n * x.d), x.d * y.d)

  private def times(x: Rational, y: Rational): Rational = Rational(x.n * y.n, x.d * y.d)

  private def toDoubleViaString(x: BigInt) = x.toString().toDouble

  private def toDouble(x: Rational): Double = Try((BigDecimal(x.n) / BigDecimal(x.d)).toDouble).getOrElse(toDoubleViaString(x.n) / toDoubleViaString(x.d))

  private def toFloat(x: Rational): Float = toDouble(x).toFloat

  private def narrow(x: Rational, min: BigInt, max: BigInt): Try[BigInt] = for (b <- toBigInt(x); z <- narrow(b, min, max)) yield z

  def narrow(x: BigInt, min: BigInt, max: BigInt): Try[BigInt] =
    if (min <= x && x <= max) Success(x)
    else Failure(RationalException("narrow: loss of precision"))

  private def toLong(x: Rational): Try[Long] = narrow(x, Long.MinValue, Long.MaxValue) map (_.toLong)

  /**
    * This method gets an Int from a Rational.
    *
    * XXX: Needs to be public for testing
    *
    * @param x a Rational.
    * @return an Try[Int]
    */
  def toInt(x: Rational): Try[Int] = narrow(x, Int.MinValue, Int.MaxValue) map (_.toInt)

  /**
    * Get Rational x as a pair of Longs (if possible).
    * The result will be at full precision or an exception will be thrown.
    *
    * @param x a Rational.
    * @return an optional tuple of Longs.
    */
  def toLongs(x: Rational): Option[(Long, Long)] = (for (n <- narrow(x.n.toLong, Long.MinValue, Long.MaxValue); d <- narrow(x.d.toLong, Long.MinValue, Long.MaxValue)) yield (n.toLong, d.toLong)).toOption

  /**
    * Get Rational x as a pair of Ints (if possible).
    * The result will be at full precision or an exception will be thrown.
    *
    * @param x a Rational.
    * @return an optional tuple of Ints.
    */
  def toInts(x: Rational): Option[(Int, Int)] = toLongs(x) map { case (n, d) => (math.toIntExact(n), math.toIntExact(d)) }

  // CONSIDER making this public
  private def toBigInt(x: Rational): Try[BigInt] = if (x.isWhole) Success(x.n) else Failure(RationalException(s"toBigInt: $x is " + (if (x.d == 0L)
    "infinite" else "not whole")))

  private def div(x: Rational, y: Rational): Rational = x / y

  private def compare(x: Rational, y: Rational): Int = minus(x, y).signum

  private def approximatePositive(x: Double)(implicit epsilon: Tolerance) =
    if (x > 0.1 && x < 10) approximateSmall(x)
    else {
      val e: Int = getExponent(x)
      val scale = pow(2, -e)
      approximateSmall(x * scale) * Rational.two.power(e)
    }

  private def approximateSmall(x: Double)(implicit epsilon: Tolerance) =
    if (x < 1) approximate(x)
    else {
      val y = floor(x).toInt
      approximate(x - y) + y
    }
}

/**
  * Exception class for Rationals.
  *
  * @param s the cause as a String.
  */
case class RationalException(s: String) extends Exception(s)

/**
  * Value class to define Tolerance.
  *
  * @param x        the tolerance (epsilon) value.
  * @param maxDenom used in the approximation of an irrational number --
  *                 whereby the denominator is limited to approximately this value.
  */
case class Tolerance(x: Double, maxDenom: BigInt)

/**
  * Companion object to Tolerance.
  */
object Tolerance {
  /**
    * Standard tolerance (epsilon) of 10 to the power of -15.
    */
  implicit val standardTolerance: Tolerance = Tolerance(1E-14, BigInt(1000000000L))
}
