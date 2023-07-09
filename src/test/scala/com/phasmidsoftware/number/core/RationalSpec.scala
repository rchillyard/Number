package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Rational.{RationalHelper, bigTen, findRepeatingSequence}
import org.scalatest.matchers.should
import org.scalatest.{PrivateMethodTester, flatspec}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

/**
  * @author scalaprof
  */
class RationalSpec extends flatspec.AnyFlatSpec with should.Matchers with PrivateMethodTester {

  private val zeroSymbol = Symbol("zero")
  private val toBigIntSymbol = Symbol("toBigInt")
  private val wholeSymbol = Symbol("whole")
  private val infinitySymbol = Symbol("infinity")
  private val unitySymbol = Symbol("unity")
  private val narrowSymbol = Symbol("narrow")
  private val normalizeSymbol = Symbol("normalize")
  private val compareSymbol = Symbol("compare")

  behavior of "new Rational(BigInt,Long)"

  it should "work for 0, 1" in {
    val r = new Rational(BigInt(0), 1L)
    r.isZero shouldBe true
    r shouldBe Rational(Rational.bigZero, 1)
  }
  it should "yield infinity for 1, 0" in {
    val r = new Rational(BigInt(1), 0L)
    r.isInfinity shouldBe true
  }
  it should "yield NaN for 0, 0" in {
    new Rational(BigInt(0), 0L).isNaN shouldBe true
  }
  it should "fail for 2, 2" in {
    a[IllegalArgumentException] should be thrownBy new Rational(BigInt(2), 2L)
  }
  it should "succeed for 0, -1" in {
    new Rational(BigInt(0), -1L).d shouldBe BigInt(-1)
  }
  it should "fail for 1, -1" in {
    a[IllegalArgumentException] should be thrownBy new Rational(BigInt(1), -1L)
  }

  behavior of "apply(BigInt,Long)"
  it should "work for 0, 1" in {
    Rational(BigInt(0), 1L) shouldBe Rational(Rational.bigZero, 1)
  }
  it should "succeed for 2, 2" in {
    Rational(BigInt(2), 2L) shouldBe Rational.one
  }
  it should "succeed for -1, -1" in {
    Rational(BigInt(-1), -1L) shouldBe Rational.one
  }
  it should "work for 355, 113" in {
    val a: BigInt = 355
    val b: Int = 113
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }
  it should "work for 355*2, 113*2" in {
    val a: BigInt = 355 * 2
    val b: Int = 113 * 2
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }
  it should "work for -355, -113" in {
    val a: BigInt = -355
    val b: Int = -113
    val r = Rational(a, b)
    Rational.hasCorrectRatio(r, a, b) shouldBe true
  }
  it should "work for -355 (2)" in {
    val a: Rational = Rational(BigInt(355), negative = true)
    a shouldBe Rational(-355L)
  }

  behavior of "apply(Long,Long)"
  it should "work for 0, 1" in {
    Rational(0L, 1L) shouldBe Rational(Rational.bigZero, 1)
  }
  behavior of "apply(Long,Int)"
  it should "work for 0, 1" in {
    Rational(0L, 1) shouldBe Rational(Rational.bigZero, 1)
  }
  behavior of "apply(Int,Int)"
  it should "work for 0, 1" in {
    Rational(0, 1) shouldBe Rational(Rational.bigZero, 1)
  }
  it should "work for 0, 0" in {
    Rational(0, 0) shouldBe Rational.NaN
  }
  it should "work for 1, 0" in {
    val r = Rational(1, 0)
    r.isInfinity shouldBe true
    r shouldBe Rational.infinity
    r.render shouldBe "+ve infinity"
  }
  it should "work for -1, 0" in {
    val r = Rational(-1, 0)
    r.isInfinity shouldBe true
    r.render shouldBe "-ve infinity"
  }
  it should "work for -1, -2" in {
    val r = Rational(-1, -2)
    r.isInfinity shouldBe false
    r shouldBe Rational.half
  }
  it should "work for -2624712818L, -1" in {
    val r = Rational(BigInt(-2624712818L), -1)
    r.signum shouldBe 1
  }

  it should "fail to convert to BigInt" in {
    val r = Rational(-1, 0)
    val decorateToBigInt = PrivateMethod[Try[BigInt]](toBigIntSymbol)
    val z: Try[BigInt] = Rational invokePrivate decorateToBigInt(r)
    a[RationalException] should be thrownBy z.get
  }

  behavior of "apply(Long)"
  it should "work for Rational.bigZero" in {
    Rational(0L) shouldBe new Rational(Rational.bigZero, 1)
  }

  behavior of "apply(Int)"
  it should "work for 0" in {
    Rational(0)
  }

  behavior of "apply(Double)"
  it should "convert Avagadro's number" in {
    val target = Rational("6.02214076E23")
    target shouldBe Rational(6.02214076E23) +- 1E9
  }
  it should "convert a very small number" in {
    val verySmall = Rational(-4.076956044934884E-134)
    Rational(-4.076956044934884E-134) shouldBe verySmall
  }
  it should "be zero for very very small number" in {
    val x = 1.111314801067662E-299
    val epsilon: Double = 1E-305
    val r = Rational(x)
    r.toDouble shouldBe x +- epsilon
  }
  it should "convert 3.1416 correctly" in {
    val target = Rational(3.1416)
    target shouldBe Rational(3927, 1250)
  }
  it should "convert 3.1416 the same as \"3.1416\"" in {
    val target = Rational(3.1416)
    target shouldBe Rational("3.1416") +- 1E-10
  }
  it should "pick up a float" in {
    val target = Rational(1.5f)
    target shouldBe Rational(3, 2)
  }

  behavior of "apply(BigDecimal)"
  it should "convert 0.5 to Rational" in {
    Rational(BigDecimal.valueOf(0.5)) shouldBe Rational.half
  }
  it should "convert to Rational" in {
    // NOTE: this is rather artificial
    val pi = BigDecimal(math.Pi)
    val r = Rational(pi)
    r.toDouble shouldBe math.Pi
  }

  behavior of "isWhole"
  it should "be true for integers" in {
    Rational.zero.isWhole shouldBe true
    Rational.one.isWhole shouldBe true
    Rational.two.isWhole shouldBe true
  }
  it should "be false for reciprocals" in {
    Rational.zero.invert.isWhole shouldBe false
    Rational.one.invert.isWhole shouldBe true
    Rational.two.invert.isWhole shouldBe false
  }

  behavior of "toBigInt"
  it should "work for Long.MaxValue" in {
    val r = Rational(Long.MaxValue)
    r.toBigInt shouldBe BigInt(Long.MaxValue)
  }

  behavior of "toLong"
  it should "work for Long.MaxValue" in {
    val r = Rational(Long.MaxValue)
    r.toLong shouldBe Long.MaxValue
  }

  behavior of "toBigDecimal"
  it should "work for pi" in {
    val pi = BigDecimal(Math.PI)
    val r = Rational(pi)
    r.toBigDecimal shouldBe Some(pi)
    r.toDouble shouldBe Math.PI +- 1E-15
  }

  behavior of "equals"
  it should "equate 0 and zero" in {
    Rational(0) shouldBe Rational.zero
    Rational.zero shouldBe zeroSymbol
  }
  it should "be whole" in {
    Rational.zero shouldBe wholeSymbol
  }
  it should "equal 0" in {
    Rational.zero.toInt shouldBe 0
  }
  it should "equal infinity when inverted" in {
    Rational.zero.invert shouldBe infinitySymbol
  }
  it should "equal BigDecimal.ZERO" in {
    Rational.zero.toBigDecimal shouldBe Some(BigDecimal(0))
  }
  it should "equal r when added to r" in {
    val r = Rational(22, 7) // we could choose anything here
    (Rational.zero + r) shouldBe r
  }
  it should "equal infinity when r-interpolator has 0 denominator" in {
    r"1/0" shouldBe infinitySymbol
  }

  behavior of "+"
  it should "return 4 for 2+2" in {
    val r1 = Rational(2)
    val r2 = Rational(2)
    val r = r1 + r2
    r shouldBe Rational(4, 1)
  }
  it should "return result for 0+-236274181" in {
    val n1 = BigInt(0)
    val d1 = -1L
    val n2 = BigInt(-2362741811L)
    val d2 = 1L
    val r = Rational(n1, d1) + Rational(n2, d2)
    r shouldBe Rational(BigInt(-2362741811L), 1)
  }
  it should "add 0 to large number" in {
    val r1 = Rational(0)
    val r2 = Rational(BigInt("18050442446843353054"))
    val r = r1 + r2
    Rational.hasCorrectRatio(r, BigInt("18050442446843353054"), 1L) shouldBe true
  }
  it should "add 0 to large number (2)" in {
    val r1 = Rational(0)
    val r2 = BigInt("18050442446843353054")
    val r = r1 + r2
    Rational.hasCorrectRatio(r, BigInt("18050442446843353054"), 1L) shouldBe true
  }
  it should "add 1 to large number" in {
    val r1 = Rational(1)
    val r2 = Rational(BigInt("18050442446843353054"), 2)
    val r = r1 + r2
    Rational.hasCorrectRatio(r, 2 + BigInt("18050442446843353054"), 2L) shouldBe true
  }
  it should "add large number to smaller one" in {
    val r1 = Rational(BigInt("-9223372036854775808"), 1)
    val r2 = Rational(BigInt("816512980"), -1)
    val r = r1 + r2
    Rational.hasCorrectRatio(r, 816512980 - BigInt("-9223372036854775808"), -1) shouldBe true
  }

  behavior of "*"
  it should "return 4 for 2*2" in {
    val r1 = Rational(2)
    val r2 = Rational(2)
    val r = r1 * r2
    r shouldBe Rational(4, 1)
  }
  it should "return result for 0 * -236274181" in {
    val n1 = BigInt(0)
    val d1 = -1L
    val n2 = BigInt(-2362741811L)
    val d2 = 1L
    val r = Rational(n1, d1) * Rational(n2, d2)
    r shouldBe Rational.zero
  }
  it should "multiply 2 by large number" in {
    val r1 = Rational(2)
    val r2 = Rational(BigInt("18050442446843353054"), 2)
    val r = r1 * r2
    r shouldBe Rational(BigInt("18050442446843353054"))
  }
  it should "multiply large number by smaller one" in {
    val r1 = Rational(BigInt("-9223372036854775808"), 1)
    val r2 = Rational(BigInt("816512980"), -1)
    val r = r1 * r2
    r shouldBe Rational(BigInt("-9223372036854775808") * BigInt(-816512980L))
  }
  it should "multiply large number by short" in {
    val r1 = Rational(BigInt("-9223372036854775808"), 1)
    val r = r1 * 15.toShort
    r shouldBe Rational(BigInt(-9223372036854775808L) * BigInt(15))
  }

  behavior of "division"
  it should "work for 6 / 10 / 3" in {
    import com.phasmidsoftware.number.core.Rational.RationalOps
    // TODO why do we have to use parentheses here?
    val r: Rational = (6 :/ 10) / 3
    r shouldBe Rational(5).invert
  }

  behavior of "negate"
  it should "work for -1" in {
    val r = Rational(-1)
    r.negate shouldBe Rational.one
  }

  behavior of "implicit conversion"
  it should "work for 0" in {
    val r: Rational = BigInt(0)
    r shouldBe Rational.zero
  }

  behavior of "1/2"
  it should "be OK" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "equal half" in {
    Rational("1/2") shouldBe Rational.half
  }
  it should "be half of one" in {
    Rational.half * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator" in {
    r"1/2" * 2 shouldBe Rational.one
  }
  it should "be OK using r-interpolator with variable" in {
    val denominator = 2
    r"1/$denominator" * denominator shouldBe Rational.one
  }

  behavior of "1"
  it should "be OK" in {
    Rational(1)
  }
  it should "be one" in {
    Rational(1) shouldBe Rational.one
  }
  it should "be positive" in {
    Rational.one.signum shouldBe 1
  }
  it should "be whole" in {
    Rational.one shouldBe wholeSymbol
  }
  it should "be unity" in {
    Rational.one shouldBe unitySymbol
  }
  it should "equal 1" in {
    Rational.one.toInt shouldBe 1
  }
  it should "not equal infinity when inverted" in {
    Rational.one.invert should not be infinitySymbol
  }
  it should "equal itself when inverted" in {
    Rational.one.invert shouldBe Rational.one
  }
  it should "equal BigDecimal.one" in {
    Rational.one.toBigDecimal shouldBe Some(BigDecimal(1))
  }
  it should "equal r when multiplied by r" in {
    val r = Rational(22, 7) // we could choose anything here
    (Rational.one * r) shouldBe r
  }
  it should "be -1 when negated" in {
    val r = Rational.one
    -r shouldBe (Rational.one * -1)
    r.signum shouldBe 1
  }

  behavior of "power"
  it should "work for Int power" in {
    val ten = Rational.ten
    ten.power(2) shouldBe Rational(100)
    ten.power(10) shouldBe Rational(10000000000L)
    ten.power(0) shouldBe Rational.one
    ten.power(-1) shouldBe ten.invert
  }

  it should "work for Rational power (1)" in {
    val ten = Rational.ten
    ten.power(Rational(2)) shouldBe Success(Rational(100))
    ten.power(Rational(10)) shouldBe Success(Rational(10000000000L))
    ten.power(Rational.zero) shouldBe Success(Rational.one)
    ten.power(Rational(-1)) shouldBe Success(ten.invert)
  }

  it should "work for Rational power (2)" in {
    val target = Rational(9, 16)
    target.power(Rational.two) shouldBe Success(Rational(81, 256))
    target.power(Rational.half).flatMap(r => r.power(Rational.two)) shouldBe Success(target)
    target.power(Rational(-1)) shouldBe Success(target.invert)
  }

  it should "work for Rational power (3)" in {
    val target = Rational(8, 27)
    target.power(Rational(2, 3)) shouldBe Success(Rational(4, 9))
  }

  it should "work for Rational power (4)" in {
    val target = Rational(8, 27)
    target ^ Rational(2, 3) shouldBe Success(Rational(4, 9))
  }

  it should "fail for non-exact powers" in {
    val target = Rational(7, 28)
    target.power(Rational(2, 3)).isFailure shouldBe true
  }

  behavior of "exponent"
  it should "work for 2" in {
    val hundred = Rational.exponent(2)
    hundred shouldBe Rational(100)
  }
  it should "work for Avagadro" in {
    val r = Rational(6.02214076)
    val avagadro = r.applyExponent(23)
    avagadro shouldBe Rational("6.02214076E23")
  }

  behavior of "10"
  it should "be OK" in {
    Rational(10)
  }
  it should "be ten" in {
    Rational(10) shouldBe Rational.ten
  }
  it should "be whole" in {
    Rational.ten shouldBe wholeSymbol
  }
  it should "not be zero" in {
    Rational.ten should not be zeroSymbol
  }
  it should "equal 10" in {
    Rational.ten.toInt shouldBe 10
  }
  it should "equal 5*2" in {
    (Rational.ten / 2) shouldBe Rational(5)
  }
  it should "equal 10*1" in {
    (Rational.ten / 10) shouldBe Rational.one
  }
  it should "equal BigDecimal(10)" in {
    Rational.ten.toBigDecimal shouldBe Some(BigDecimal(10))
  }
  it should "equal a million when raised to 6th power" in {
    (Rational.ten ^ 6) shouldBe Rational(1000000)
  }

  behavior of "2/3"
  it should "be OK" in {
    Rational(2, 3)
  }
  it should "equal -1/3 when added to -1" in {
    Rational(2, 3) - Rational.one shouldBe Rational(-1, 3)
  }
  it should "be less than 1" in {
    Rational(2, 3).compare(Rational.one) shouldBe (-1)
  }
  it should "not be whole" in {
    Rational(2, 3) should not be wholeSymbol
  }
  it should "equal 2 when multiplied by 3" in {
    // TODO try to understand what the Analyzer sees as wrong here.
    (Rational(2, 3) * 3 toInt) shouldBe 2
  }
  it should "equal 3/2 when inverted" in {
    Rational(2, 3).invert shouldBe Rational(3, 2)
  }
  it should "equal 5/3 when added to 1" in {
    (Rational.one + Rational(2, 3)) shouldBe Rational(5, 3)
  }
  it should "equal 4/9 when multiplied by itself" in {
    val r = Rational(2, 3)
    (r * r) shouldBe Rational(4, 9)
  }
  it should "equal 4/9 when squared" in {
    (Rational(2, 3) ^ 2) shouldBe Rational(4, 9)
  }
  it should "barf when toInt invoked" in {
    an[RationalException] should be thrownBy Rational(2, 3).toInt
  }
  it should "barf when Rational.toInt invoked" in {
    an[RationalException] should be thrownBy Rational.toInt(Rational(2, 3)).get
    val thrown = the[Exception] thrownBy Rational(2, 3).toInt
    thrown.getMessage should equal("2/3 is not whole")
  }

  behavior of "2/4"
  it should "not be OK" in {
    val thrown = the[IllegalArgumentException] thrownBy new Rational(2, 4)
    thrown.getMessage should equal("requirement failed: Rational(2,4): arguments have common factor: 2")
  }
  it should "not OK via apply" in {
    Rational(2, 4) shouldBe Rational.half
  }

  behavior of "Floating Point Problem"
  it should "be OK" in {
    val x = Rational(1, 10) + Rational(2, 10)
    val y = x * 10 / 3
    y shouldBe unitySymbol
  }

  behavior of "toString"
  it should "be decimal when exact" in {
    val r = Rational(1, 2)
    r.toString shouldBe "1/2"
  }
  it should "be recurring when exact: 2/3" in {
    val r = Rational(2, 3)
    r.toString shouldBe "2/3"
  }
  it should "be decimal when not exact: pi" in {
    val pi = Rational(BigDecimal(math.Pi))
    pi.toString shouldBe "3141592653589793/1000000000000000"
  }
  it should "work for NaN" in {
    Rational.NaN.toString shouldBe "0/0"
  }
  it should "work for Infinity" in {
    Rational.infinity.toString shouldBe "1/0"
  }
  it should "work for negative Infinity" in {
    Rational.infinity.negate.toString shouldBe "-1/0"
  }
  // XXX this is the diagnostic for Issue #70
  it should "work for gamma" in {
    val n = BigInt("57721566490153286060651209008240243104215933593992")
    val d = bigTen.pow(50)
    val r = Rational(n, d)
    r.toString shouldBe "7215195811269160757581401126030030388026991699249/12500000000000000000000000000000000000000000000000"
  }
  it should "work for various prime denominators" in {
    import com.phasmidsoftware.number.core.Rational._

    (3 :/ 4 toString) shouldBe "3/4"
    (4 :/ 5 toString) shouldBe "4/5"
    (6 :/ 7 toString) shouldBe "6/7"
    (10 :/ 11 toString) shouldBe "10/11"
    (12 :/ 13 toString) shouldBe "12/13"
    (16 :/ 17 toString) shouldBe "16/17"
    (7918 :/ 7919 toString) shouldBe "7918/7919"
  }

  behavior of "render"
  it should "be decimal when exact" in {
    val r = Rational(1, 2)
    r.render shouldBe "0.5"
  }
  it should "be recurring when exact: 2/3" in {
    val r = Rational(2, 3)
    r.render shouldBe "0.<6>"
  }
  it should "be decimal when not exact: pi" in {
    val pi = Rational(BigDecimal(math.Pi))
    pi.render shouldBe "3.141592653589793"
  }
  it should "work for NaN" in {
    Rational.NaN.render shouldBe "NaN"
  }
  it should "work for Infinity" in {
    Rational.infinity.render shouldBe "+ve infinity"
  }
  it should "work for negative Infinity" in {
    Rational.infinity.negate.render shouldBe "-ve infinity"
  }
// XXX this is the diagnostic for Issue #70
  it should "work for gamma" in {
    val n = BigInt("57721566490153286060651209008240243104215933593992")
    val d = bigTen.pow(50)
    val r = Rational(n, d)
    // XXX why doesn't this end in a 2?
    r.render shouldBe "0.5772156649015328606065120900824024310421593359399"
  }
  it should "work for various prime denominators" in {
    import com.phasmidsoftware.number.core.Rational._

    (3 :/ 4 render) shouldBe "0.75"
    (4 :/ 5 render) shouldBe "0.8"
    (6 :/ 7 render) shouldBe "0.<857142>"
    (10 :/ 11 render) shouldBe "0.<90>"
    (12 :/ 13 render) shouldBe "0.<923076>"
    (16 :/ 17 render) shouldBe "0.<9411764705882352>"
    (7918 :/ 7919 render) shouldBe "7918/7919"
  }
  it should "work for various composite denominators" in {
    import com.phasmidsoftware.number.core.Rational._

    (1 :/ 6).render shouldBe "0.1<6>"
    (1 :/ 14 render) shouldBe "0.0<714285>"
  }

  behavior of "findRepeatingSequence"
  it should "fail for 4/5" in {
    findRepeatingSequence(4, 5, Seq(Prime(5))) should matchPattern { case Failure(_) => }
  }
  it should "work when denominator is prime" in {
    val r = Rational(17).invert
    val sequence = findRepeatingSequence(r.n, r.d, Seq(Prime(17)))
    sequence shouldBe Success("0.<0588235294117647>")
  }
  it should "work when numerator and denominator are prime 1" in {
    val r = Rational(2, 17)
    val sequence = findRepeatingSequence(r.n, r.d, Seq(Prime(17)))
    sequence shouldBe Success("0.<1176470588235294>")
  }
  it should "work when numerator and denominator are prime 2" in {
    val r = Rational(23, 17)
    val sequence = findRepeatingSequence(r.n, r.d, Seq(Prime(17)))
    sequence shouldBe Success("1.<3529411764705882>")
  }
  it should "work when denominator is composite 1" in {
    val r = Rational(1, 85) // 5 * 17
    val sequence = findRepeatingSequence(r.n, r.d, Seq(Prime(5), Prime(17)))
    sequence shouldBe Success("0.0<1176470588235294>")
  }
  it should "work when denominator is composite 2" in {
    val sequence = findRepeatingSequence(1, 119, Seq(Prime(7), Prime(17))) // 7 * 17
    sequence shouldBe Success("0.<008403361344537815126050420168067226890756302521>")
  }
  it should "fail when denominator has too many prime factors" in {
    findRepeatingSequence(1, 257, Seq(Prime(257))) should matchPattern { case Failure(NumberException("Rational.getPeriods: not yet implemented for: List(1, 2, 2, 2, 2, 2, 2, 2, 2)")) => }
  }

  behavior of "Rational(String)"
  it should "work for 0.1" in {
    val r = Rational("0.1")
    r shouldBe Rational(1, 10)
  }
  it should "work for 1.0e6" in {
    val r = Rational("1.0e6")
    r shouldBe Rational(10).power(6)
  }
  it should "convert Avagadro's number" in {
    val r = Rational("6.02214076E23")
    r.toBigInt shouldBe BigInt("602214076000000000000000")
  }

  behavior of "parse(String)"
  it should "work for 0.1" in {
    val r = Rational.parse("0.1")
    r shouldBe Success(Rational(1, 10))
  }
  it should "work for 1." in {
    val r = Rational.parse("1.")
    r shouldBe Success(Rational(1))
  }
  it should "work for 1.0e6" in {
    val r = Rational.parse("1.0e6")
    r shouldBe Success(Rational(10).power(6))
  }
  it should "work for 15699511928844194920/4294967295" in {
    val r = Rational.parse("15699511928844194920/4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "work for 15699511928844194920" in {
    val r = Rational.parse("15699511928844194920")
    r should matchPattern { case Success(Rational(b, Rational.bigOne)) if b == BigInt("15699511928844194920") => }
  }
  it should "work for -15699511928844194920/4294967295" in {
    val r = Rational.parse("-15699511928844194920/4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "work for -15699511928844194920/-4294967295" in {
    val r = Rational.parse("-15699511928844194920/-4294967295")
    r should matchPattern { case Success(Rational(_, _)) => }
  }
  it should "fail for x" in {
    val r = Rational.parse("x")
    r should matchPattern { case Failure(_) => }
  }

  behavior of "edu/neu/coe/csye7200/sorting"
  it should "work" in {
    val r = List(Rational(1, 2), Rational(2, 3), Rational(1, 3))
    val x = r.sorted
    x.head shouldBe Rational(1, 3)
    x.tail.head shouldBe Rational(1, 2)
    x.tail.tail.head shouldBe Rational(2, 3)
  }

  behavior of "r-interpolator"
  it should "work for -1/0" in {
    val r = r"-1/0"
    r.isInfinity shouldBe true
    r shouldBe Rational(-1, 0)
  }
  it should "work for 1/-2147483648" in {
    val r = r"1/-2147483648"
    r.signum shouldBe -1
  }
  it should "work for -1/-2147483648" in {
    val r = r"-1/-2147483648"
    r.signum shouldBe 1
  }

  behavior of "toInt"
  it should "work for Int.MaxValue" in {
    val target = Rational(Int.MaxValue)
    target.toInt shouldBe Int.MaxValue
  }
  it should "work for Int.MinValue" in {
    val target = Rational(Int.MinValue)
    target.toInt shouldBe Int.MinValue
  }
  it should "fail for Int.MaxValue+1" in {
    val target = Rational(Int.MaxValue) + 1
    a[RationalException] shouldBe thrownBy(target.toInt)
  }
  it should "fail for Int.MinValue-1" in {
    val target = Rational(Int.MinValue) - 1
    a[RationalException] shouldBe thrownBy(target.toInt)
  }

  // Test Private Methods...
  behavior of "narrow(BigInt)"
  it should "work for Int.MaxValue" in {
    val b = BigInt(Int.MaxValue)
    val decorateNarrow = PrivateMethod[Try[BigInt]](narrowSymbol)
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(b, BigInt(Int.MinValue), BigInt(Int.MaxValue))
    z should matchPattern { case Success(x) if x == Int.MaxValue => }
  }
  it should "not work for Int.MaxValue+1" in {
    val b = BigInt(Int.MaxValue) + 1
    val decorateNarrow = PrivateMethod[Try[BigInt]](narrowSymbol)
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(b, BigInt(Int.MinValue), BigInt(Int.MaxValue))
    z should matchPattern { case Failure(_) => }
  }

  behavior of "narrow(Rational)"
  it should "work for Int.MaxValue" in {
    val r = Rational(Int.MaxValue)
    val decorateNarrow = PrivateMethod[Try[BigInt]](narrowSymbol)
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(r, BigInt(Int.MinValue), BigInt(Int.MaxValue))
    z should matchPattern { case Success(x) if x == Int.MaxValue => }
  }
  it should "work for Int.MaxValue+1" in {
    val r = Rational(Int.MaxValue) + 1
    val decorateNarrow = PrivateMethod[Try[BigInt]](narrowSymbol)
    val z: Try[BigInt] = Rational invokePrivate decorateNarrow(r, BigInt(Int.MinValue), BigInt(Int.MaxValue))
    z should matchPattern { case Failure(_) => }
  }

  behavior of "normalize"
  it should "work for 0,0" in {
    val decorateNormalize = PrivateMethod[Rational](normalizeSymbol)
    val z = Rational invokePrivate decorateNormalize(Rational.bigZero, Rational.bigZero)
    z should matchPattern { case Rational(x, y) if x == Rational.bigZero && y == 0L => }
  }
  it should "work for 1,0" in {
    val decorateNormalize = PrivateMethod[Rational](normalizeSymbol)
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne, Rational.bigZero)
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 0L => }
  }
  it should "work for 1,1" in {
    val decorateNormalize = PrivateMethod[Rational](normalizeSymbol)
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne, Rational.bigOne)
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 1L => }
  }
  it should "work for 2,2" in {
    val decorateNormalize = PrivateMethod[Rational](normalizeSymbol)
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne * 2, BigInt(2))
    z should matchPattern { case Rational(x, y) if x == Rational.bigOne && y == 1L => }
  }
  it should "work for 3,5" in {
    val decorateNormalize = PrivateMethod[Rational](normalizeSymbol)
    val z = Rational invokePrivate decorateNormalize(Rational.bigOne * 3, BigInt(5))
    z should matchPattern { case Rational(x, y) if x == BigInt(3) && y == 5L => }
  }

  behavior of "compare"
  it should "work for 0,0" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(Rational.zero, Rational.zero)
    z should matchPattern { case 0 => }
  }
  it should "work for 0,1" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(Rational.zero, Rational.one)
    z should matchPattern { case -1 => }
  }
  it should "work for 1,0" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(Rational.one, Rational.zero)
    z should matchPattern { case 1 => }
  }
  it should "work for 1,1" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(Rational.one, Rational.one)
    z should matchPattern { case 0 => }
  }
  it should "work for inf,inf" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(Rational.infinity, Rational.infinity)
    z should matchPattern { case 0 => }
  }
  it should "work for -inf,inf" in {
    val decorateCompare = PrivateMethod[Int](compareSymbol)
    val z = Rational invokePrivate decorateCompare(-Rational.infinity, Rational.infinity)
    z should matchPattern { case 0 => }
  }

  behavior of "mediant"
  it should "work" in {
    Rational.zero mediant Rational.one shouldBe Rational.half
    Rational.one mediant Rational.zero shouldBe Rational.half
    Rational.zero mediant Rational.half shouldBe Rational(1, 3)
    Rational.half mediant Rational.zero shouldBe Rational(1, 3)
    Rational(1, 3) mediant Rational(3, 2) shouldBe Rational(4, 5)
    Rational(0, 1) mediant Rational.infinity shouldBe Rational.NaN
    Rational.infinity mediant Rational.zero shouldBe Rational.NaN
  }

  behavior of "approximate"
  it should "work" in {
    Rational.approximate(1.0 / 2) shouldBe Rational.half
    Rational.approximate(1.0 / 3) shouldBe Rational(1, 3)
    Rational.approximate(3.0 / 4) shouldBe Rational(3, 4)
  }

  it should "fail" in {
    a[IllegalArgumentException] should be thrownBy Rational.approximate(Math.PI)
    a[IllegalArgumentException] should be thrownBy Rational.approximate(-0.5)
  }

  behavior of "approximateAny"
  it should "work for specific epsilon" in {
    implicit val epsilon: Tolerance = Tolerance(1E-7, BigInt(1000000000))
    Rational.approximateAny(Math.PI) shouldBe Rational(75948, 24175)
  }
  // NOTE: this test works but it is very slow. It should be checked from time to time.
  ignore should "work for 3.1416" in {
    Rational.approximateAny(3.1416) shouldBe Rational(3141600355L, 1000000113)
  }

  behavior of "convertDouble"
  it should "work" in {
    Rational.convertDouble(1.0 / 2) shouldBe Rational.half
    Rational.convertDouble(-5.0 / 4) shouldBe Rational(-5, 4)
    Rational.convertDouble(Math.PI).toDouble shouldBe Math.PI +- 1E-15
    Rational.convertDouble(6.02214076E23).toDouble shouldBe 6.02214076E23 +- 1E9
  }

  behavior of "sqrt"
  it should "work correctly" in {
    Rational.one.sqrt shouldBe Success(Rational.one)
    Rational(4).sqrt shouldBe Success(Rational.two)
    Rational(9, 4).sqrt shouldBe Success(Rational(3, 2))
  }

  behavior of "root"
  it should "work for sqrt(4)" in {
    val b = BigInt(4)
    Rational.root(b, 2) shouldBe Some(2)
  }

  behavior of "RationalOps"

  import com.phasmidsoftware.number.core.Rational.RationalOps

  it should "work correctly for 2 + 3" in {
    // NOTE: here we perform integer + on 2 and 3 and then implicitly convert 5 to a Rational
    val r: Rational = 2 + 3
    r shouldBe Rational(5)
  }

  it should "work correctly for 2 + Rational(3)" in {
    // NOTE: here we convert 2 to RationalOps and then invoke the RationalOps.+ operator.
    // NOTE: because we have imported RationalOps, we do not need to provide a type annotation of Rational for r.
    val r = 2 + Rational(3)
    r shouldBe Rational(5)
  }

  it should "work correctly for 2 * 3" in {
    val r: Rational = 2 * 3
    r shouldBe Rational(6)
  }

  it should "work correctly for 2 * Rational(3)" in {
    val r = 2 * Rational(3)
    r shouldBe Rational(6)
  }

  it should "work correctly for 2:/3" in {
    // NOTE: here we convert 2 to RationalOps and then invoke the RationalOps.:/ operator.
    // NOTE: because we have imported RationalOps, we do not need to provide a type annotation of Rational for r.
    val r = 2 :/ 3
    r shouldBe Rational(2, 3)
  }

  it should "work correctly (but somewhat counter-intuitively) for 2/3" in {
    // NOTE: here we perform integer / on 2 and 3 and then implicitly convert 0 to a Rational
    val r: Rational = 2 / 3
    r shouldBe Rational.zero
  }

  behavior of "negZero"
  it should "add to zero correctly" in {
    Rational.negZero + Rational.zero shouldBe Rational.zero
  }
  it should "add to one correctly" in {
    Rational.negZero + Rational.one shouldBe Rational.one
  }

  behavior of "Rat..."
  it should "parse a String" in {
    implicitly[Numeric[Rational]].parseString("3.1415927") shouldBe Some(Rational(31415927, 10000000))
  }
  it should "convert from an Int" in {
    implicitly[Numeric[Rational]].fromInt(299792458) shouldBe Rational(299792458, 1)
  }
  it should "convert to an Int" in {
    implicitly[Numeric[Rational]].toInt(Rational(299792458)) shouldBe 299792458
  }
  it should "convert to a Long" in {
    implicitly[Numeric[Rational]].toLong(Rational(299792458000L)) shouldBe 299792458000L
  }
  it should "convert to a Float" in {
    implicitly[Numeric[Rational]].toFloat(Rational(3.1415927)) shouldBe 3.1415927f
  }
  it should "convert infinity to a Double" in {
    implicitly[Numeric[Rational]].toDouble(Rational.infinity) shouldBe Double.PositiveInfinity
  }
}
