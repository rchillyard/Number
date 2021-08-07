package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers._
import com.phasmidsoftware.number.core.Field.convertToNumber
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  val sb = new StringBuilder
  implicit val logger: MatchLogger = SBLogger(LogOff, sb)
  val sbLogger: MatchLogger = SBLogger(LogDebug, sb)

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    if (sbLogger.logLevel != com.phasmidsoftware.matchers.LogOff)
      println("===============================\n")
  }

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  val em: ExpressionMatchers = Expression.em
  val eml: ExpressionMatchers = new ExpressionMatchers()(sbLogger) {}
  private val two: Number = Number(2)
  private val one: Number = Number.one
  private val zero: Number = Number.zero
  private val half: Number = convertToNumber(Number.two.invert)


  behavior of "value"
  it should "work with value on Literal" in {
    val f: em.ExpressionMatcher[Field] = em.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = em.value
    f(Literal(FuzzyNumber(Right(1), Scalar, None))).successful shouldBe true
  }
  it should "fail on non-value" in {
    val x = Expression(1) * 2
    val f = em.value
    f(x).successful shouldBe false
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: em.ExpressionMatcher[Field] = em.matchValue(one)
    val e = Literal(one)
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = em.matchValue(one)
    val g = f | em.matchValue(Number.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Number.pi)).successful shouldBe true
    g(Literal(Number.e)).successful shouldBe false
  }

  import com.phasmidsoftware.number.core.Expression.ExpressionOps

  behavior of "gathering operations"
  it should "gather 2 and * 1/2" in {
    val x: Expression = Literal(Number(7))
    val y = x.sqrt
    val z = y ^ 2
    val q = em.simplifier(z)
    q should matchPattern { case em.Match(_) => }
    // TODO eliminate simplify here and everywhere
    val result = q.get.simplify
    result shouldBe Literal(7)
  }

  behavior of "matchBiFunctionConstantResult"
  it should "match 1" in {
    val negativeOne: Number = Number(-1)
    val q = em.matchBiFunctionConstantResult(Product, MinusOne, Zero)
    import em.TildeOps
    val r: em.MatchResult[Expression] = q(One ~ BiFunction(One, MinusOne, Product))
    r.successful shouldBe true
    r.get shouldBe Zero
  }

  behavior of "matchEitherDyadic"
  it should "match (1, biFunction)" in {
    val negativeOne: Number = Number(-1)
    val p = em.matchEitherDyadic(commutes = true)
    import em.TildeOps
    val r: em.MatchResult[BiFunction ~ Expression] = p(One ~ BiFunction(One, MinusOne, Product))
    r.successful shouldBe true
  }
  it should "match (biFunction, 1)" in {
    val negativeOne: Number = Number(-1)
    val p = em.matchEitherDyadic(commutes = true)
    import em.TildeOps
    val r: em.MatchResult[BiFunction ~ Expression] = p(BiFunction(One, MinusOne, Product) ~ One)
    r.successful shouldBe true
  }

  behavior of "matchOffsetting"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = em.matchBiFunction & em.matchDyadicBranches(Sum) & em.matchSumOffsetting
    val result = p(z)
    result should matchPattern { case em.Match(Zero) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    val p = em.matchBiFunction & em.matchDyadicBranches(Sum) & em.matchSumOffsetting
    val result = p(z)
    result should matchPattern { case em.Match(Zero) => }
  }

  behavior of "simplifier"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = em.simplifier
    val result = p(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    val p = em.simplifier
    val result = p(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = MinusOne * x
    val z = x + y
    val result = em.simplifier(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1 b" in {
    sb.append("cancel -1 and - 1 b:\n")
    val x: Expression = Expression.one
    val y = MinusOne * x
    val z = y + x
    val result = em.simplifier(z) map (_.materialize)
    result should matchPattern { case em.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe One
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe One
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z = y.simplify
    val q = z.simplify
    q shouldBe Literal(7)
  }
  it should "show that lazy evaluation only works when you use it" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = x doPower two
    y should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "show ^2 and sqrt for illustrative purposes" in {
    val seven = Number(7)
    val x = seven.sqrt
    val y: Expression = Literal(x) ^ two
    val result = convertToNumber(y.materialize)
    result.isExact shouldBe false
    result shouldEqual Number(7)
  }
  it should "cancel addition and subtraction" in {
    val x = One + 3 - 3
    val q = x.simplify
    q shouldBe One
  }
  it should "cancel multiplication and division" in {
    val x = Literal(Number.pi) * 2 / 2
    val simplify = x.simplify
    simplify shouldBe ConstPi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Literal(Number.pi) / 2 * 2
    val simplify = x.simplify
    simplify shouldBe ConstPi
  }


  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get shouldBe seven
  }
  it should "work for products" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven * 2
    val y = x * 3
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Number(42)
  }
  it should "work for sums" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Number(12)
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (Literal(Number.e) * 2).materialize.toString shouldBe "5.436563656918090[57]"
  }

  behavior of "matchSimplifyProductIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyProductIdentity
    import em.TildeOps
    p(Product ~ Two ~ One) shouldBe em.Match(Two)
    p(Product ~ One ~ Two) shouldBe em.Match(Two)
  }

  behavior of "matchSimplifySumIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifySumIdentity
    import em.TildeOps
    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
  }

  behavior of "matchSimplifyPowerIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyPowerIdentity
    import em.TildeOps
    p(Power ~ Two ~ One) shouldBe em.Match(Two)
    p(Power ~ One ~ Two) shouldBe em.Match(One)
  }

  behavior of "biFunctionSimplifier"
  it should "simplify" in {
    val p = em.biFunctionSimplifier
    p(BiFunction(Two, Zero, Sum)) shouldBe em.Match(Two)
    p(BiFunction(Zero, Two, Sum)) shouldBe em.Match(Two)
    p(BiFunction(Two, One, Product)) shouldBe em.Match(Two)
    p(BiFunction(One, Two, Product)) shouldBe em.Match(Two)
    p(BiFunction(Two, One, Power)) shouldBe em.Match(Two)
    p(BiFunction(One, Two, Power)) shouldBe em.Match(One)
  }
  // FIXME Issue #55
  it should "evaluate (√3 + 1)(√3 - 1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 - 1)
    val p = em.biFunctionSimplifier
    val q: em.MatchResult[Expression] = p(x)
    println(q)
    q.successful shouldBe true
  }
  it should "evaluate (√3 + 1)(√3 + -1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 + MinusOne)
    val p = em.biFunctionSimplifier
    val q: em.MatchResult[Expression] = p(x)
    println(q)
    q.successful shouldBe true
  }
  // NOTE: this will succeed only if we allow simplifications which reduce depth (but are not necessarily exact)
  // FIXME problem with distributePowerPower
  ignore should "simplify 1" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val z = p(x)
    z shouldBe em.Match(BiFunction(Literal(3), Literal(0.5), Power))
  }

  behavior of "matchBiFunctionConstantResult"
  it should "simplify" in {
    val p: em.Matcher[em.Expressions, Expression] = em.matchBiFunctionConstantResult(Product, MinusOne, Zero)
    import em.TildeOps
    p(BiFunction(Two, MinusOne, Product) ~ Two) shouldBe em.Match(Zero)
    p(Two ~ BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Zero)
    p(BiFunction(MinusOne, Two, Product) ~ Two) shouldBe em.Match(Zero)
    p(Two ~ BiFunction(MinusOne, Two, Product)) shouldBe em.Match(Zero)
  }

  behavior of "simplifyProduct"
  it should "work for root(3) * root(3)" in {
    val x = Expression(3).sqrt
    val z = em.simplifyProduct(x, x)
    z.isExact shouldBe true
  }

  behavior of "biFunctionSimplifier"
  it should "simplify 1 + 2 - 2" in {
    val p = em.biFunctionSimplifier
    val x = One plus Two - Two
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }
  it should "properly simplify 1 + 2 - 2 + 0" in {
    val p = em.biFunctionSimplifier
    val x = One plus Two - Two + Zero
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }

  behavior of "matchSimplifySum"
  // FIXME Issue #55
  ignore should "properly simplify 1 + root3 - root3 + 0" in {
    val p = em.matchDyadicTwoLevels
    val z: Expression = Literal(3).sqrt
    val x = z plus -z + Zero
    import em.TildeOps
    val r = p(Sum ~ One ~ x)
    r.successful shouldBe true
    r.get shouldBe One
  }
  // FIXME Issue #55
  ignore should "properly simplify (1 + root3) + (zero - root3)" in {
    val p = em.biFunctionMatcher
    val root3 = Number(3).sqrt
    val x: Expression = One + root3
    val z = Zero - root3
    import em.TildeOps
    val r = p(Sum ~ x ~ z)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }

  behavior of "matchSimplifyProduct"
  // FIXME Issue #55
  ignore should "properly simplify 1 * (root3 / root3 * 3)" in {
    val p = em.biFunctionMatcher
    val z: Expression = Literal(3).sqrt
    val x = z * z.reciprocal * Number(3)
    import em.TildeOps
    val r = p(Product ~ One ~ x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe Literal(3)
  }
  // FIXME Issue #55
  ignore should "properly simplify (1 * root3) * (3 / root3)" in {
    val p = em.biFunctionMatcher
    val root3 = Literal(3).sqrt
    val x: Expression = One * root3
    val z = Literal(3) * root3.reciprocal
    import em.TildeOps
    val r = p(Product ~ x ~ z)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe Literal(3)
  }
  // FIXME Issue #55
  ignore should "simplify e * 2 / 2" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val e: Number = Number.e
    val x: Expression = Literal(e) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(e)
  }
  it should "simplify root3 * 2 / 2" in {
    val p = em.matchSimplifyProduct
    import em.TildeOps
    val root3: Number = Number(3).sqrt
    val x: Expression = Literal(root3) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(Literal(root3))
  }
  it should "simplify root4 * 2 / 2" in {
    val p = em.matchSimplifyProduct
    import em.TildeOps
    val root4: Number = Number(4).sqrt
    val x = Literal(root4) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(Literal(root4))
  }
  // FIXME Issue #55
  ignore should "simplify e * 2 / 2 * 1" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val x = Expression(Number.e) * Number.two
    val y: Expression = Expression(Number.two).reciprocal * One
    p(Product ~ x ~ y) shouldBe em.Match(Number.e)
  }

  behavior of "distributor"
  it should "distribute" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Literal(9))
  }
  // FIXME this wos working just a few minutes ago :(
  ignore should "distributeProductSum" in {
    val p = em.matchAndCancelTwoDyadicLevels
    val q = em.matchBiFunction
    val a = BiFunction(Two, Literal(3), Sum)
    val b = BiFunction(Literal(4), Two, Sum)
    import em.TildeOps
    val z = p(Sum ~ q(a).get ~ q(b).get)
    z shouldBe em.Match(Literal(30))
  }
  it should "distributeProductSum a" in {
    val p = em.distributeProductSum
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    import em.TildeOps
    val z = p(a ~ b)
    z shouldBe em.Match(Literal(9))
  }
  it should "distributeProductSum b" in {
    val p = em.distributeProductSum
    val x = Number("2.00")
    val y = Number("3.00")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    import em.TildeOps
    val z = p(a ~ b)
    z shouldBe em.Match(Literal(10.5))
  }
  // NOTE This expression won't simplify because it is inexact (and distribution doesn't reduce the depth).
  ignore should "distributeProductSum c" in {
    val p = em.distributeProductSum
    val x = Number("2.00*")
    val y = Number("3.00*")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    import em.TildeOps
    val z = p(a ~ b)
    val eo = Expression.parse("( ( 3.00* + 0.5 ) + ( 2.00* * ( 3.00* + 0.5 ) ) )")
    eo map (z shouldBe em.Match(_)) orElse fail("could not parse expression")
  }
  // FIXME Issue #55
  ignore should "distributeProductSum d" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val y = -x
    val a = BiFunction(One, x, Sum)
    val b = BiFunction(One, y, Sum)
    val r = BiFunction(a, b, Product)
    val z: em.MatchResult[Expression] = p(r)
    val k: em.MatchResult[Expression] = z map (_.simplify)
    k shouldBe em.Match(Literal(-2))
  }
  it should "distributePowerProduct" in {
    val p = em.distributePowerProduct
    val x = Expression(3).sqrt
    val a = BiFunction(Two, x, Product)
    import em.TildeOps
    val z = p(a ~ Two)
    z shouldBe em.Match(Literal(12))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    val p = em.distributeProductPower
    val x = Expression(3).sqrt
    import em.TildeOps
    val z = x ~ x
    val q: em.MatchResult[Expression] = p(z)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Number(3)
  }

  behavior of "simplifyProduct"
  // FIXME Issue #55
  ignore should "work" in {
    val xo = Expression.parse("( 3 ^ ( 2 ^ -1 ) )")
    val yo = Expression.parse("( ( 3 ^ ( 2 ^ -1 ) ) * -1 )")
    val zo = for (x <- xo; y <- yo) yield em.simplifier(x * y)
    zo should matchPattern { case Some(_) => }
    zo.get shouldBe BiFunction(Literal(3), Literal(-1), Product)
  }

  behavior of "gathering operations"
  it should "gather powers of 2 and * 1/2" in {
    val x: Expression = Literal(7)
    val y = x.sqrt
    val z = y ^ 2
    z.materialize shouldBe Number(7)
  }

  behavior of "canceling operations"
  it should "cancel 1 and - -1" in {
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val simplify = z.simplify
    simplify shouldBe Zero
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    z.simplify shouldBe One
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    z.simplify shouldBe One
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z = y.simplify
    z.simplify shouldBe Literal(7)
  }
  it should "show that lazy evaluation only works when you use it" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = Literal(x) ^ 2
    y.materialize should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "show ^2 and sqrt for illustrative purposes" in {
    val seven = Number(7)
    val x = Literal(seven.sqrt)
    val y = convertToNumber((x ^ 2).materialize)
    y should matchPattern { case FuzzyNumber(_, _, _) => }
    y shouldEqual Number(7)
  }
  it should "cancel addition and subtraction" in {
    val x = One + 3 - 3
    val q = x.simplify
    q shouldBe One
  }
  // FIXME Issue #55
  ignore should "cancel multiplication and division" in {
    val x = Literal(Number.e) * 2 / 2
    val result: Number = x
    println(x)
    result shouldBe Number.e
  }

  behavior of "value with logging"
  it should "work with value on Literal" in {
    import eml.MatcherOps
    implicit val logger: MatchLogger = eml.matchLogger
    val f = eml.value :| "value"
    f(Literal(one)).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on One" in {
    import eml.MatcherOps
    implicit val logger: MatchLogger = eml.matchLogger
    val f = eml.value :| "value"
    f(One).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on Number.one" in {
    import eml.MatcherOps
    implicit val logger: MatchLogger = eml.matchLogger
    val f = eml.value :| "value"
    f(One).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on FuzzyNumber" in {
    import eml.MatcherOps
    implicit val logger: MatchLogger = eml.matchLogger
    val f = eml.value :| "value"
    f(Literal(FuzzyNumber(Right(1), Scalar, None))).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "fail on non-value" in {
    val x = Expression(1) * 2
    import eml.MatcherOps
    implicit val logger: MatchLogger = eml.matchLogger
    val f = eml.value :| "value"
    f(x).successful shouldBe false
    sb.toString shouldBe "trying matcher value on (1 * 2)...\n... value((1 * 2)): Miss: value: (1 * 2)\n"
  }

  behavior of "matchSimplifyBiFunction"

  import com.phasmidsoftware.matchers.Matchers.matchers.TildeOps

  it should "simplify 1 + 1" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Sum ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe Two
  }
  it should "simplify 1 + 0" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Sum ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "fail to simplify 1 + pi" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Sum ~ One ~ ConstPi)
    r.successful shouldBe false
  }
  it should "simplify 1 + -1" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Sum ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * 1" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Product ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "simplify 1 * 0" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Product ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * -1" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Product ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe MinusOne
  }
  it should "simplify 2 ^ -1" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Power ~ Two ~ MinusOne)
    r.successful shouldBe true
    import Rational.RationalOps
    r.get shouldBe Literal(Number(1 :/ 2))
  }
  it should "fail to simplify 2 ^ 1/2" in {
    val r: em.MatchResult[Expression] = em.matchSimplifyBiFunction(Power ~ Two ~ Literal(Rational.half))
    r.successful shouldBe false
  }

  behavior of "matchSimplifyBiFunction"

  import com.phasmidsoftware.matchers.Matchers.matchers.TildeOps

  it should "simplify E" in {
    val r: em.MatchResult[Expression] = em.evaluateMonadicDuple(Exp ~ One)
    r.successful shouldBe true
    r.get shouldBe ConstE
  }

  // FIXME this was working just yesterday
  ignore should "simplify multiple similar ops" in {
    em.simplifier(Expression(2) * 3 * Number.e * 5) shouldBe em.Match(ConstE * 30)
    // TODO we would like the following to be ConstE * 30
    //    em.simplifier(ConstE * 2 * 3 * 5) shouldBe em.Match(ConstE * 2 * 15)
    em.simplifier(Expression(5) * 2 * 3 * Number.e) shouldBe em.Match(ConstE * 30)
  }

  behavior of "matchAndCancelTwoDyadicLevels"
  it should "work for √3 * √3" in {
    val p = em.matchAndCancelTwoDyadicLevels
    val result: em.MatchResult[Expression] = p(Product ~ (Power ~ Literal(3) ~ Literal(Rational.half)) ~ (Power ~ Literal(3) ~ Literal(Rational.half)))
    result.successful shouldBe true
    result.get shouldBe Literal(3)
  }
  it should "work for (π + 1) * (π - 1)" in {
    val p = em.matchAndCancelTwoDyadicLevels
    val root3: Expression = Expression(3).sqrt
    val result: em.MatchResult[Expression] = p(Sum ~ (Product ~ root3 ~ One) ~ (Product ~ root3 ~ MinusOne))
    result.successful shouldBe true
    result.get shouldBe Zero
  }

}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () })

