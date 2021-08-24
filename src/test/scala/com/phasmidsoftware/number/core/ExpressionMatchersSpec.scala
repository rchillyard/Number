package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers._
import com.phasmidsoftware.number.core.Expression.em.DyadicTriple
import com.phasmidsoftware.number.core.Field.convertToNumber
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

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

  val em: ExpressionMatchers = Expression.em
  val eml: ExpressionMatchers = new ExpressionMatchers()(sbLogger) {}
  private val two: Number = Number(2)
  private val one: Number = Number.one
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

  behavior of "evaluateExactDyadicTriple"
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val p = em.evaluateExactDyadicTriple
    import em.TildeOps
    val result = p(Sum ~ y ~ x)
    result should matchPattern { case em.Match(Zero) => }
  }

  behavior of "simplifier"
  it should "cancel 1 and - -1 (q)" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = em.simplifier
    val result = p(z)
    result should matchPattern { case em.Match(Zero) => }
  }
  it should "simplify sqrt(7)^2" in {
    val x: Expression = Literal(Number(7))
    val y = x.sqrt
    val z = y ^ 2
    val q = em.simplifier(z)
    q shouldBe em.Match(Literal(7))
  }
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
  it should "cancel 2 * 1/2 (a)" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe One
  }
  it should "cancel 2 * 1/2 (b)" in {
    val x = Literal(2) * Expression.one
    val y = x.reciprocal
    val z = y * x
    val result = em.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe One
  }
  it should "cancel ^2 and sqrt for 7" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val q = em.simplifier(y)
    q shouldBe em.Match(Literal(7))
  }
  it should "show that lazy evaluation only works when you use it (a)" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = x doPower two
    y should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "cancel addition and subtraction (a)" in {
    val x = One + 3 - 3
    em.simplifier(x) shouldBe em.Match(One)
  }
  it should "cancel multiplication and division" in {
    val x = Literal(Number.pi) * 2 / 2
    em.simplifier(x) shouldBe em.Match(ConstPi)
  }
  it should "cancel multiplication and division backwards" in {
    val x = Literal(Number.pi) / 2 * 2
    em.simplifier(x) shouldBe em.Match(ConstPi)
  }
  it should "cancel 1 and - -1 (a)" in {
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    em.simplifier(z) shouldBe em.Match(Zero)
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    em.simplifier(z) shouldBe em.Match(One)
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    em.simplifier(z) shouldBe em.Match(One)
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    em.simplifier(y) shouldBe em.Match(Literal(7))
  }
  it should "cancel addition and subtraction" in {
    val x = One + 3 - 3
    em.simplifier(x) shouldBe em.Match(One)
  }

  behavior of "materialize (a)"
  it should "show that lazy evaluation only works when you use it" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = Literal(x) ^ 2
    y.materialize should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  it should "show ^2 and sqrt for illustrative purposes (a)" in {
    val seven = Number(7)
    val x = seven.sqrt
    val y: Expression = Literal(x) ^ two
    val result = convertToNumber(y.materialize)
    result.isExact(None) shouldBe false
    result shouldEqual Number(7)
  }
  it should "show ^2 and sqrt for illustrative purposes" in {
    val seven = Number(7)
    val x = Literal(seven.sqrt)
    val y = convertToNumber((x ^ 2).materialize)
    y should matchPattern { case FuzzyNumber(_, _, _) => }
    y shouldEqual Number(7)
  }
  it should "evaluate E * 2" in {
    (Literal(Number.e) * 2).materialize.toString shouldBe "5.436563656918091[15]"
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

  behavior of "matchDyadicTrivial"
  it should "eliminate 1" in {
    val p = em.matchDyadicTrivial
    import em.TildeOps
    p(Product ~ Two ~ One) shouldBe em.Match(Two)
    p(Product ~ One ~ Two) shouldBe em.Match(Two)
  }
  it should "eliminate 0" in {
    val p = em.matchDyadicTrivial
    import em.TildeOps
    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
  }
  it should "eliminate 1 in power" in {
    val p = em.matchDyadicTrivial
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
  it should "evaluate (√3 + 1)(√3 - 1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 - 1)
    val p = em.biFunctionSimplifier
    val q: em.MatchResult[Expression] = p(x)
    q.successful shouldBe true
    q.get shouldBe Literal(2)
  }
  it should "evaluate (√3 + 1)(√3 + -1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 + MinusOne)
    val p = em.biFunctionSimplifier
    val q: em.MatchResult[Expression] = p(x)
    println(q)
    q.successful shouldBe true
  }
  // FIXME Issue #55
  ignore should "biFunctionSimplifier on (1 + √3 + 1)(1 - √3)" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val y = -x
    val a = BiFunction(One, x, Sum)
    val b = BiFunction(One, y, Sum)
    val r = BiFunction(a, b, Product)
    val z: em.MatchResult[Expression] = p(r)
    val k: em.MatchResult[Expression] = z flatMap em.simplifier
    k shouldBe em.Match(Literal(-2))
  }
  // NOTE: this will succeed only if we allow simplifications which reduce depth (but are not necessarily exact)
  // FIXME problem with distributePowerPower
  ignore should "simplify 1" in {
    val p = em.biFunctionSimplifier
    val x = Expression(3).sqrt
    val z = p(x)
    z shouldBe em.Match(BiFunction(Literal(3), Literal(0.5), Power))
  }
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

  behavior of "matchAndCollectTwoDyadicLevels"
  it should "simplify e * 2 / 2 direct" in {
    val p = em.matchTwoDyadicLevels & em.matchAndCollectTwoDyadicLevels
    import em.TildeOps
    val e: Number = Number.e
    val x: Expression = Literal(e) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    val result = p(Product ~ x ~ y)
    result.successful shouldBe true
    result.get shouldBe ConstE
  }

  behavior of "biFunctionMatcher"
  // FIXME Issue #57
  ignore should "properly simplify 1 + root3 - root3 + 0" in {
    val p = em.biFunctionMatcher
    val z: Expression = Literal(3).sqrt
    val x = z plus -z + Zero
    import em.TildeOps
    val r = p(Sum ~ One ~ x)
    println(r)
    r.successful shouldBe true
    r.get shouldBe One
  }
  // FIXME Issue #57
  ignore should "properly simplify (1 + root3) + (zero - root3)" in {
    val p = em.biFunctionMatcher
    val root3 = Number(3).sqrt
    val x: Expression = One + root3
    val z = Zero - root3
    import em.TildeOps
    val expression: DyadicTriple = Sum ~ x ~ z
    val r = p(expression)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }
  it should "simplify (1+2)*(2+1)" in {
    val p = em.biFunctionMatcher
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    import em.TildeOps
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Literal(9))
  }
  // FIXME Issue #57
  ignore should "properly simplify 1 * (root3 / root3 * 3)" in {
    val p = em.biFunctionMatcher
    val z: Expression = Literal(3).sqrt
    val x = z * z.reciprocal * Number(3)
    import em.TildeOps
    val r = p(Product ~ One ~ x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe Literal(3)
  }
  // FIXME Issue #57
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
  // FIXME Issue #47
  ignore should "simplify e * 2 / 2" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val e: Number = Number.e
    val x: Expression = Literal(e) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(e)
  }
  it should "simplify root3 * 2 / 2" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val root3: Number = Number(3).sqrt
    val x: Expression = Literal(root3) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(Literal(root3))
  }
  it should "simplify root4 * 2 / 2" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val root4: Number = Number(4).sqrt
    val x = Literal(root4) * Number.two
    val y: Expression = Expression(Number.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(Literal(root4))
  }
  it should "distribute" in {
    val p = em.biFunctionMatcher
    import em.TildeOps
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Literal(9))
  }
  it should "distributeProductSum a" in {
    val p = em.biFunctionMatcher
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    import em.TildeOps
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Literal(9))
  }
  it should "distributeProductSum b" in {
    val p = em.biFunctionMatcher
    val x = Number("2.00")
    val y = Number("3.00")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    import em.TildeOps
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Literal(10.5))
  }
  // NOTE This expression won't simplify because it is inexact (and distribution doesn't reduce the depth).
  ignore should "distributeProductSum c" in {
    val p = em.biFunctionMatcher
    val x = Number("2.00*")
    val y = Number("3.00*")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    import em.TildeOps
    val z = p(Product ~ a ~ b)
    val eo = Expression.parse("( ( 3.00* + 0.5 ) + ( 2.00* * ( 3.00* + 0.5 ) ) )")
    eo map (z shouldBe em.Match(_)) orElse fail("could not parse expression")
  }
  // FIXME Issue #55
  ignore should "simplify 2 root(3) all squared" in {
    val p = em.biFunctionMatcher
    val x = Expression(3).sqrt
    val a = BiFunction(Two, x, Product)
    import em.TildeOps
    val z = p(Power ~ a ~ Two)
    z shouldBe em.Match(Literal(12))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    val p = em.biFunctionMatcher
    val x = Expression(3).sqrt
    import em.TildeOps
    val q: em.MatchResult[Expression] = p(Product ~ x ~ x)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Number(3)
  }
  // FIXME Issue #57
  ignore should "work" in {
    val xo = Expression.parse("( 3 ^ ( 2 ^ -1 ) )")
    val yo = Expression.parse("( ( 3 ^ ( 2 ^ -1 ) ) * -1 )")
    val zo = for (x <- xo; y <- yo) yield em.biFunctionSimplifier(x * y)
    zo should matchPattern { case Some(_) => }
    zo.get shouldBe BiFunction(Literal(3), Literal(-1), Product)
  }
  it should "gather powers of 2 and * 1/2" in {
    val x: Expression = Literal(7)
    val y = x.sqrt
    val z = y ^ 2
    em.biFunctionSimplifier(z) shouldBe em.Match(Literal(7))
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

  behavior of "biFunctionMatcher (2)"

  import com.phasmidsoftware.matchers.Matchers.matchers.TildeOps

  it should "simplify 1 + 1" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Sum ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe Two
  }
  it should "simplify 1 + 0" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Sum ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "fail to simplify 1 + pi" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Sum ~ One ~ ConstPi)
    r.successful shouldBe true
  }
  it should "simplify 1 + -1" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Sum ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * 1" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Product ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "simplify 1 * 0" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Product ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * -1" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Product ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe MinusOne
  }
  it should "simplify 2 ^ -1" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Power ~ Two ~ MinusOne)
    r.successful shouldBe true
    import Rational.RationalOps
    r.get shouldBe Literal(Number(1 :/ 2))
  }
  it should "fail to simplify 2 ^ 1/2" in {
    val r: em.MatchResult[Expression] = em.biFunctionMatcher(Power ~ Two ~ Literal(Rational.half))
    r.successful shouldBe true
  }

  behavior of "matchSimplifyBiFunction"

  import com.phasmidsoftware.matchers.Matchers.matchers.TildeOps

  it should "simplify E" in {
    val r: em.MatchResult[Expression] = em.evaluateMonadicDuple(Exp ~ One)
    r.successful shouldBe true
    r.get shouldBe ConstE
  }

  // FIXME Issue #57 this was working just yesterday
  ignore should "simplify multiple similar ops" in {
    em.simplifier(Expression(2) * 3 * Number.e * 5) shouldBe em.Match(ConstE * 30)
    // TODO we would like the following to be ConstE * 30
    //    em.simplifier(ConstE * 2 * 3 * 5) shouldBe em.Match(ConstE * 2 * 15)
    em.simplifier(Expression(5) * 2 * 3 * Number.e) shouldBe em.Match(ConstE * 30)
  }

  behavior of "two levels"
  it should "get 0 from -√3 + √3" in {
    val p = em.matchDyadicTwoLevels
    val root3: Number = Number(3).sqrt
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Product)
    val e: DyadicTriple = Sum ~ e1 ~ Literal(root3)
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  it should "get 0 from √3 + -√3" in {
    val p = em.matchDyadicTwoLevels
    val root3: Number = Number(3).sqrt
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Product)
    val e: DyadicTriple = Sum ~ Literal(root3) ~ e1
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  it should "get 1 from 1/√3 * √3" in {
    val p = em.matchDyadicTwoLevels
    val root3: Number = Number(3).sqrt
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Power)
    val e: DyadicTriple = Product ~ e1 ~ Literal(root3)
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe One
  }

  it should "simplify various" in {
    val p: em.Matcher[em.DyadicTriple, Expression] = em.matchDyadicTwoLevels
    import em.TildeOps
    p(Sum ~ BiFunction(Two, MinusOne, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Zero)
    p(Sum ~ BiFunction(MinusOne, Two, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(MinusOne, Two, Product)) shouldBe em.Match(Zero)
  }


  behavior of "matchAndCollectTwoDyadicLevels"
  it should "work for √3 * √3" in {
    val p = em.matchAndCollectTwoDyadicLevels
    val result: em.MatchResult[Expression] = p(Product ~ (Power ~ Literal(3) ~ Literal(Rational.half)) ~ (Power ~ Literal(3) ~ Literal(Rational.half)))
    result.successful shouldBe true
    result.get shouldBe Literal(3)
  }
  it should "work for (π + 1) * (π - 1)" in {
    val p = em.matchAndCollectTwoDyadicLevels
    val root3: Expression = Expression(3).sqrt
    val result: em.MatchResult[Expression] = p(Sum ~ (Product ~ root3 ~ One) ~ (Product ~ root3 ~ MinusOne))
    result.successful shouldBe true
    result.get shouldBe Zero
  }

  behavior of "matchMultiLevels"
  it should "get 2" in {
    val p = em.matchMultiLevels
    val e1: BiFunction = BiFunction(Two, One, Sum)
    val e2: BiFunction = BiFunction(Zero, MinusOne, Sum)
    val e: DyadicTriple = Sum ~ e1 ~ e2
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Literal(2)
  }
  it should "get 1" in {
    val p = em.matchMultiLevels
    val e1: BiFunction = BiFunction(Literal(99), Literal(3), Sum)
    val e2: BiFunction = BiFunction(Literal(-100), MinusOne, Sum)
    val e: DyadicTriple = Sum ~ e1 ~ e2
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Literal(1)
  }
  it should "get 3" in {
    val p = em.matchMultiLevels
    val e1: BiFunction = BiFunction(Literal(99), Literal(3), Sum)
    val e2: BiFunction = BiFunction(Literal(-100), BiFunction(ConstE, Zero, Power), Sum)
    val e: DyadicTriple = Sum ~ e1 ~ e2
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Literal(3)
  }
  // FIXME Issue #57
  ignore should "get x" in {
    val p = em.matchMultiLevels
    val root3: Number = Number(3).sqrt
    val e1: BiFunction = BiFunction(One, Literal(root3), Sum)
    val e2: BiFunction = BiFunction(Zero, BiFunction(Literal(root3), MinusOne, Product), Sum)
    val e: DyadicTriple = Sum ~ e1 ~ e2
    val result: em.MatchResult[Expression] = p(e)
    result.successful shouldBe true
    result.get shouldBe Literal(3)
  }

}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () })

