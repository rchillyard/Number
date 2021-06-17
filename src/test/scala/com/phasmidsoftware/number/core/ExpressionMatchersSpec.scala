package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogDebug, LogLevel, MatchLogger, ~}
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  val sb = new StringBuilder
  implicit val logger: MatchLogger = w => sb.append(s"$w\n")
  implicit val ll: LogLevel = LogDebug

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    println("===============================\n")
  }

  implicit val em: ExpressionMatchers = new ExpressionMatchers {}
  private val two: Number = Number(2)
  private val one: Number = Number.one
  private val zero: Number = Number.zero

  behavior of "value"
  it should "work with value on Literal" in {
    val f: em.ExpressionMatcher[Number] = em.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = em.value
    f(one).successful shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = em.value
    f(FuzzyNumber(Right(1), Scalar, None)).successful shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: em.ExpressionMatcher[Number] = em.matchValue(one)
    val e = Literal(one)
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val q = new ExpressionMatchers {}
    val f = q.matchValue(one)
    val g = f | q.matchValue(Number.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Number.pi)).successful shouldBe true
    g(Literal(Number.e)).successful shouldBe false
  }

  import com.phasmidsoftware.number.core.Expression.ExpressionOps

  behavior of "gathering operations"
  ignore should "gather 2 and * 1/2" in {
    val x: Expression = Number(7)
    val y = x.sqrt
    val z = y ^ 2
    val matchers = implicitly[ExpressionMatchers]
    matchers.simplifier(z) should matchPattern { case matchers.Match(ExactNumber(Right(7), Scalar)) => }
  }

  behavior of "matchBiFunctionConstantResult"
  it should "match 1" in {
    val matchers = implicitly[ExpressionMatchers]
    val negativeOne: Number = Number(-1)
    val q = matchers.matchBiFunctionConstantResult(Product, negativeOne, zero)
    import matchers.TildeOps
    val r: matchers.MatchResult[Expression] = q(one ~ BiFunction(one, negativeOne, Product))
    r.successful shouldBe true
    r.get shouldBe zero
  }

  behavior of "matchEitherDyadic"
  it should "match (1, biFunction)" in {
    //    implicit val matchers: ExpressionMatchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val negativeOne: Number = Number(-1)
    val p = matchers.matchEitherDyadic(true)
    import matchers.TildeOps
    val r: matchers.MatchResult[BiFunction ~ Expression] = p(one ~ BiFunction(one, negativeOne, Product))
    r.successful shouldBe true
  }
  it should "match (biFunction, 1)" in {
    //    implicit val matchers: ExpressionMatchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val negativeOne: Number = Number(-1)
    val p = matchers.matchEitherDyadic(true)
    import matchers.TildeOps
    val r: matchers.MatchResult[BiFunction ~ Expression] = p(BiFunction(one, negativeOne, Product) ~ one)
    r.successful shouldBe true
  }

  behavior of "simplifier"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = x + y
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1 b" in {
    sb.append("cancel -1 and - 1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = y + x
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe one
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    //    val matchers = new ExpressionMatchers
    val matchers = implicitly[ExpressionMatchers]
    val result = matchers.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe one
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    println(y)
    val z = y.simplify
    val q = z.simplify
    q shouldBe Number(7)
  }
  ignore should "show that lazy evaluation only works when you use it" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = x ^ 2
    y.materialize should matchPattern { case FuzzyNumber(_, _, _) => }
  }
  ignore should "show ^2 and sqrt for illustrative purposes" in {
    val seven = Number(7)
    val x = seven.sqrt
    val y = x ^ 2
    y should matchPattern { case FuzzyNumber(_, _, _) => }
    y shouldEqual Number(7)
  }
  // ISSUE 25
  it should "cancel addition and subtraction" in {
    val x = one + 3 - 3
    val q = x.simplify
    q shouldBe one
  }
  // ISSUE 25
  it should "cancel multiplication and division" in {
    val x = Number.pi * 2 / 2
    val simplify = x.simplify
    simplify shouldBe Number.pi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Number.pi / 2 * 2
    val simplify = x.simplify
    simplify shouldBe Number.pi
  }

  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val q = em.biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z: ExpressionMatchers#MatchResult[Expression] = q(y)
    z.successful shouldBe true
    z.get shouldBe seven ^ one
  }
  it should "work for products" in {
    val q = new ExpressionMatchers().biFunctionSimplifier
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
    z.get shouldBe BiFunction(Number(9), Literal(3), Sum)
  }

  behavior of "various operations"
  ignore should "evaluate E * 2" in {
    (Number.e * 2).materialize.toString shouldBe "5.436563656918090(35)"
  }

  behavior of "matchSimplifyTimesIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyTimesIdentity
    import em.TildeOps
    p(Product ~ two ~ one) shouldBe em.Match(two)
    p(Product ~ one ~ two) shouldBe em.Match(two)
  }

  behavior of "matchSimplifyPlusIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyPlusIdentity
    import em.TildeOps
    p(Sum ~ two ~ zero) shouldBe em.Match(two)
    p(Sum ~ zero ~ two) shouldBe em.Match(two)
  }

  behavior of "matchSimplifyPowerIdentity"
  it should "eliminate 1" in {
    val p = em.matchSimplifyPowerIdentity
    import em.TildeOps
    p(Power ~ two ~ one) shouldBe em.Match(two)
    p(Power ~ one ~ two) shouldBe em.Match(one)
  }

  behavior of "biFunctionSimplifier"
  it should "simplify" in {
    val p = em.biFunctionSimplifier
    p(BiFunction(two, zero, Sum)) shouldBe em.Match(two)
    p(BiFunction(zero, two, Sum)) shouldBe em.Match(two)
    p(BiFunction(two, one, Product)) shouldBe em.Match(two)
    p(BiFunction(one, two, Product)) shouldBe em.Match(two)
    p(BiFunction(two, one, Power)) shouldBe em.Match(two)
    p(BiFunction(one, two, Power)) shouldBe em.Match(one)
  }

  behavior of "matchBiFunctionConstantResult"
  it should "simplify" in {
    val p: em.Matcher[em.Expressions, Expression] = em.matchBiFunctionConstantResult(Product, Number(-1), zero)
    import em.TildeOps
    p(BiFunction(two, Number(-1), Product) ~ two) shouldBe em.Match(zero)
    p(two ~ BiFunction(two, Number(-1), Product)) shouldBe em.Match(zero)
    p(BiFunction(Number(-1), two, Product) ~ two) shouldBe em.Match(zero)
    p(two ~ BiFunction(Number(-1), two, Product)) shouldBe em.Match(zero)
  }
}
