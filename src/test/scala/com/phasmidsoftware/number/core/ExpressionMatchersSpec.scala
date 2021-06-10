package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogLevel, LogOff, MatchLogger}
import com.phasmidsoftware.number.core.Number.one
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  val sb = new StringBuilder
  implicit val logger: MatchLogger = w => sb.append(s"$w\n")
  implicit val ll: LogLevel = LogOff

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    println("===============================\n")
  }

  implicit val p: ExpressionMatchers = new ExpressionMatchers {}

  behavior of "value"
  it should "work with value on Literal" in {
    val f: p.ExpressionMatcher[Number] = p.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = p.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = p.value
    f(one).successful shouldBe true
  }
  it should "work with value on FuzzyNumber" in {
    val f = p.value
    f(FuzzyNumber(Right(1), Scalar, None)).successful shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: p.ExpressionMatcher[Number] = p.matchValue(one)
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
    val matchers = new ExpressionMatchers
    matchers.simplifier(z) should matchPattern { case matchers.Match(ExactNumber(Right(7), Scalar)) => }
  }

  behavior of "matchBiFunctionConstantResult"
  it should "match 1" in {
    implicit val matchers: ExpressionMatchers = new ExpressionMatchers
    val one: Expression = Number.one
    val negativeOne: Number = Number(-1)
    val p = matchers.matchBiFunctionConstantResult(Product, negativeOne, Number.zero)
    val r: matchers.MatchResult[Expression] = p((one, BiFunction(one, negativeOne, Product)))
    r.successful shouldBe true
    r.get shouldBe Number.zero
  }

  behavior of "matchEitherDyadic"
  it should "match (1, biFunction)" in {
    implicit val matchers: ExpressionMatchers = new ExpressionMatchers
    val one: Expression = Number.one
    val negativeOne: Number = Number(-1)
    val p = matchers.matchEitherDyadic
    val r: matchers.MatchResult[(BiFunction, Expression)] = p((one, BiFunction(one, negativeOne, Product)))
    r.successful shouldBe true
  }
  it should "match (biFunction, 1)" in {
    implicit val matchers: ExpressionMatchers = new ExpressionMatchers
    val one: Expression = Number.one
    val negativeOne: Number = Number(-1)
    val p = matchers.matchEitherDyadic
    val r: matchers.MatchResult[(BiFunction, Expression)] = p((BiFunction(one, negativeOne, Product), one))
    r.successful shouldBe true
  }

  behavior of "simplifier"
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = x + y
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel -1 and - 1 b" in {
    sb.append("cancel -1 and - 1 b:\n")
    val x: Expression = Expression.one
    val y = Number(-1) * x
    val z = y + x
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result should matchPattern { case matchers.Match(ExactNumber(Right(0), Scalar)) => }
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe Number.one
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result.successful shouldBe true
    result.get shouldBe Number.one
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z = y.simplify
    z shouldBe Number(7)
  }
  it should "show that lazy evaluation only works when you use it" in {
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
  ignore should "cancel addition and subtraction" in {
    val x = Number.one + 3 - 3
    x.simplify shouldBe Expression(Number.one)
  }
  // ISSUE 25
  ignore should "cancel multiplication and division" in {
    val x = Number.e * 2 / 2
    x.simplify shouldBe Expression(Number.e)
  }

  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val p = new ExpressionMatchers().biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get shouldBe Number(7)
  }
  it should "work for products" in {
    val p = new ExpressionMatchers().biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven * 2
    val y = x * 3
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get shouldBe Number(42)
  }
  it should "work for sums" in {
    val p = new ExpressionMatchers().biFunctionSimplifier
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get shouldBe Number(12)
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (Number.e * 2).materialize.toString shouldBe "5.436563656918090(35)"
  }

}
