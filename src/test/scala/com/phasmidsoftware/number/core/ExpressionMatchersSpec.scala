package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Number.one
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  val sb = new StringBuilder
  implicit val logger: MatchLogger = w => sb.append(s"$w\n")

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    println("===============================\n")
  }

  private val p = new ExpressionMatchers {}

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

  behavior of "matchDyadicBranch"
  it should "match 1" in {
    val matchers = new ExpressionMatchers
    val one: Expression = Number.one
    val negativeOne: Number = Number(-1)
    val p = matchers.matchDyadicBranch(Product, negativeOne, Number.zero)
    val r: matchers.MatchResult[Expression] = p((one, BiFunction(one, negativeOne, Product)))
    r.successful shouldBe true
    r.get shouldBe Number.zero
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
  ignore should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val matchers = new ExpressionMatchers
    val result = matchers.simplifier(z)
    result shouldBe One
    //      z.simplify shouldBe One
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
    z shouldBe Expression(7)
    y.simplify.materialize should matchPattern { case ExactNumber(_, _) => }
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
    val y = x power 2
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

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (Number.e * 2).materialize.toString shouldBe "5.436563656918090(35)"
  }

}
