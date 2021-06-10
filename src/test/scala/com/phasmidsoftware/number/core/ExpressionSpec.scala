package com.phasmidsoftware.number.core

import com.phasmidsoftware.matchers.{LogLevel, MatchLogger}
import com.phasmidsoftware.number.core.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case _ => false
    }
  }

  val sb = new StringBuilder
  implicit val logger: MatchLogger = w => sb.append(s"$w\n")
  implicit val ll: LogLevel = com.phasmidsoftware.matchers.LogOff

  before {
    sb.clear()
  }

  after {
    if (sb.nonEmpty) println(sb.toString())
    println("===============================\n")
  }

  implicit val p: ExpressionMatchers = new ExpressionMatchers {}

  behavior of "Expression"

  it should "materialize" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(x1, x2, Sum)
    val result = e.materialize
    result shouldEqual Number(Math.PI + 1)
  }

  it should "render" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(x1, x2, Sum)
    e.render shouldBe "4.1415926535897930(36)"
  }

  behavior of "ExpressionOps"

  it should "evaluate +" in {
    val x = Number(1) + 2
    x shouldEqual Number(3)
  }
  it should "evaluate -" in {
    val x = Number(1) - 2
    x shouldEqual Number(-1)
  }
  it should "evaluate *" in {
    val x = Number(3) * 2
    x shouldEqual Number(6)
  }
  it should "evaluate /" in {
    val x = Number(6) / 2
    x shouldEqual Number(3)
  }
  it should "evaluate ^ 2" in {
    val x = Number(6) ^ 2
    x shouldEqual Number(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = Number(36).sqrt
    x shouldEqual Number(6)
  }
  ignore should "evaluate (√3 + 1)(√3 - 1) as 2" in {
    val root3: Expression = Number(3).sqrt
    val x = (root3 + 1) * (root3 - 1)
    x.materialize shouldBe Number(2)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = Number.pi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Number.one
  }

  behavior of "toString"
  it should "work for (sqrt 2)^2" in {
    val seven: Expression = Number(7)
    val result: Expression = seven.sqrt ^ 2
    result.toString shouldBe "((7 ^ (2 ^ -1)) ^ 2)"
  }

  behavior of "gathering operations"
  it should "gather 2 and * 1/2" in {
    val x: Expression = Number(7)
    val y = x.sqrt
    val z = y ^ 2
    z.simplify shouldBe Number(7)
  }

  behavior of "canceling operations"
  it should "cancel 1 and - -1" in {
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    z.simplify shouldBe Number.zero
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    z.simplify shouldBe Number.one
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    z.simplify shouldBe Number.one
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
  it should "show ^2 and sqrt for illustrative purposes" in {
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
