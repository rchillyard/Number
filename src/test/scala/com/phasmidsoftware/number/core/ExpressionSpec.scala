package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionSpec extends AnyFlatSpec with should.Matchers {

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
    e.render shouldBe "4.1415926535897930(61)"
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

  behavior of "gathering operations"
  it should "gather 2 and * 1/2" in {
    val x: Expression = Number(7)
    println(x)
    val y = x.sqrt
    println(y)
    val z = y ^ 2
    println(z)
    z shouldBe Number(7)
  }


  behavior of "canceling operations"
  it should "cancel 1 and - -1" in {
    val x: Expression = Number.one
    val y = -x
    val z = x + y
    z shouldBe Zero
  }
  it should "cancel 2 and * 1/2" in {
    val x: Expression = Number.one * 2
    val y = x.reciprocal
    val z = x * y
    z shouldBe One
  }
  it should "cancel 2 * 1/2" in {
    val x: Expression = Number.one * 2
    val y = x.reciprocal
    val z = y * x
    z shouldBe One
  }
  it should "cancel ^2 and sqrt" in {
    val seven: Expression = Number(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    y shouldEqual Number(7)
    y.materialize should matchPattern { case ExactNumber(_, _) => }
  }

}
