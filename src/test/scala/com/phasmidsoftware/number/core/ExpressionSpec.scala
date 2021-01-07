package com.phasmidsoftware.number.core

import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionSpec extends AnyFlatSpec with should.Matchers {

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
    val e = Sum(x1, x2)
    val result = e.materialize
    result shouldEqual Number(Math.PI + 1)
  }

  it should "render" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = Sum(x1, x2)
    e.render shouldBe "4.1415926535897930(61)"

  }

}
