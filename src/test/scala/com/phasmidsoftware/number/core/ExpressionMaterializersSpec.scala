package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionMaterializersSpec extends AnyFlatSpec with should.Matchers {

  private val p = new ExpressionMaterializers {}

  behavior of "ExpressionMaterializer method"

  it should "work with fixed success result" in {
    val f: p.ExpressionMaterializer[Number] = p.ExpressionMaterializer(_ => p.Evaluation(Number.one))
    val e = Literal(Number.one)
    f(e).success shouldBe true
  }
  it should "work with fixed fail result" in {
    val f: p.ExpressionMaterializer[Number] = p.ExpressionMaterializer(e => p.NonMaterialized(e))
    val e = Literal(Number.one)
    f(e).success shouldBe false
  }

  behavior of "value"
  it should "work with value on Literal" in {
    val f: p.ExpressionMaterializer[Number] = p.value
    val e = Literal(Number.one)
    f(e).success shouldBe true
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: p.ExpressionMaterializer[Number] = p.matchValue(Number.one)
    val e = Literal(Number.one)
    f(e).success shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f: p.ExpressionMaterializer[Number] = p.matchValue(Number.one)
    val g = f | p.matchValue(Number.pi)
    f(Literal(Number.one)).success shouldBe true
    g(Literal(Number.pi)).success shouldBe true
    g(Literal(Number.e)).success shouldBe false
  }
}
