package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.expression.{ConstPi, Expression}
import com.phasmidsoftware.number.core.inner.PureNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"
  it should "isExact" in {
    val x = (ConstPi / 2).materialize
    x.isExact shouldBe true
  }
  it should "multiply i by itself correctly" in {
    val z = Expression(Constants.i) * Constants.i
    val result = z.materialize
    result shouldBe Constants.minusOne
  }
  it should "add" in {
    val one = Number.one
    val result = one `add` Constants.i
    result shouldBe ComplexCartesian(1, 1)
  }
  it should "take the natural log of i" in {
    val x = Constants.i.ln
    x shouldBe ComplexCartesian(Number.zero, Constants.piBy2.x)
    x.render shouldBe "i½\uD835\uDED1"
  }
  it should "take the natural log of 2" in {
    val x = Constants.two.ln
    x should matchPattern { case Real(FuzzyNumber(Left(Left(Some(0.6931471805599453))), PureNumber, _)) => }
    x.render shouldBe "0.6931471805599453±.00000000000020%"
  }
}
