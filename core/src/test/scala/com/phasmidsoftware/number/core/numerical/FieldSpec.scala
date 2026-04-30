package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.Real.RealIsFractional.mkNumericOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FieldSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Field"
  it should "isExact" in {
    val x = (Constants.pi / Real(2))
    x.isExact shouldBe true
  }
  it should "multiply i by itself correctly" in {
    val z = Constants.i * Constants.i
    z shouldBe ComplexCartesian(-1, 0)
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
    x.render shouldBe "0.69314718055994530[89]"
  }
  it should "add infinity to infinity" in {
    val result = Constants.infinity `add` Constants.infinity
    result shouldBe Constants.infinity
  }
  it should "add infinity to negative infinity" in {
    val result = Constants.infinity `add` Real(Rational(-1).invert)
    result shouldBe Constants.infinity
  }
  it should "add infinity to a finite number" in {
    val result = Constants.infinity `add` Real(1)
    result shouldBe Constants.infinity
  }
  it should "multiply infinity by zero" in {
    val result = Constants.infinity `multiply` Constants.infinity
    result shouldBe Constants.infinity
  }
  it should "multiply infinity by a finite number" in {
    val result = Constants.infinity `multiply` Real(1)
    result shouldBe Constants.infinity
  }
  it should "divide infinity by infinity" in {
    val result = Constants.infinity `divide` Constants.infinity
    result shouldBe Constants.NaN
  }

}
