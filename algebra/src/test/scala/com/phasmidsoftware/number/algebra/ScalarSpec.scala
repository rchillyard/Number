package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.core.{AbsoluteFuzz, Box}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ScalarSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Scalar"

  it should "parse string" in {
    val x1: Valuable = "5"
    x1 shouldBe WholeNumber(5)
    val x2: Valuable = "5.01"
    x2 shouldBe RationalNumber(501, 100)
    val x3: Valuable = "5.012"
    x3 shouldBe Real(5.012, Some(AbsoluteFuzz(5.0E-4, Box)))
    val x4: Valuable = "0.5𝛑"
    x4 shouldBe Angle(RationalNumber(1, 2))
  }

  it should "correctly determine if a value is exact" in {
    val exact = WholeNumber(5)
    val inexact = Real(5.5)
    exact.isExact shouldBe true
    inexact.isExact shouldBe false
  }

  it should "convert to Real when possible" in {
    val number = WholeNumber(10)
    number.convert(Real.zero) shouldBe Some(Real(10.0, None))
  }

  it should "render values correctly" in {
    val number = WholeNumber(42)
    number.render shouldBe "42"
  }

  it should "perform addition using doPlus" in {
    val n1 = WholeNumber(3)
    val n2 = WholeNumber(4)
    n1.+(n2) shouldBe (WholeNumber(7))
  }

  it should "compare for equality correctly" in {
    val n1 = WholeNumber(5)
    val n2 = WholeNumber(5)
    val n3 = WholeNumber(6)
    n1.equals(n2) shouldBe true
    n1.equals(n3) shouldBe false
  }
}
