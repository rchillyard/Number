/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.core.inner.{Rational, SquareRoot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InversePowerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InversePower"

  val two = InversePower(1, 2)
  val three = InversePower(1, 3)

  ignore should "compareTo" in {
    InversePower(2, 4).compareTo(two) shouldBe 0
    InversePower(2, 9).compareTo(three) shouldBe 0
    InversePower(2, 4).compareTo(three) should be < 0
    InversePower(2, 9).compareTo(two) should be > 0
    InversePower(3, 8).compareTo(two) shouldBe 0
  }

  it should "convert" in {
    InversePower(2, 4).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(2, 1)
    InversePower(2, 9).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(3, 1)
    InversePower(3, 8).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(2, 1)
    InversePower(2, 2).convert[Real](Real.zero).map(_.toDouble).get shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "toDouble" in {
    InversePower(2, 4).toDouble shouldBe 2.0
    InversePower(2, 9).toDouble shouldBe 3.0
    InversePower(3, 8).toDouble shouldBe 2.0
    InversePower(2, 2).toDouble shouldBe math.sqrt(2.0) +- 1e-10
    InversePower(3, 27).toDouble shouldBe 3.0
  }

  ignore should "signum" in {
    InversePower(2, 4).signum shouldBe 1
    InversePower(2, 0).signum shouldBe 0
    InversePower(3, -8).signum shouldBe -1
  }

  it should "approximation" in {
    InversePower(2, 4).approximation should matchPattern { case Some(Real(2, None)) => }
    InversePower(2, 2).approximation.get.toDouble shouldBe math.sqrt(2.0)
    InversePower(3, 27).approximation.get.toDouble shouldBe 3.0
  }

  ignore should "asJavaNumber" in {
    InversePower(2, 4).asJavaNumber.get shouldBe java.lang.Double.valueOf(2.0)
    InversePower(2, 2).asJavaNumber.get.doubleValue() shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "maybeDouble" in {
    InversePower(2, 4).maybeDouble shouldBe Some(2)
    InversePower(2, 2).maybeDouble shouldBe Some(1.4142135623730951)
    InversePower(3, 8).maybeDouble shouldBe Some(2)
  }

//  it should "$times" in {
//    InversePower(2, 4) * WholeNumber(3) shouldBe WholeNumber(6)
//    InversePower(2, 2) * InversePower(2, 2) shouldBe WholeNumber(2)
//    InversePower(2, 9) * WholeNumber(2) shouldBe WholeNumber(6)
//  }

//  it should "$div" in {
//    InversePower(2, 4) / WholeNumber(2) shouldBe WholeNumber(1)
//    InversePower(2, 9) / WholeNumber(3) shouldBe WholeNumber(1)
//    InversePower(3, 8) / WholeNumber(2) shouldBe WholeNumber(1)
//  }

//  it should "compare" in {
//    InversePower(2, 4).compare(WholeNumber(2)) shouldBe 0
//    InversePower(2, 4).compare(WholeNumber(3)) should be < 0
//    InversePower(2, 9).compare(WholeNumber(2)) should be > 0
//  }

  ignore should "doScaleDouble" in {
    InversePower(2, 4).doScaleDouble(2.0) shouldBe Real(4.0, 0.0)
    InversePower(2, 9).doScaleDouble(0.5) shouldBe Real(1.5, 0.0)
  }

  it should "isZero" in {
    InversePower(2, 4).isZero shouldBe false
    InversePower(2, 0).isZero shouldBe true
    InversePower(3, 0).isZero shouldBe true
  }

  it should "isExact" in {
    InversePower(2, 4).isExact shouldBe true
    InversePower(2, 2).isExact shouldBe true
    InversePower(3, 8).isExact shouldBe true
  }

  it should "maybeFactor" in {
    InversePower(2, 4).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
    InversePower(2, 2).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
  }

//  it should "doScale" in {
//    InversePower(2, 4).doScale(WholeNumber(3)) shouldBe WholeNumber(6)
//    InversePower(2, 9).doScale(RationalNumber(1, 3)) shouldBe WholeNumber(1)
//  }

  it should "toRational" in {
    InversePower(2, 4).toRational.get shouldBe Rational(2)
    InversePower(2, 9).toRational.get shouldBe Rational(3)
    InversePower(3, 8).toRational.get shouldBe Rational(2)
  }

  it should "showInversePower" in {
    InversePower.showInversePower.show(InversePower(2, 4)) should include("√4")
    InversePower.showInversePower.show(InversePower(3, 8)) should include("³√8")
  }

  it should "render" in {
    Eager.root2.render shouldBe "√2"
    Eager.root3.render shouldBe "√3"
    InversePower(3, 2).render shouldBe "³√2"
  }

  it should "render implicitly" in {
    val w1: String = Eager.root2
    w1 shouldBe "√2"
    val w2: String = Eager.root3
    w2 shouldBe "√3"
    val w3: String = InversePower(3, 2)
    w3 shouldBe "³√2"
  }

  behavior of "normalize"

  it should "normalize" in {
    // Perfect square roots
    InversePower(2, 4).normalize shouldBe WholeNumber(2)
    InversePower(2, 9).normalize shouldBe WholeNumber(3)
    InversePower(2, 16).normalize shouldBe WholeNumber(4)
    InversePower(2, 1).normalize shouldBe WholeNumber(1)

    // Perfect cube roots
    InversePower(3, 8).normalize shouldBe WholeNumber(2)
    InversePower(3, 27).normalize shouldBe WholeNumber(3)
    InversePower(3, 1).normalize shouldBe WholeNumber(1)

    // Non-perfect roots (should remain as InversePower)
    InversePower(2, 5).normalize shouldBe InversePower(2, WholeNumber(5))
    InversePower(3, 10).normalize shouldBe InversePower(3, WholeNumber(10))

    // n = 1 (should just return the number normalized)
    InversePower(1, 5).normalize shouldBe WholeNumber(5)
    InversePower(1, RationalNumber(6, 2)).normalize shouldBe WholeNumber(3)

    // RationalNumber that normalizes to WholeNumber, then perfect root
    InversePower(2, RationalNumber(16, 1)).normalize shouldBe WholeNumber(4)

    // Nested normalization
    InversePower(2, RationalNumber(9, 1)).normalize shouldBe WholeNumber(3)

    // Higher degree roots (should remain as InversePower for now)
    InversePower(4, 16).normalize shouldBe InversePower(4, WholeNumber(16))
  }
}
