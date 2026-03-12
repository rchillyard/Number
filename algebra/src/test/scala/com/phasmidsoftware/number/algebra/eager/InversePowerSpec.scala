/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.AnyContext
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.eager.InversePower.{cubeRoot, squareRoot}
import com.phasmidsoftware.number.core.inner.{Rational, SquareRoot}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class InversePowerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "InversePower"

  val two: InversePower = InversePower(1, 2)
  val three: InversePower = InversePower(1, 3)

  it should "compareTo" in {
    squareRoot(4).compareTo(two) shouldBe 0
    squareRoot(9).compareTo(three) shouldBe 0
    squareRoot(4).compareTo(three) should be < 0
    squareRoot(9).compareTo(two) should be > 0
    cubeRoot(8).compareTo(two) shouldBe 0
  }

  it should "convert" in {
    squareRoot(4).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(2, 1)
    squareRoot(9).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(3, 1)
    cubeRoot(8).convert[RationalNumber](RationalNumber.zero).get shouldBe RationalNumber(2, 1)
    squareRoot(2).convert[Real](Real.zero).map(_.toDouble).get shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "toDouble" in {
    squareRoot(4).toDouble shouldBe 2.0
    squareRoot(9).toDouble shouldBe 3.0
    cubeRoot(8).toDouble shouldBe 2.0
    squareRoot(2).toDouble shouldBe math.sqrt(2.0) +- 1e-10
    cubeRoot(27).toDouble shouldBe 3.0
  }

  it should "signum" in {
    squareRoot(4).signum shouldBe 1
    cubeRoot(-8).signum shouldBe -1
  }

  it should "approximation" in {
    squareRoot(4).approximation should matchPattern { case Some(Real(2, None)) => }
    squareRoot(2).approximation.get.toDouble shouldBe math.sqrt(2.0)
    cubeRoot(27).approximation.get.toDouble shouldBe 3.0
  }

  it should "asJavaNumber" in {
    squareRoot(4).asJavaNumber.get shouldBe java.lang.Double.valueOf(2.0)
    squareRoot(2).asJavaNumber.get.doubleValue() shouldBe math.sqrt(2.0) +- 1e-10
  }

  it should "maybeDouble" in {
    squareRoot(4).maybeDouble shouldBe Some(2)
    squareRoot(2).maybeDouble shouldBe Some(1.4142135623730951)
    cubeRoot(8).maybeDouble shouldBe Some(2)
  }

  it should "scale" in {
    squareRoot(4) * Rational.two shouldBe squareRoot(16)
    cubeRoot(8) * Rational.two shouldBe cubeRoot(64)
  }

//  it should "$times" in {
  //    squareRoot(4) * WholeNumber(3) shouldBe WholeNumber(6)
  //    squareRoot(2) * squareRoot(2) shouldBe WholeNumber(2)
  //    squareRoot(9) * WholeNumber(2) shouldBe WholeNumber(6)
//  }

//  it should "$div" in {
  //    squareRoot(4) / WholeNumber(2) shouldBe WholeNumber(1)
  //    squareRoot(9) / WholeNumber(3) shouldBe WholeNumber(1)
//    InversePower(3, 8) / WholeNumber(2) shouldBe WholeNumber(1)
//  }

//  it should "compare" in {
  //    squareRoot(4).compare(WholeNumber(2)) shouldBe 0
  //    squareRoot(4).compare(WholeNumber(3)) should be < 0
  //    squareRoot(9).compare(WholeNumber(2)) should be > 0
//  }

  it should "isZero" in {
    squareRoot(4).isZero shouldBe false
  }

  it should "isExact" in {
    squareRoot(4).isExact shouldBe true
    squareRoot(2).isExact shouldBe true
    cubeRoot(8).isExact shouldBe true
  }

  it should "maybeFactor" in {
    squareRoot(4).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
    squareRoot(2).maybeFactor(AnyContext) shouldBe Some(SquareRoot)
  }

//  it should "doScale" in {
  //    squareRoot(4).doScale(WholeNumber(3)) shouldBe WholeNumber(6)
  //    squareRoot(9).doScale(RationalNumber(1, 3)) shouldBe WholeNumber(1)
//  }

  it should "toRational" in {
    squareRoot(4).toRational.get shouldBe Rational(2)
    squareRoot(9).toRational.get shouldBe Rational(3)
    cubeRoot(8).toRational.get shouldBe Rational(2)
  }

  it should "showInversePower" in {
    InversePower.showInversePower.show(squareRoot(4)) should include("√4")
    InversePower.showInversePower.show(cubeRoot(8)) should include("³√8")
  }

  it should "render" in {
    Eager.root2.render shouldBe "√2"
    Eager.root3.render shouldBe "√3"
    cubeRoot(2).render shouldBe "³√2"
  }

  it should "render implicitly" in {
    val w1: String = Eager.root2
    w1 shouldBe "√2"
    val w2: String = Eager.root3
    w2 shouldBe "√3"
    val w3: String = cubeRoot(2)
    w3 shouldBe "³√2"
  }

  behavior of "normalize"

  it should "normalize" in {
    // Perfect square roots
    squareRoot(4).normalize shouldBe WholeNumber(2)
    squareRoot(9).normalize shouldBe WholeNumber(3)
    squareRoot(16).normalize shouldBe WholeNumber(4)
    squareRoot(1).normalize shouldBe WholeNumber(1)

    // Perfect cube roots
    cubeRoot(8).normalize shouldBe WholeNumber(2)
    cubeRoot(27).normalize shouldBe WholeNumber(3)
    cubeRoot(1).normalize shouldBe WholeNumber(1)

    // Non-perfect roots (should remain as InversePower)
    squareRoot(5).normalize shouldBe squareRoot(WholeNumber(5))
    cubeRoot(10).normalize shouldBe InversePower(3, WholeNumber(10))

    // n = 1 (should just return the number normalized)
    InversePower(1, 5).normalize shouldBe WholeNumber(5)
    InversePower(1, RationalNumber(6, 2)).normalize shouldBe WholeNumber(3)

    // RationalNumber that normalizes to WholeNumber, then perfect root
    squareRoot(RationalNumber(16, 1)).normalize shouldBe WholeNumber(4)

    // Nested normalization
    squareRoot(RationalNumber(9, 1)).normalize shouldBe WholeNumber(3)

    // Higher degree roots
    InversePower(4, 16).normalize shouldBe WholeNumber(2)
  }
}
