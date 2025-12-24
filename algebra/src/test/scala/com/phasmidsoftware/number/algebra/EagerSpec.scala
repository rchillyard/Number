/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EagerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Eager"

  it should "~= Real/Real" in {
    val x: Eager = Real(1.0)
    val y: Eager = Real(1.0)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= Angle/Angle" in {
    val x: Eager = Angle(Real(1.0))
    val y: Eager = Angle(Real(1.0))
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= WholeNumber/WholeNumber" in {
    val x: Eager = WholeNumber(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/RationalNumber" in {
    val x: Eager = RationalNumber(1)
    val y: Eager = RationalNumber(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/WholeNumber" in {
    val x: Eager = RationalNumber(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/Functional" in {
    val x: Functional = NatLog(1)
    val y: Functional = NatLog(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= Real/WholeNumber" in {
    val x: Eager = Real(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/RationalNumber" in {
    val x: Eager = Real(1)
    val y: Eager = RationalNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/Real" in {
    val y: Eager = Real(1)
    val x: Eager = RationalNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/Functional (1)" in {
    val x: Eager = Real(scala.math.Pi)
    val y: Eager = Angle(Real(1.0))
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/Functional (2)" in {
    val x: Eager = Real(math.E)
    val y: Eager = NatLog(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/Real" in {
    val y: Eager = Real(scala.math.Pi)
    val x: Eager = Angle(Real(1.0))
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/WholeNumber" in {
    val x: Eager = Angle(Real(1.0 / math.Pi))
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= WholeNumber/Functional" in {
    val y: Eager = Angle(Real(1.0 / math.Pi))
    val x: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
}
