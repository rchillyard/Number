package com.phasmidsoftware.number.core.inner

/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

import com.phasmidsoftware.number.core.inner.RationalFloor.{ceiling, floor}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RationalFloorSpec extends AnyFlatSpec with Matchers {

  behavior of "floor"

  it should "return the same value for whole numbers" in {
    val r = Rational(6, 2) // 3
    r.floor shouldBe Rational(3)
  }

  it should "floor positive fractions down" in {
    val r = Rational(7, 2) // 3.5
    r.floor shouldBe Rational(3)
  }

  it should "floor negative fractions down" in {
    val r = Rational(-7, 2) // -3.5
    r.floor shouldBe Rational(-4)
  }

  it should "handle positive fraction less than 1" in {
    val r = Rational(1, 4) // 0.25
    r.floor shouldBe Rational(0)
  }

  it should "handle negative fraction greater than -1" in {
    val r = Rational(-1, 4) // -0.25
    r.floor shouldBe Rational(-1)
  }

  it should "handle large positive fractions" in {
    val r = Rational(1001, 100) // 10.01
    r.floor shouldBe Rational(10)
  }

  it should "handle large negative fractions" in {
    val r = Rational(-1001, 100) // -10.01
    r.floor shouldBe Rational(-11)
  }

  it should "handle zero" in {
    Rational.zero.floor shouldBe Rational.zero
  }

  it should "handle one" in {
    Rational.one.floor shouldBe Rational.one
  }

  it should "handle minus one" in {
    Rational(-1).floor shouldBe Rational(-1)
  }

  it should "handle exactly on boundary - positive" in {
    val r = Rational(10, 2) // 5.0
    r.floor shouldBe Rational(5)
  }

  it should "handle exactly on boundary - negative" in {
    val r = Rational(-10, 2) // -5.0
    r.floor shouldBe Rational(-5)
  }

  behavior of "ceiling"

  it should "return the same value for whole numbers" in {
    val r = Rational(6, 2) // 3
    r.ceiling shouldBe Rational(3)
  }

  it should "ceiling positive fractions up" in {
    val r = Rational(7, 2) // 3.5
    r.ceiling shouldBe Rational(4)
  }

  it should "ceiling negative fractions up" in {
    val r = Rational(-7, 2) // -3.5
    r.ceiling shouldBe Rational(-3)
  }

  it should "handle positive fraction less than 1" in {
    val r = Rational(1, 4) // 0.25
    r.ceiling shouldBe Rational(1)
  }

  it should "handle negative fraction greater than -1" in {
    val r = Rational(-1, 4) // -0.25
    r.ceiling shouldBe Rational(0)
  }

  it should "handle large positive fractions" in {
    val r = Rational(1001, 100) // 10.01
    r.ceiling shouldBe Rational(11)
  }

  it should "handle large negative fractions" in {
    val r = Rational(-1001, 100) // -10.01
    r.ceiling shouldBe Rational(-10)
  }

  it should "handle zero" in {
    Rational.zero.ceiling shouldBe Rational.zero
  }

  it should "handle one" in {
    Rational.one.ceiling shouldBe Rational.one
  }

  it should "handle minus one" in {
    Rational(-1).ceiling shouldBe Rational(-1)
  }

  it should "handle exactly on boundary - positive" in {
    val r = Rational(10, 2) // 5.0
    r.ceiling shouldBe Rational(5)
  }

  it should "handle exactly on boundary - negative" in {
    val r = Rational(-10, 2) // -5.0
    r.ceiling shouldBe Rational(-5)
  }

  behavior of "floor and ceiling relationship"

  it should "satisfy floor <= x <= ceiling" in {
    val testCases = Seq(
      Rational(7, 2),
      Rational(-7, 2),
      Rational(1, 3),
      Rational(-1, 3),
      Rational(100, 7),
      Rational(-100, 7)
    )

    testCases.foreach { r =>
      r.floor.toDouble should be <= r.toDouble
      r.ceiling.toDouble should be >= r.toDouble
    }
  }

  it should "have ceiling = floor + 1 for non-whole numbers" in {
    val testCases = Seq(
      Rational(7, 2),
      Rational(-7, 2),
      Rational(1, 3),
      Rational(-1, 3)
    )

    testCases.foreach { r =>
      if (!r.isWhole) {
        r.ceiling shouldBe r.floor + Rational(1)
      }
    }
  }

  it should "have floor = ceiling for whole numbers" in {
    val testCases = Seq(
      Rational(0),
      Rational(1),
      Rational(-1),
      Rational(10),
      Rational(-10)
    )

    testCases.foreach { r =>
      r.floor shouldBe r.ceiling
    }
  }
}
