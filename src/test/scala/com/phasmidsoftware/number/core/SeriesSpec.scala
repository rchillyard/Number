/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{FiniteSeries, InfiniteSeries, Rational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class SeriesSpec extends AnyFlatSpec with Matchers with FuzzyEquality {

  behavior of "FiniteSeries"

  val s = FiniteSeries(List(1, 2, 3, 4, 5, 6, 7))

  it should "term" in {
    s.term(0) shouldBe Some(1)
    s.term(1) shouldBe Some(2)
    s.term(2) shouldBe Some(3)
    s.term(6) shouldBe Some(7)
    s.term(7) shouldBe None
  }

  it should "evaluate n" in {
    s.evaluate(None) shouldBe Some(28)
  }

  it should "evaluate epsilon" in {

  }

  behavior of "InfiniteSeries"

  val t: InfiniteSeries[Int] = InfiniteSeries(LazyList.from(1), 0.01)

  it should "term" in {
    t.term(0) shouldBe Some(1)
    t.term(1) shouldBe Some(2)
    t.term(2) shouldBe Some(3)
    t.term(6) shouldBe Some(7)
    t.term(7) shouldBe Some(8)
  }

  it should "evaluate n" in {
    t.evaluate(Some(1)) shouldBe Some(1)
    t.evaluate(Some(4)) shouldBe Some(10)
    t.evaluate(Some(7)) shouldBe Some(28)
    val n = Random.nextInt(1000)
    t.evaluate(Some(n)) shouldBe Some(n * (n + 1) / 2)
  }
  it should "evaluate infinity" in {
    t.evaluate(None) shouldBe None
  }

  it should "evaluate epsilon" in {
  }

  behavior of "Basel Problem"

  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)

  it should "term" in {
    basel.term(0) shouldBe Some(Number.one)
    basel.term(1) shouldBe Number(4).invert.asNumber
  }

  it should "evaluate n" in {
    basel.evaluate(Some(1)) shouldBe Some(Number.one)
    basel.evaluate(Some(2)) shouldBe Number(Rational(5, 4)).asNumber
  }
  it should "evaluate N" in {
    println(basel.term(1000))
    val b1000 = basel.evaluate(Some(1000)).get
    b1000.toNominalDouble.get shouldBe 1.64393456668156 +- 1E-6
    b1000.fuzz.isDefined shouldBe false
  }
  // NOTE this test needs some attention.
  it should "evaluate epsilon" in {
    val actual = basel.evaluate(1E-6)
    actual.isSuccess shouldBe true
    println(s"actual = ${actual.get}")
    val expected = Number.pi.square.doDivide(Number(6))
    println(s"expected = $expected")
    expected.fuzz.isDefined shouldBe true
    println(s"expected.fuzz = ${expected.fuzz}")
    actual.get.fuzz.isDefined shouldBe true
    val z: Fuzziness[Double] = actual.get.fuzz.get
    val wiggle = z.wiggle(0.0001)
    println(s"wiggle = $wiggle")
    val difference = actual.get doSubtract expected
    println(s"difference = $difference")
    val fuzz: Fuzziness[Double] = difference.fuzz.get
    println(s"fuzz = $fuzz")
    fuzz.probability(1.0E-6, 0.6744897501960815E-6) shouldBe 0.5 +- 1E-8
    fuzz.wiggle(0.5) shouldBe (6.744897501960816E-4 +- 1E-8)
    fuzz.probability(wiggle, 0.001) shouldBe 0.203 +- 1E-2
    difference.isProbablyZero(0.001) shouldBe true
  }


}
