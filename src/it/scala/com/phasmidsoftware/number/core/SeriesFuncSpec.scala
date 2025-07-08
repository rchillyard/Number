/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.inner.{FiniteSeries, InfiniteSeries, Rational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

class SeriesFuncSpec extends AnyFlatSpec with Matchers with FuzzyEquality{
  behavior of "Basel Problem"

  val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square))
  it should "evaluate epsilon" in {
    val actual = basel.evaluate(1E-7)
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
    fuzz.wiggle(0.5) shouldBe (0.6744897501960815E-6 +- 1E-8)
    fuzz.probability(1.0E-6, 0.001) shouldBe 1.0
    difference.isProbablyZero(0.00001) shouldBe true
  }


}
