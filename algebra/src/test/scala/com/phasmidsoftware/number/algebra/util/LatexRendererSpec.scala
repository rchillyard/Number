/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for LatexRenderer typeclass infrastructure.
  */
class LatexRendererSpec extends AnyFlatSpec with Matchers {

  behavior of "LatexRenderer typeclass"

  it should "provide summoner method" in {
    case class TestType(value: Int)
    implicit val testRenderer: LatexRenderer[TestType] = LatexRenderer.instance(t => s"${t.value}")

    val renderer = LatexRenderer[TestType]
    renderer.toLatex(TestType(42)) shouldBe "42"
  }

  it should "provide instance constructor" in {
    case class TestType(value: String)
    val renderer = LatexRenderer.instance[TestType](_.value)

    renderer.toLatex(TestType("hello")) shouldBe "hello"
  }

  it should "provide syntax extension" in {
    import LatexRenderer.*

    case class TestType(value: String)
    implicit val testRenderer: LatexRenderer[TestType] = LatexRenderer.instance(_.value)

    TestType("world").toLatex shouldBe "world"
  }

  behavior of "LatexRenderer helper methods"

  it should "wrap content in inline math mode" in {
    LatexRenderer.mathMode("x^2") shouldBe "$x^2$"
  }

  it should "wrap content in display math mode" in {
    LatexRenderer.mathMode("x^2", inline = false) shouldBe "\\[x^2\\]"
  }

  it should "create fractions" in {
    LatexRenderer.frac("1", "2") shouldBe "\\tfrac{1}{2}"
    LatexRenderer.frac("a", "b") shouldBe "\\tfrac{a}{b}"
  }

  it should "create square roots" in {
    LatexRenderer.sqrt("2") shouldBe "\\sqrt{2}"
    LatexRenderer.sqrt("x") shouldBe "\\sqrt{x}"
  }

  it should "create nth roots" in {
    LatexRenderer.nthRoot(3, "8") shouldBe "\\sqrt[3]{8}"
    LatexRenderer.nthRoot(5, "x") shouldBe "\\sqrt[5]{x}"
  }

  it should "format signs correctly" in {
    LatexRenderer.sign(isPositive = true) shouldBe "+"
    LatexRenderer.sign(isPositive = false) shouldBe "-"
  }
}
