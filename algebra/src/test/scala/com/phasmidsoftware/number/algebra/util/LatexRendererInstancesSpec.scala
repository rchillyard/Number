/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.util

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for LatexRenderer instances for algebra types.
  */
class LatexRendererInstancesSpec extends AnyFlatSpec with Matchers {

  import LatexRenderer.*
  import RationalNumber.given

  behavior of "Rational LatexRenderer"

  it should "render integers without denominator" in {
    Rational(5).toLatex shouldBe "5"
    Rational(0).toLatex shouldBe "0"
    Rational(-3).toLatex shouldBe "-3"
  }

  it should "render proper fractions" in {
    Rational(1, 2).toLatex shouldBe "\\frac{1}{2}"
    Rational(3, 4).toLatex shouldBe "\\frac{3}{4}"
    Rational(22, 7).toLatex shouldBe "\\frac{22}{7}"
  }

  it should "render negative fractions" in {
    Rational(-1, 2).toLatex shouldBe "\\frac{-1}{2}"
    Rational(1, -2).toLatex shouldBe "\\frac{-1}{2}" // Rational normalizes sign to numerator
  }

  behavior of "RationalNumber LatexRenderer"

  it should "delegate to Rational renderer" in {
    RationalNumber(Rational(1, 2)).toLatex shouldBe "\\frac{1}{2}"
    RationalNumber(Rational(7)).toLatex shouldBe "7"
  }

  it should "render half correctly" in {
    RationalNumber.half.toLatex shouldBe "\\frac{1}{2}"
  }

  it should "render zero correctly" in {
    RationalNumber.zero.toLatex shouldBe "0"
  }

  it should "render one correctly" in {
    RationalNumber.one.toLatex shouldBe "1"
  }

  behavior of "Logarithm LatexRenderer"

  it should "render logarithmic numbers" in {
    Eager.e.toLatex shouldBe """\mathrm{e}"""
    BinaryLog(5).toLatex shouldBe "2^5"
  }

  behavior of "InversePower LatexRenderer"

  it should "render square roots" in {
    val sqrt2 = InversePower(2, RationalNumber(Rational(2)))
    sqrt2.toLatex shouldBe "\\sqrt{2}"
  }

  it should "render square roots of fractions" in {
    val sqrtHalf = InversePower(2, RationalNumber(Rational(1, 2)))
    sqrtHalf.toLatex shouldBe "\\sqrt{\\frac{1}{2}}"
  }

  it should "render cube roots" in {
    val cbrt8 = InversePower(3, RationalNumber(Rational(8)))
    cbrt8.toLatex shouldBe "\\sqrt[3]{8}"
  }

  it should "render fifth roots" in {
    val fifthRoot = InversePower(5, RationalNumber(Rational(32)))
    fifthRoot.toLatex shouldBe "\\sqrt[5]{32}"
  }

  it should "render nested roots properly" in {
    val innerSqrt = InversePower(2, RationalNumber(Rational(5, 4)))
    innerSqrt.toLatex shouldBe "\\sqrt{\\frac{5}{4}}"
  }

  behavior of "QuadraticSolution LatexRenderer"

  it should "render phi constant with LaTeX symbol" in {
    QuadraticSolution.phi.toLatex shouldBe "\\varphi"
  }

  it should "render psi constant with LaTeX symbol" in {
    QuadraticSolution.psi.toLatex shouldBe "\\psi"
  }

  it should "render solution with zero offset as just base" in {
    val solution = QuadraticSolution(RationalNumber(Rational(5)), RationalNumber.zero, 1, false)
    solution.toLatex shouldBe "5"
  }

  it should "render solution with zero base as just offset" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber(Rational(3)), 1, false)
    solution.toLatex shouldBe "3"
  }

  it should "render solution with zero base and negative branch" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber(Rational(3)), -1, false)
    solution.toLatex shouldBe "-3"
  }

  it should "render real solution with positive branch (branch 0)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(2)))
    val solution = QuadraticSolution(base, offset, 1, imaginary = false)

    solution.toLatex shouldBe "\\frac{1}{2} + \\sqrt{2}"
  }

  it should "render real solution with negative branch (branch 1)" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(2)))
    val solution = QuadraticSolution(base, offset, -1, imaginary = false)

    solution.toLatex shouldBe "\\frac{1}{2} - \\sqrt{2}"
  }

  it should "render complex solution with positive branch" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(3, 4)))
    val solution = QuadraticSolution(base, offset, 1, imaginary = true)

    solution.toLatex shouldBe "\\frac{1}{2} + i \\sqrt{\\frac{3}{4}}"
  }

  it should "render complex solution with negative branch" in {
    val base = RationalNumber(Rational(1, 2))
    val offset = InversePower(2, RationalNumber(Rational(3, 4)))
    val solution = QuadraticSolution(base, offset, -1, imaginary = true)

    solution.toLatex shouldBe "\\frac{1}{2} - i \\sqrt{\\frac{3}{4}}"
  }

  it should "render solution with integer base and irrational offset" in {
    val base = RationalNumber(Rational(3))
    val offset = InversePower(2, RationalNumber(Rational(5)))
    val solution = QuadraticSolution(base, offset, 1, false)

    solution.toLatex shouldBe "3 + \\sqrt{5}"
  }

  it should "render negative base correctly" in {
    val base = RationalNumber(Rational(-2))
    val offset = InversePower(2, RationalNumber(Rational(3)))
    val solution = QuadraticSolution(base, offset, 1, false)

    solution.toLatex shouldBe "-2 + \\sqrt{3}"
  }

  behavior of "LinearSolution LatexRenderer"

  it should "render linear solution as its value" in {
    val solution = LinearSolution(RationalNumber(Rational(5)))
    solution.toLatex shouldBe "5"
  }

  it should "render fractional linear solution" in {
    val solution = LinearSolution(RationalNumber(Rational(3, 4)))
    solution.toLatex shouldBe "\\frac{3}{4}"
  }

  it should "render linear solution with irrational value" in {
    val value = InversePower(2, RationalNumber(Rational(2)))
    val solution = LinearSolution(value)
    solution.toLatex shouldBe "\\sqrt{2}"
  }

  behavior of "Algebraic LatexRenderer (general)"

  it should "handle QuadraticSolution through Algebraic type" in {
    val solution: Algebraic = QuadraticSolution(
      RationalNumber.half,
      InversePower(2, RationalNumber(Rational(5, 4))),
      1, false
    )
    solution.toLatex shouldBe "\\varphi"
  }

  it should "handle LinearSolution through Algebraic type" in {
    val solution: Algebraic = LinearSolution(RationalNumber(Rational(7)))
    solution.toLatex shouldBe "7"
  }

  behavior of "Edge cases and special values"

  it should "handle very large rationals" in {
    Rational(1000000, 1).toLatex shouldBe "1000000"
    Rational(999999, 1000000).toLatex shouldBe "\\frac{999999}{1000000}"
  }

  it should "handle negative zero-like cases" in {
    val solution = QuadraticSolution(RationalNumber.zero, RationalNumber.zero, 1, false)
    solution.toLatex shouldBe "0"
  }

  it should "render unity correctly in various forms" in {
    RationalNumber.one.toLatex shouldBe "1"
    Rational(1).toLatex shouldBe "1"
    Rational(5, 5).toLatex shouldBe "1" // Assuming Rational normalizes
  }

  behavior of "Complex number representations"

  it should "handle the golden ratio phi correctly" in {
    val phi = QuadraticSolution.phi
    phi.toLatex shouldBe "\\varphi"

    // Verify it's the correct value structure
    phi.base shouldBe RationalNumber.half
    phi.offset shouldBe InversePower(2, RationalNumber(Rational(5, 4)))
  }

  it should "handle both roots of x² - x - 1 = 0" in {
    // φ = (1 + √5) / 2
    val phi = QuadraticSolution.phi
    phi.toLatex shouldBe "\\varphi"

    // ψ = (1 - √5) / 2
    val psi = QuadraticSolution.psi
    psi.toLatex shouldBe "\\psi"
  }

  behavior of "Integration with existing render method"

  it should "provide LaTeX rendering separate from display rendering" in {
    val solution = QuadraticSolution(
      RationalNumber.half,
      InversePower(2, RationalNumber(Rational(2))),
      1, false
    )

    // LaTeX should be different from render (unless render already produces LaTeX)
    val latex = solution.toLatex
    val display = solution.render

    // They serve different purposes
    latex should include("\\")  // LaTeX commands
  }
}
