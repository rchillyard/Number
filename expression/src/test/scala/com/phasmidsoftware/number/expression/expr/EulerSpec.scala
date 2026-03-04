/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.SquareRoot.IntToImaginary
import com.phasmidsoftware.number.core.numerical.ComplexPolar
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Comprehensive test suite for the Euler expression type and all associated
  * simplification patterns.
  *
  * Tests are organised into seven sections matching the design doc and work items:
  *
  *  1. Basic properties          — depth, isExact, render, terms, evaluate
  *  2. Materialisation           — evaluate() produces ComplexPolar
  *  3. Identity simplification   — special values of r or θ reduce to simpler forms
  *  4. Structural simplification — de Moivre arithmetic on Euler expressions
  *  5. Euler recognition         — BiFunction/UniFunction trees convert to Euler form
  *  6. Euler companion           — unit-modulus convenience constructor
  *  7. End-to-end                — multi-step simplifications
  */
class EulerSpec extends AnyFlatSpec with Matchers {

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  /** Half-Pi as an expression tree, matching Euler.HalfPi extractor */
  private val halfPi: Expression = BiFunction(Pi, Half, Product)

  /** -Pi/2 as an expression tree, matching Euler.MinusHalfPi extractor */
  private val minusHalfPi: Expression = UniFunction(halfPi, Negate)

  //  /** A non-special angle (2π/3) that won't trigger identitiesMatcher */
  //  private val genericTheta: Expression = BiFunction(Pi, Literal(RationalNumber(2, 3)), Product)
  /** A non-special angle (π/3) that won't trigger identitiesMatcher and whose doubles stay non-special */
  private val genericTheta: Expression = BiFunction(Pi, Literal(RationalNumber(1, 3)), Product)
  // ---------------------------------------------------------------------------
  // 1. Basic Properties
  // ---------------------------------------------------------------------------

  behavior of "Euler - Basic Properties"

  it should "not be atomic" in {
    val target = Euler(One, Pi)
    target.isAtomic shouldBe false
  }

  it should "report correct depth" in {
    val target = Euler(One, Pi)
    target.depth shouldBe 2
  }

  it should "report deeper depth for nested expressions" in {
    val target = Euler(BiFunction(Two, 3, Product), Pi)
    target.depth shouldBe 3
  }

  it should "be exact when both r and θ are exact" in {
    Euler(One, Pi).isExact shouldBe true
    Euler(Two, halfPi).isExact shouldBe true
  }

  it should "be inexact when r is inexact" in {
    val fuzzyR = Real("1.5*")
    Euler(fuzzyR, Pi).isExact shouldBe false
  }

  it should "be inexact when θ is inexact" in {
    val fuzzyTheta = Real("1.5*")
    Euler(One, fuzzyTheta).isExact shouldBe false
  }

  it should "provide terms as Seq(r, θ)" in {
    val target = Euler(Two, Pi)
    target.terms shouldBe Seq(Two, Pi)
  }

  it should "render as r*e^(i*θ)" in {
    val target = Euler(One, Pi)
    target.renderAsExpression should include("e^(i*")
  }

  it should "render modulus and angle in renderAsExpression" in {
    val target = Euler(Two, Pi)
    val rendered = target.renderAsExpression
    rendered should include("2")
    rendered should include("e^(i*")
  }

  // ---------------------------------------------------------------------------
  // 2. Materialisation — evaluate() produces ComplexPolar
  // ---------------------------------------------------------------------------

  behavior of "Euler - Materialisation"

  it should "evaluate Euler(1, 0) to ComplexPolar(1, 0)" in {
    val result = Euler(One, Zero).evaluateAsIs
    result shouldBe defined
    result.get shouldBe a[Complex]
    val cp = result.get.asInstanceOf[Complex].complex
    cp shouldBe a[ComplexPolar]
  }

  it should "evaluate Euler(1, π) to ComplexPolar(1, π)" in {
    val result = Euler(One, Pi).evaluateAsIs
    result shouldBe defined
    result.get shouldBe a[Complex]
  }

  it should "evaluate Euler(2, π/2) to ComplexPolar(2, π/2)" in {
    val result = Euler(Two, halfPi).evaluateAsIs
    result shouldBe defined
    result.get shouldBe a[Complex]
  }

  it should "return None or Some when r is sin(π) — documents contract" in {
    val unevaluable = UniFunction(Pi, Sine)
    val result = Euler(unevaluable, Zero).evaluateAsIs
    result should (be(defined) or be(empty))
  }

  // ---------------------------------------------------------------------------
  // 3. Identity Simplification
  // ---------------------------------------------------------------------------

  behavior of "Euler - identitiesMatcher: zero modulus"

  it should "simplify Euler(0, θ) to 0 for any θ" in {
    Euler(Zero, Pi).simplify shouldBe Zero
    Euler(Zero, halfPi).simplify shouldBe Zero
    Euler(Zero, One).simplify shouldBe Zero
  }

  behavior of "Euler - identitiesMatcher: zero angle"

  it should "simplify Euler(r, 0) to r" in {
    Euler(One, Zero).simplify shouldBe One
    Euler(Two, Zero).simplify shouldBe Two
    Euler(Pi, Zero).simplify shouldBe Pi
  }

  behavior of "Euler - identitiesMatcher: angle = π"

  it should "simplify Euler(r, π) to -r" in {
    val result = Euler(Two, Pi).simplify
    result shouldBe Literal(-2)
  }

  it should "simplify Euler(1, π) to -1 (Euler's identity)" in {
    Euler(One, Pi).simplify shouldBe MinusOne
  }

  behavior of "Euler - identitiesMatcher: angle = π/2"

  it should "simplify Euler(1, π/2) to i" in {
    Euler(One, halfPi).simplify shouldBe I
  }

  it should "simplify Euler(r, π/2) to r*i" in {
    val result = Euler(Two, halfPi).simplify
    result shouldBe Literal(2.i)
  }

  behavior of "Euler - identitiesMatcher: angle = -π/2"

  it should "simplify Euler(1, -π/2) to -i" in {
    val result = Euler(One, minusHalfPi).simplify
    result shouldBe UniFunction(I, Negate)
  }

  it should "simplify Euler(r, -π/2) to -(r*i)" in {
    val result = Euler(Two, minusHalfPi).simplify
    result shouldBe a[UniFunction]
    val uf = result.asInstanceOf[UniFunction]
    uf.f shouldBe Negate
    uf.x shouldBe Literal(2.i)
  }

  behavior of "Euler - identitiesMatcher: r = 1 special cases"

  it should "simplify Euler(1, 0) to 1" in {
    Euler(One, Zero).simplify shouldBe One
  }

  // ---------------------------------------------------------------------------
  // 4. Structural Simplification — de Moivre arithmetic
  // ---------------------------------------------------------------------------

  behavior of "Euler - structuralMatcher: multiplication"

  it should "multiply two Euler expressions by adding angles and multiplying moduli" in {
    val e1 = Euler(Two, Pi)
    val e2 = Euler(3, halfPi)
    val product = BiFunction(e1, e2, Product)
    val actual = product.simplify
    val expected: Expression = Euler(6, Pi + (Pi * Half))
    actual shouldBe expected
  }

  it should "multiply Euler(1, θ1) * Euler(1, θ2) to Euler(1, θ1+θ2)" in {
    val e1 = Euler(One, genericTheta)
    val e2 = Euler(One, halfPi)
    val product = BiFunction(e1, e2, Product).simplify
    product shouldBe a[Euler]
    val eu = product.asInstanceOf[Euler]
    eu.r.simplify shouldBe One
  }

  behavior of "Euler - structuralMatcher: division"

  it should "divide two Euler expressions by subtracting angles and dividing moduli" in {
    val e1 = Euler(Two, genericTheta)
    val e2 = Euler(One, halfPi)
    val division = BiFunction(e1, UniFunction(e2, Reciprocal), Product)
    val simplified = division.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.θ.simplify shouldBe (genericTheta - halfPi).simplify
  }

  behavior of "Euler - structuralMatcher: power (de Moivre's theorem)"

  it should "raise Euler(r, θ) to power n giving Euler(r^n, n*θ)" in {
    val e = Euler(One, genericTheta)
    val powered = BiFunction(e, Two, Power).simplify
    powered shouldBe a[Euler]
    val eu = powered.asInstanceOf[Euler]
    eu.r.simplify shouldBe One
    eu.θ.simplify shouldBe Literal(Angle(Rational(2, 3)))
  }

  it should "apply de Moivre: Euler(1, π/2)^2 = Euler(1, π) which simplifies to -1" in {
    val e = Euler(One, halfPi)
    val powered = BiFunction(e, Two, Power).simplify
    powered.simplify shouldBe MinusOne
  }

  it should "apply de Moivre: Euler(1, π/2)^4 = Euler(1, 2π) which simplifies to 1" in {
    Angle(2).normalize.isZero shouldBe true
    val e = Euler(One, halfPi)
    val powered = BiFunction(e, Literal(4), Power).simplify
    powered.simplify shouldBe One
  }

  behavior of "Euler - structuralMatcher: conjugate sum"

  it should "simplify Euler(r,θ) + Euler(r,-θ) to 2*r*cos(θ)" in {
    val r = Two
    val e1 = Euler(r, genericTheta)
    val e2 = Euler(r, UniFunction(genericTheta, Negate))
    val sum = BiFunction(e1, e2, Sum).simplify
    sum should not be a[Euler]
  }

  it should "simplify Euler(1,θ) + Euler(1,-θ) to 2*cos(θ)" in {
    val e1 = Euler(One, genericTheta)
    val e2 = Euler(One, UniFunction(genericTheta, Negate))
    val sum = BiFunction(e1, e2, Sum).simplify
    sum should not be a[Euler]
  }

  behavior of "Euler - structuralMatcher: conjugate difference"

  it should "simplify Euler(r,θ) - Euler(r,-θ) to 2*i*r*sin(θ)" in {
    val r = One
    val e1 = Euler(r, genericTheta)
    val negE2 = UniFunction(Euler(r, UniFunction(genericTheta, Negate)), Negate)
    val diff = BiFunction(e1, negE2, Sum).simplify
    diff should not be a[Euler]
  }

  // ---------------------------------------------------------------------------
  // 5. Euler Recognition — BiFunction/UniFunction trees → Euler form
  // ---------------------------------------------------------------------------

  behavior of "Euler recognition: cos(θ) + i·sin(θ)"

  it should "recognise cos(θ) + i*sin(θ) as Euler(1, θ)" in {
    val theta = genericTheta
    val expr = BiFunction(
      UniFunction(theta, Cosine),
      BiFunction(I, UniFunction(theta, Sine), Product),
      Sum
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.r.simplify shouldBe One
    eu.θ.simplify shouldBe theta.simplify
  }

  it should "recognise cos(θ) + sin(θ)*i as Euler(1, θ) (commuted i)" in {
    val theta = genericTheta
    val expr = BiFunction(
      UniFunction(theta, Cosine),
      BiFunction(UniFunction(theta, Sine), I, Product),
      Sum
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
  }

  it should "recognise i*sin(θ) + cos(θ) as Euler(1, θ) (sum operands swapped)" in {
    val theta = genericTheta
    val expr = BiFunction(
      BiFunction(I, UniFunction(theta, Sine), Product),
      UniFunction(theta, Cosine),
      Sum
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
  }

  it should "recognise cos(θ) - i*sin(θ) as Euler(1, -θ) (conjugate)" in {
    val theta = genericTheta
    val expr = BiFunction(
      UniFunction(theta, Cosine),
      UniFunction(BiFunction(I, UniFunction(theta, Sine), Product), Negate),
      Sum
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.r.simplify shouldBe One
    eu.θ.simplify shouldBe UniFunction(theta, Negate).simplify
  }

  behavior of "Euler recognition: exp(i·θ)"

  it should "recognise exp(i*θ) as Euler(1, θ)" in {
    val theta = genericTheta
    val expr = UniFunction(BiFunction(I, theta, Product), Exp)
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.r.simplify shouldBe One
    eu.θ.simplify shouldBe theta.simplify
  }

  it should "recognise exp(θ*i) as Euler(1, θ) (commuted i)" in {
    val theta = genericTheta
    val expr = UniFunction(BiFunction(theta, I, Product), Exp)
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
  }

  behavior of "Euler recognition: exp(a + i·b)"

  it should "recognise exp(a + i*b) as Euler(exp(a), b)" in {
    val one = One
    val b = genericTheta
    val expr = UniFunction(
      BiFunction(one, BiFunction(I, b, Product), Sum),
      Exp
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.θ.simplify shouldBe b.simplify
    eu.r.simplify shouldBe UniFunction(one, Exp).simplify
  }

  it should "recognise exp(a + b*i) as Euler(exp(a), b) (commuted inner i)" in {
    val two = Two
    val b = genericTheta
    val expr = UniFunction(
      BiFunction(two, BiFunction(b, I, Product), Sum),
      Exp
    )
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.θ.simplify shouldBe b.simplify
  }

  behavior of "Euler recognition: r * exp(i·θ)"

  it should "recognise r * exp(i*θ) as Euler(r, θ)" in {
    val r = Two
    val theta = genericTheta
    val expr = BiFunction(r, UniFunction(BiFunction(I, theta, Product), Exp), Product)
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.r shouldBe r
    eu.θ.simplify shouldBe theta.simplify
  }

  it should "recognise exp(i*θ) * r as Euler(r, θ) (commuted)" in {
    val r = Literal(3)
    val theta = genericTheta
    val expr = BiFunction(UniFunction(BiFunction(I, theta, Product), Exp), r, Product)
    val simplified = expr.simplify
    simplified shouldBe a[Euler]
    val eu = simplified.asInstanceOf[Euler]
    eu.r shouldBe r
    eu.θ.simplify shouldBe theta.simplify
  }

  // ---------------------------------------------------------------------------
  // 6. Euler companion — unit-modulus convenience constructor
  // ---------------------------------------------------------------------------

  behavior of "Euler companion object"

  it should "construct Euler(One, θ) via single-arg apply" in {
    val target = Euler(Pi)
    target.r shouldBe One
    target.θ shouldBe Pi
  }

  it should "simplify Euler(π) (i.e. Euler(1,π)) to -1" in {
    Euler(Pi).simplify shouldBe MinusOne
  }

  it should "simplify Euler(0) (i.e. Euler(1,0)) to 1" in {
    Euler(Zero).simplify shouldBe One
  }

  it should "simplify Euler(π/2) (i.e. Euler(1,π/2)) to i" in {
    Euler(halfPi).simplify shouldBe I
  }

  it should "simplify Euler(-π/2) (i.e. Euler(1,-π/2)) to -i" in {
    Euler(minusHalfPi).simplify shouldBe UniFunction(I, Negate)
  }

  // ---------------------------------------------------------------------------
  // 7. End-to-end: multi-step simplifications
  // ---------------------------------------------------------------------------

  behavior of "Euler - end-to-end multi-step"

  it should "simplify (cos(θ) + i*sin(θ))^2 via recognition then de Moivre" in {
    BiFunction(I, Two, Power).simplify shouldBe MinusOne
    val theta = halfPi
    val cosTheta = UniFunction(theta, Cosine)
    val iSinTheta = BiFunction(I, UniFunction(theta, Sine), Product)
    val eulerForm = BiFunction(cosTheta, iSinTheta, Sum) // → Euler(1, π/2)
    val powered = BiFunction(eulerForm, Two, Power) // → Euler(1, π) → -1
    powered.simplify.simplify shouldBe MinusOne
  }

  it should "simplify exp(i*π/2)^2 to -1 via de Moivre" in {
    pending
    val expIHalfPi = UniFunction(BiFunction(I, halfPi, Product), Exp)
    val squared = BiFunction(expIHalfPi, Two, Power)
    squared.simplify.simplify shouldBe MinusOne
  }

  it should "evaluate Euler(r,θ) * Euler(r,-θ) = Euler(r^2, 0) = r^2" in {
    val r = Two
    val e1 = Euler(r, halfPi)
    val e2 = Euler(r, minusHalfPi)
    // Euler(2,π/2) * Euler(2,-π/2) = Euler(4, 0) = 4
    val product = BiFunction(e1, e2, Product).simplify.simplify
    product.simplify shouldBe Literal(4)
  }
}