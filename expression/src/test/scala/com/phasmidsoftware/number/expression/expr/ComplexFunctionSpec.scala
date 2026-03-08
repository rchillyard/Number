/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Complex, Eager, InversePower, WholeNumber}
import com.phasmidsoftware.number.core.numerical.ComplexCartesian
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Tests for sin, cos, sinh, cosh, exp, and log applied to complex arguments.
  *
  * Many of these tests are // pending until complex argument support is verified
  * or implemented in the expression layer.
  *
  * Key identities under test:
  * sin(ix)  =  i·sinh(x)
  * cos(ix)  =  cosh(x)
  * sinh(ix) =  i·sin(x)
  * cosh(ix) =  cos(x)
  * exp(iπ)  = -1         (Euler's identity)
  * log(-1)  =  iπ
  */
class ComplexFunctionSpec extends AnyFlatSpec with should.Matchers {

  // i·π/2 expressed in the expression layer
  private lazy val iPi = BiFunction(I, Pi, Product)
  private lazy val iPiBy2 = BiFunction(I, BiFunction(Pi, Half, Product), Product)
  private lazy val iOne = I * One

  behavior of "exp for complex arguments"

  it should "evaluate exp(iπ) = -1 (Euler's identity)" in {
    (E ∧ iPi).simplify shouldBe MinusOne
  }

  it should "evaluate exp(iπ/2) = i" in {
    val expression = (E ∧ iPiBy2).simplify
    val materialized = expression.materialize
    materialized should matchPattern { case InversePower(2, WholeNumber(-1)) => }
  }

  it should "evaluate exp(i) approximately" in {
    val simplified = (E ∧ I).simplify
    val materialized = simplified.materialize
    materialized shouldBe a[Complex]
    materialized.asInstanceOf[Complex].complex.isSame(ComplexCartesian(0.5403, 0.8415)) shouldBe true
    val result: Eager = simplified.fuzzy
    result.toDouble shouldBe (1.0) +- 1e-10
  }

  behavior of "log for complex arguments"

  it should "evaluate log(-1) = iπ" in {
    MinusOne.ln.simplify shouldBe (I * Pi).simplify
  }

  it should "evaluate log(i) = iπ/2" in {
    I.ln.simplify shouldBe (I * Pi * Half).simplify
  }

  behavior of "sin for complex arguments"

  it should "evaluate sin(i) = i·sinh(1)" in {
    val p = Expression.matchSimpler
    p(I.sin).get shouldBe (I * UniFunction(One, Sinh))
  }

  it should "satisfy sin(ix) = i·sinh(x)" in {
    val x = Two
    val result = Expression.matchSimpler(UniFunction(BiFunction(I, x, Product), Sine))
    result.get shouldBe I * UniFunction(x, Sinh)
  }

  it should "evaluate sin(1 + i) approximately" in {
    // sin(1+i) ≈ 1.2985 + 0.6350i
    val z = One + I
    val result = UniFunction(z, Sine).materialize
    result shouldBe a[Complex]
    result.asInstanceOf[Complex].complex.isSame(ComplexCartesian(1.2985, 0.6350)) shouldBe true
  }

  behavior of "cos for complex arguments"

  it should "evaluate cos(i) = cosh(1)" in {
    val result = I.cos
    val materialized = result.materialize
    materialized.fuzzy.toDouble shouldBe Math.cosh(1.0) +- 1e-4
    result.fuzzy.toDouble shouldBe Math.cosh(1.0) +- 1e-10
  }

  it should "satisfy cos(ix) = cosh(x)" in {
    //    pending // TODO Work Item 10 — requires approximationComplex
    val x = Two
    val lhs = (I * x).cos
    println(lhs.simplify.debug)
    val rhs = x.cosh
    println(rhs.simplify.debug)
    lhs.simplify shouldBe rhs.simplify
  }

  it should "evaluate cos(1 + i) approximately" in {
    val z = One + I
    val materialized = z.cos.materialize
    materialized shouldBe a[Complex]
    materialized.asInstanceOf[Complex].complex.isSame(ComplexCartesian(0.8337, -0.9889)) shouldBe true
  }

  behavior of "sinh for complex arguments"

  it should "evaluate sinh(iπ/2) = i" in {
    // sinh(iπ/2) = i·sin(π/2) = i
    println(iPiBy2)
    println(BiFunction(Pi, Half, Product).sin.simplify)
    println(BiFunction(Pi, Half, Product).simplify)
    val result = iPiBy2.sinh.simplify
    result shouldBe I
  }

  it should "evaluate sinh(iπ) = 0" in {
    val result = iPi.sinh.simplify
    result shouldBe Zero
  }

  it should "satisfy sinh(ix) = i·sin(x)" in {
    val x = Two
    val result = Expression.matchSimpler(UniFunction(BiFunction(I, x, Product), Sinh))
    result.get shouldBe I * UniFunction(x, Sine)
  }

  it should "evaluate sinh(1 + i) approximately" in {
    val z = One + I
    val materialized = z.sinh.materialize
    materialized shouldBe a[Complex]
    materialized.asInstanceOf[Complex].complex.isSame(ComplexCartesian(0.6350, 1.2985)) shouldBe true
  }

  behavior of "cosh for complex arguments"

  it should "evaluate cosh(iπ) = -1" in {
    val result = iPi.cosh.simplify
    result shouldBe MinusOne
  }

  it should "evaluate cosh(iπ/2) = 0" in {
    val result = iPiBy2.cosh.simplify
    result shouldBe Zero
  }

  it should "satisfy cosh(ix) = cos(x)" in {
    val x = Two
    val result = Expression.matchSimpler(UniFunction(BiFunction(I, x, Product), Cosh))
    result.get shouldBe UniFunction(x, Cosine)
  }

  it should "evaluate cosh(1 + i) approximately" in {
    val z = One + I
    val materialized = z.cosh.materialize
    materialized shouldBe a[Complex]
    materialized.asInstanceOf[Complex].complex.isSame(ComplexCartesian(0.8337, 0.9889)) shouldBe true
  }

  behavior of "cross-checks between circular and hyperbolic functions"

  it should "satisfy sin²(z) + cos²(z) = 1 for z = 1 + i" in {
    val z = One + I
    val s = z.sin
    val c = z.cos
    val ss = s * s
    val cc = c * c
    (ss + cc).simplify shouldBe One
  }

  it should "satisfy cosh²(z) - sinh²(z) = 1 for z = 1 + i" in {
    pending // Issue #193
    val z = One + I
    val c = z.cosh
    val s = z.sinh
    ((c * c) - (s * s)).simplify shouldBe One
  }
}