/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.Eager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
  * Tests for sin, cos, sinh, cosh, exp, and log applied to complex arguments.
  *
  * Many of these tests are pending until complex argument support is verified
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
  private lazy val iPi = Literal(Eager.iPi)
  private lazy val iPiBy2 = iPi / Two
  private lazy val iOne = I * One

  behavior of "exp for complex arguments"

  it should "evaluate exp(iπ) = -1 (Euler's identity)" in {
    (E ∧ iPi).simplify shouldBe MinusOne
  }

  it should "evaluate exp(iπ/2) = i" in {
    pending // Issue #189
    (E ∧ iPiBy2).fuzzy.asDouble shouldBe 0.0 +- 1e-10 // real part
    // imaginary part should be 1
  }

  it should "evaluate exp(i) approximately" in {
    pending // Issue #189
    // exp(i) = cos(1) + i·sin(1) ≈ 0.5403 + 0.8415i
    val result = (E ∧ iOne).fuzzy
    result.asDouble shouldBe Math.cos(1.0) +- 1e-10
  }

  behavior of "log for complex arguments"

  it should "evaluate log(-1) = iπ" in {
    pending // Issue #189
    // log(-1) = iπ
    MinusOne.log shouldBe iPi
  }

  it should "evaluate log(i) = iπ/2" in {
    pending // Issue #189
    I.log shouldBe iPiBy2
  }

  behavior of "sin for complex arguments"

  it should "evaluate sin(i) = i·sinh(1)" in {
    pending // Issue #189
    // sin(i) = i·sinh(1) ≈ 1.1752i
    val result = iOne.sin
    result.fuzzy.asDouble shouldBe 0.0 +- 1e-10 // real part = 0
    // imaginary part ≈ 1.1752
  }

  it should "satisfy sin(ix) = i·sinh(x)" in {
    pending // Issue #189
    val x = Two
    val lhs = (I * x).sin
    val rhs = I * x.sinh
    lhs shouldBe rhs
  }

  it should "evaluate sin(1 + i) approximately" in {
    pending // Issue #189
    // sin(1+i) ≈ 1.2985 + 0.6350i
    val z = One + iOne
    z.sin.fuzzy.asDouble shouldBe 1.2985 +- 1e-3
  }

  behavior of "cos for complex arguments"

  it should "evaluate cos(i) = cosh(1)" in {
    pending // Issue #189
    // cos(i) = cosh(1) ≈ 1.5431
    val result = iOne.cos
    result.fuzzy.asDouble shouldBe Math.cosh(1.0) +- 1e-10
  }

  it should "satisfy cos(ix) = cosh(x)" in {
    pending // Issue #189
    val x = Two
    val lhs = (I * x).cos
    val rhs = x.cosh
    lhs shouldBe rhs
  }

  it should "evaluate cos(1 + i) approximately" in {
    pending // Issue #189
    // cos(1+i) ≈ 0.8337 - 0.9889i
    val z = One + iOne
    z.cos.fuzzy.asDouble shouldBe 0.8337 +- 1e-3
  }

  behavior of "sinh for complex arguments"

  it should "evaluate sinh(iπ/2) = i" in {
    pending // Issue #189
    // sinh(iπ/2) = i·sin(π/2) = i
    val result = iPiBy2.sinh.simplify
    result shouldBe I
  }

  it should "evaluate sinh(iπ) = 0" in {
    pending // Issue #189
    val result = iPi.sinh.simplify
    result shouldBe Zero
  }

  it should "satisfy sinh(ix) = i·sin(x)" in {
    pending // Issue #189
    val x = Two
    val lhs = (I * x).sinh
    val rhs = I * x.sin
    lhs shouldBe rhs
  }

  it should "evaluate sinh(1 + i) approximately" in {
    pending // Issue #189
    // sinh(1+i) ≈ 0.6350 + 1.2985i
    val z = One + iOne
    z.sinh.fuzzy.asDouble shouldBe 0.6350 +- 1e-3
  }

  behavior of "cosh for complex arguments"

  it should "evaluate cosh(iπ) = -1" in {
    pending // Issue #189
    val result = iPi.cosh.simplify
    result shouldBe MinusOne
  }

  it should "evaluate cosh(iπ/2) = 0" in {
    pending // Issue #189
    val result = iPiBy2.cosh
    result shouldBe Zero
  }

  it should "satisfy cosh(ix) = cos(x)" in {
    pending // Issue #189
    val x = Two
    val lhs = (I * x).cosh
    val rhs = x.cos
    lhs shouldBe rhs
  }

  it should "evaluate cosh(1 + i) approximately" in {
    pending // Issue #189
    // cosh(1+i) ≈ 0.8337 + 0.9889i
    val z = One + iOne
    z.cosh.fuzzy.asDouble shouldBe 0.8337 +- 1e-3
  }

  behavior of "cross-checks between circular and hyperbolic functions"

  it should "satisfy sin²(z) + cos²(z) = 1 for z = 1 + i" in {
    pending // Issue #189
    val z = One + iOne
    val s = z.sin
    val c = z.cos
    (s * s) + (c * c) shouldBe One
  }

  it should "satisfy cosh²(z) - sinh²(z) = 1 for z = 1 + i" in {
    pending // Issue #189
    val z = One + iOne
    val c = z.cosh
    val s = z.sinh
    (c * c) - (s * s) shouldBe One
  }
}