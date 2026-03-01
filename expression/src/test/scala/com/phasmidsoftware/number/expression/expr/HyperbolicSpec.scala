/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class HyperbolicSpec extends AnyFlatSpec with should.Matchers {

  behavior of "sinh"

  it should "give exact zero for sinh(0)" in {
    Zero.sinh.simplify shouldBe Zero
  }

  it should "evaluate sinh(1) approximately" in {
    val result = One.sinh
    result.fuzzy.asDouble shouldBe Math.sinh(1.0) +- 1e-10
  }

  it should "evaluate sinh(-1) approximately" in {
    val result = MinusOne.sinh
    result.fuzzy.asDouble shouldBe Math.sinh(-1.0) +- 1e-10
  }

  it should "satisfy sinh(-x) = -sinh(x)" in {
    val x = Two
    (x + (-x)).isZero shouldBe true
    val pos = x.sinh
    val neg = (-x).sinh
    (pos + neg).simplify shouldBe Zero
    //    pending // TODO Issue #187
  }

  it should "satisfy sin(-x) = -sin(x)" in {
    val x = Two
    val pos = x.sin
    val neg = (-x).sin
    (pos + neg).simplify shouldBe Zero
  }

  behavior of "cosh"

  it should "give exact one for cosh(0)" in {
    val simplified = Zero.cosh.simplify
    simplified shouldBe One
  }
  it should "give exact one for cos(0)" in {
    val simplified = Zero.cos.simplify
    simplified shouldBe One
  }

  it should "evaluate cosh(1) approximately" in {
    val result = One.cosh
    result.fuzzy.asDouble shouldBe Math.cosh(1.0) +- 1e-10
  }

  it should "evaluate cosh(-1) approximately" in {
    val result = MinusOne.cosh
    result.fuzzy.asDouble shouldBe Math.cosh(-1.0) +- 1e-10
  }

  it should "satisfy cosh(-x) = cosh(x)" in {
    val x = Two
    val pos = x.cosh
    val neg = (-x).cosh
    (pos + neg).simplify shouldBe (pos * 2).simplify
  }

  behavior of "sinh and cosh together"

  it should "satisfy cosh²(x) - sinh²(x) = 1" in {
    val x = Literal(Rational(3, 2))
    val c = x.cosh
    val s = x.sinh
    //    ((c * c) - (s * s)).simplify shouldBe One
    pending // TODO Issue #187
  }
}