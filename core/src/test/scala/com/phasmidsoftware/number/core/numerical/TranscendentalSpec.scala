/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.expression.*
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, Value}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TranscendentalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Transcendental"
  it should "evaluate pi" in {
    Pi.evaluateAsIs shouldBe Some(Constants.pi)
  }
  it should "evaluate e" in {
    E.evaluateAsIs shouldBe Some(Constants.e)
  }
  it should "evaluate l2" in {
    L2.evaluateAsIs shouldBe None
    L2.asNumber should matchPattern { case Some(FuzzyNumber(_, _, _)) => }
    L2.asNumber map (_.render) shouldBe Some("0.6931471805599453±.00000000000020%")
  }
  // Test for (fixed) Issue #124
  it should "evaluate lg2e" in {
    LgE.evaluateAsIs shouldBe None
    (Two ∧ LgE.expression).simplify shouldEqual ConstE
  }
  it should "evaluate gamma" in {
    val rational = Rational("7215195811269160757581401126030030388026991699249/12500000000000000000000000000000000000000000000000")
    val fuzz = AbsoluteFuzz(5.0E-51, Box)
    val value1 = Value.fromRational(rational)
    EulerMascheroni.evaluateAsIs should matchPattern { case Some(Real(FuzzyNumber(`value1`, PureNumber, Some(`fuzz`)))) => }
    EulerMascheroni.asNumber map (_.render) shouldBe Some("0.5772156649015329*")
  }
  it should "expression pi" in {
    Pi.expression shouldEqual ConstPi
  }
  it should "expression e" in {
    E.expression shouldEqual ConstE
  }
  it should "expression l2" in {
    L2.expression shouldEqual Two.ln
  }
  it should "simplify pi" in {
    val actual = Pi.simplify
    val expected = ConstPi
    actual shouldBe expected
  }
  it should "simplify e" in {
    E.simplify shouldBe ConstE
  }
  it should "simplify l2" in {
    L2.simplify === Two.ln
  }
  it should "function 1" in {
    val result = Pi.function(Sine).simplify
    val expected = Expression.zero
    result shouldEqual expected
  }
  it should "function 2" in {
    val natLog2 = L2
    val result = natLog2.function(Exp).simplify
    val expected = Expression.two
    result shouldEqual expected
  }
  it should "render" in {
    Pi.render shouldBe "\uDED1"
    E.render shouldBe "\uD835\uDF00"
    L2.render shouldBe "ln(2)"
  }
}
