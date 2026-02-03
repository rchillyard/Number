/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, NaturalExponential, Real}
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational, Value}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Constants, FuzzyNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TranscendentalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Transcendental constants"

  it should "normalize PiTranscendental to Angle" in {
    val normalized = PiTranscendental.normalize
    normalized shouldBe a[Angle]
    normalized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "normalize ETranscendental to Real" in {
    val normalized = ETranscendental.normalize
    normalized shouldBe a[NaturalExponential]
    normalized.asInstanceOf[NaturalExponential].toDouble shouldBe Math.E +- 1e-10
  }

  it should "normalize L2 to Real" in {
    val normalized = L2.normalize
    normalized shouldBe a[Real]
    normalized.asInstanceOf[Real].toDouble shouldBe Math.log(2.0) +- 1e-10
  }

  it should "normalize lgE to Real" in {
    val normalized = LgE.normalize
    normalized shouldBe a[Real]
    normalized.asInstanceOf[Real].toDouble shouldBe 1.4426950408889634 +- 1e-10
  }

  it should "normalize EulerMascheroni to itself" in {
    val normalized = EulerMascheroni.normalize
    // Euler-Mascheroni constant γ ≈ 0.5772156649
    normalized.render shouldBe "\uD835\uDEFE"
    normalized shouldBe EulerMascheroni
  }


  it should "materialize PiTranscendental to Angle" in {
    val materialized = PiTranscendental.materialize
    materialized shouldBe a[Angle]
    materialized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "materialize ETranscendental to Real" in {
    val materialized = ETranscendental.materialize
    materialized shouldBe a[NaturalExponential]
    materialized.asInstanceOf[NaturalExponential].toDouble shouldBe Math.E +- 1e-10
  }

  it should "materialize 𝛾 to Real" in {
    val materialized: Eager = EulerMascheroni.materialize
    materialized shouldBe a[Real]
    materialized shouldBe Real.𝛾
    materialized.asInstanceOf[Real].toDouble shouldBe 0.57721566490153286060651209 +- 1e-20
  }

  it should "have correct isAtomic" in {
    L2.isAtomic shouldBe true
    LgE.isAtomic shouldBe true
    EulerMascheroni.isAtomic shouldBe true
  }

  behavior of "old tests"

  behavior of "Transcendental"
  it should "evaluate pi" in {
    Pi.evaluateAsIs shouldBe Some(Eager.pi)
  }
  it should "evaluate e" in {
    E.evaluateAsIs shouldBe Some(Eager.e)
  }
  it should "evaluate l2" in {
    L2.evaluateAsIs shouldBe None
    L2.fuzzy should matchPattern { case Real(_, _) => }
    L2.fuzzy.render shouldBe "0.6931471805599453±1.4E-13%"
  }
  // Test for (fixed) Issue #124
  it should "evaluate lg2e" in {
    LgE.evaluateAsIs shouldBe None
    (Two ∧ LgE.expression).materialize shouldEqual Eager.e
  }
  it should "evaluate gamma" in {
    val rational = Rational("7215195811269160757581401126030030388026991699249/12500000000000000000000000000000000000000000000000")
    val fuzz = AbsoluteFuzz(5.0E-51, Box)
    val value1 = Value.fromRational(rational)
    EulerMascheroni.evaluateAsIs.toString shouldBe "Some(0.5772156649015329*)" // matchPattern { case Some(Real(FuzzyNumber(`value1`, PureNumber, Some(`fuzz`)))) => }
    EulerMascheroni.fuzzy.render shouldBe "0.5772156649015329*"
  }
  it should "expression pi" in {
    PiTranscendental.expression shouldEqual Pi
  }
  it should "expression e" in {
    ETranscendental.expression shouldEqual E
  }
  it should "expression l2" in {
    L2.expression shouldEqual Two.ln
  }
  it should "simplify pi" in {
    val actual = Pi.simplify
    val expected = Pi
    actual shouldBe expected
  }
  it should "simplify e" in {
    ETranscendental.simplify shouldBe ETranscendental
  }
  it should "simplify l2" in {
    L2.simplify === Two.ln
  }
  it should "function 1" in {
    val result = PiTranscendental.function(Sine).materialize
    val expected = Eager.zero
    result shouldEqual expected
  }
  it should "function 2" in {
    val natLog2 = L2
    val result = natLog2.function(Exp).simplify
    result.materialize shouldEqual Eager.two
  }
  it should "render" in {
    PiTranscendental.render shouldBe "\uD835\uDED1"
    ETranscendental.render shouldBe "\uD835\uDF00"
    L2.render shouldBe "ln(2)"
  }

}