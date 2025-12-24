/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Angle, NatLog, Real}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TranscendentalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Transcendental constants"

  it should "normalize Pi to Angle" in {
    val normalized = Pi.normalize
    normalized shouldBe a[Angle]
    normalized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "normalize E to Real" in {
    val normalized = E.normalize
    normalized shouldBe a[NatLog]
    normalized.asInstanceOf[NatLog].toDouble shouldBe Math.E +- 1e-10
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

  it should "render Pi correctly" in {
    Pi.render shouldBe "\uD835\uDED1"
  }

  it should "render E correctly" in {
    E.render shouldBe "\uD835\uDF00"
  }

  it should "materialize Pi to Angle" in {
    val materialized = Pi.materialize
    materialized shouldBe a[Angle]
    materialized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "materialize E to Real" in {
    val materialized = E.materialize
    materialized shouldBe a[NatLog]
    materialized.asInstanceOf[NatLog].toDouble shouldBe Math.E +- 1e-10
  }

  it should "materialize 𝛾 to Real" in {
    val materialized = EulerMascheroni.materialize
    materialized shouldBe a[Real]
    materialized shouldBe Real.𝛾
    materialized.asInstanceOf[Real].toDouble shouldBe 0.57721566490153286060651209 +- 1e-20
  }

  it should "have correct isAtomic" in {
    Pi.isAtomic shouldBe true
    E.isAtomic shouldBe true
    L2.isAtomic shouldBe true
    LgE.isAtomic shouldBe true
    EulerMascheroni.isAtomic shouldBe true
  }
}