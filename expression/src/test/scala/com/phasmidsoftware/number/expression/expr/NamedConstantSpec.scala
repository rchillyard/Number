/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NamedConstantSpec extends AnyFlatSpec with should.Matchers {

  behavior of "NamedConstant"

  it should "create pi" in {
    val pi = ConstPi
    pi.maybeName shouldBe Some("π")
    pi.value shouldBe Angle.pi
  }

  it should "create e" in {
    val e = ConstE
    e.maybeName shouldBe Some("e")
    e.value shouldBe NatLog(1)
  }

  it should "normalize to Real" in {
    val pi = ConstPi
    val normalized = pi.normalize
    normalized shouldBe a[Angle]
    normalized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "render with its name" in {
    ConstPi.render shouldBe "π"
    ConstE.render shouldBe "e"
  }

  it should "materialize to Real" in {
    val pi = ConstPi
    val materialized = pi.materialize
    materialized shouldBe a[Angle]
    materialized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "have correct isAtomic" in {
    ConstPi.isAtomic shouldBe true
  }

  it should "scale correctly" in {
    val pi = ConstPi
    val scaled: Eager = pi.*(WholeNumber(2))
    scaled shouldBe a[Angle]
    scaled.asInstanceOf[Angle].toDouble shouldBe 2 * Math.PI +- 1e-10
  }
}
