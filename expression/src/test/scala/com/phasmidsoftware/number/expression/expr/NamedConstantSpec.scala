/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, NaturalExponential}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NamedConstantSpec extends AnyFlatSpec with should.Matchers {

  behavior of "NamedConstant"

  it should "create pi" in {
    val pi = Pi
    pi.maybeName shouldBe Some("\uD835\uDED1")
    pi.value shouldBe Angle.pi
  }

  it should "create e" in {
    val e = E
    e.maybeName shouldBe Some("e")
    e.value shouldBe NaturalExponential(1)
  }

  it should "normalize to Real" in {
    val pi = Pi
    val normalized = pi.normalize
    normalized shouldBe a[Angle]
    normalized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "render with its name" in {
    Pi.render shouldBe "\uD835\uDED1"
    E.render shouldBe "e"
  }

  it should "materialize to Real" in {
    val pi = Pi
    val materialized = pi.materialize
    materialized shouldBe a[Angle]
    materialized.asInstanceOf[Angle].toDouble shouldBe Math.PI +- 1e-10
  }

  it should "have correct isAtomic" in {
    Pi.isAtomic shouldBe true
  }

  it should "scale correctly" in {
    import Expression.ExpressionOps
    val scaled: Expression = Pi :* Expression(Eager.two)
    val value: Eager = scaled.materialize
    value shouldBe a[Angle]
    value.asInstanceOf[Angle].toDouble shouldBe 2 * Math.PI +- 1e-10
  }
}
