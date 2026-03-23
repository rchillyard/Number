package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.{Eager, Real}
import com.phasmidsoftware.number.core.numerical.{Box, RelativeFuzz}
import com.phasmidsoftware.number.expression.expr.{Expression, Pi}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FoucaultSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Foucault worksheets"
  it should "perform Foucault1" in {
    val g: Expression = Real(9.81, Some(RelativeFuzz(1E-4, Box)))
    val t: Expression = Real(16.5, Some(RelativeFuzz(0.01, Box)))
    val expression = g * ((t / Pi / 2) ∧ 2)
    val length: Eager = expression.materialize
    length.render shouldBe "6.76[14]E+01"
    (length ~= Eager(67.65)) shouldBe true
  }
}
