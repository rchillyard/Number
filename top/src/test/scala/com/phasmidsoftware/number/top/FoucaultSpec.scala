package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.Eager
import com.phasmidsoftware.number.expression.expr.{ConstPi, Expression}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FoucaultSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Foucault worksheets"
  it should "perform Foucault1" in {
    val g: Expression = Eager("9.81*")
    val t: Expression = Eager("16.5*")
    val expression = g * ((t / ConstPi / 2) ∧ 2)
    val length: Eager = expression.materialize
    (length ~= Eager(67.65)) shouldBe true
  }
}
