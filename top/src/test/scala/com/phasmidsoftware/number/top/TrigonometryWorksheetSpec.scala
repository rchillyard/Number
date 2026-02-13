package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.expression.expr.MinusOne
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TrigonometryWorksheetSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Trigonometry worksheet"
  it should "work" in {

    import com.phasmidsoftware.number.algebra.eager.Eager
    import com.phasmidsoftware.number.expression.expr.{E, I, Pi}

    val iPi = (I * Pi).simplify
    iPi shouldBe I * Pi
    val euler = (E ∧ iPi).simplify
    euler shouldBe MinusOne
    euler.materialize.render shouldBe "-1"

    val piBy2 = Pi / 2
    piBy2.cos.materialize shouldBe Eager.zero
    piBy2.sin.materialize shouldBe Eager.one
    piBy2.tan.materialize shouldBe Eager.infinity



    // You should see
    (piBy2 / 2).cos.materialize.render shouldBe "√½"
    (piBy2 / 3).cos.materialize.render should startWith ("0.86602540378")
    (Pi / 3).sin.materialize.render should startWith ("0.86602540378")

  }
}
