package com.phasmidsoftware.number.top

import com.phasmidsoftware.number.algebra.eager.Eager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NumberUpdatedSpec extends AnyFlatSpec with should.Matchers {

  import cats.implicits.*
  import com.phasmidsoftware.number.algebra.core.AnyContext
  import com.phasmidsoftware.number.core.inner.Rational
  import com.phasmidsoftware.number.core.inner.Rational.RationalOps
  import com.phasmidsoftware.number.expression.expr.*

  behavior of "NumberUpdated"
  it should "get one half" in {

    // NOTE Demonstrate that sin(π/4) is an exact number.
    val piBy4 = Pi / 4
    piBy4.show shouldBe "¼\uD835\uDED1"
    val sinePiBy4 = piBy4.sin
    sinePiBy4.show shouldBe "√½"
    val oneHalf = (sinePiBy4 * sinePiBy4)
    oneHalf.show shouldBe "½"
  }
  it should "evaluate root 3" in {
    val biFunction = BiFunction(3, Rational(1, 2), Power)
    biFunction.evaluate(AnyContext) shouldBe Some(Eager.root3)
  }
  it should "work" in {
    val root3 = Root.√(3)
    val two = (root3 + 1) * (root3 - 1)
    two.materialize shouldBe Eager.two

    val half = 1 :/ 2
    half shouldBe Rational.half

    val infinity = 1 :/ 0 // should be infinity
    infinity shouldBe Rational.infinity

    // NOTE Demonstrate that sin(π/4) is an exact number.
    val piBy4 = Pi / 4
    piBy4.show shouldBe "¼\uD835\uDED1"
    val sinePiBy4 = piBy4.sin
    val oneHalf = (sinePiBy4 * sinePiBy4)
    oneHalf.show shouldBe "½"

    val biFunction = BiFunction(Literal(3), Literal(Rational(1, 2)), Power)
    biFunction.evaluate(AnyContext)
  }
}
