package com.phasmidsoftware.number.expression.algebraic

import com.phasmidsoftware.number.core.algebraic.Quadratic
import com.phasmidsoftware.number.core.expression.{Expression, MinusOne}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AlgebraicSpec extends AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.number.algebra.eager.QuadraticSolution
  import com.phasmidsoftware.number.core.expression.Root.{phi, psi}

  behavior of "worksheet"

  it should "work" in {

    phi.render shouldBe "\uD835\uDED7"

    (phi + 1).render shouldBe "2.6180339887498950(12)"

    (phi * phi).simplify.render shouldBe "Solution: 1.5 + +  √1.25"

    phi.approximation.get.toDouble should ===(1.618033988749895)

    psi.render shouldBe "\uD835\uDED9"

    psi.approximation.get.toDouble should ===(-0.618033988749895 +- 1E-10)

    (phi * psi).simplify shouldBe MinusOne

    phi.equation shouldBe Quadratic(-1, -1)

    (phi * psi).simplify.render shouldBe "-1"

    (phi * phi).simplify.approximation.get.toDouble shouldBe 2.618033988749895 +- 1E-10

    QuadraticEquation.goldenRatioEquation.solve(0) match {
      case q: QuadraticSolution =>
        q.render shouldBe "\uD835\uDED7"
      case _ =>
        fail("not a QuadraticSolution")
    }
  }
}
