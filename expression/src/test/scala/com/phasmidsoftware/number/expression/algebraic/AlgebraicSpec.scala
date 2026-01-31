package com.phasmidsoftware.number.expression.algebraic

import com.phasmidsoftware.number.expression.expr.{Expression, MinusOne, One}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should.Matchers.shouldBe

class AlgebraicSpec extends AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.number.algebra.eager.Eager.half
  import com.phasmidsoftware.number.algebra.eager.{Eager, QuadraticSolution, Solution}
  import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic
  import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
  import com.phasmidsoftware.number.expression.expr.Root

  // phi, the Golden Ratio
  private val phi = Root.phi
  // psi, the conjugate of phi
  private val psi = Root.psi

  behavior of "worksheet"

  it should "work" in {

    phi.render shouldBe "\uD835\uDED7"

    (phi + One).materialize.render shouldBe "(1.5 + √1.25)"

    val phiSquared = (phi * phi).simplify

    phiSquared.render shouldBe "(\uD835\uDED7 + 1)"

    phiSquared.materialize.toString shouldBe "(1.5 + √1.25)"

    phi.approximation.get.toDouble should ===(1.618033988749895)

    psi.render shouldBe "\uD835\uDED9"

    psi.approximation.get.toDouble should ===(-0.618033988749895 +- 1E-10)

    (phi * psi).simplify shouldBe MinusOne

    phi.equation shouldBe QuadraticEquation(-1, -1)

    (phi * psi).simplify.render shouldBe "-1"

    (phi * phi).simplify.approximation.get.toDouble shouldBe 2.618033988749895 +- 1E-10

    QuadraticEquation.goldenRatioEquation.solve(0) match {
      case q: QuadraticSolution =>
        q.render shouldBe "\uD835\uDED7"
      case _ =>
        fail("not a QuadraticSolution")
    }
  }

  it should "evaluate phi + 1" in {
    val expression = phi + 1
    val value = expression.evaluateAsIs
    value shouldBe defined
  }
}
