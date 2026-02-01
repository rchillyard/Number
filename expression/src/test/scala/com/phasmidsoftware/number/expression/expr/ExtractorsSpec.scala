package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Eager, QuadraticSolution, Solution}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExtractorsSpec extends AnyFlatSpec with Matchers {

  behavior of "IsIntegral"

  it should "match WholeNumber literals" in {
    val expr = Literal((42))
    expr match {
      case IsIntegral(n) => n shouldBe 42
      case _ => fail("Should match integer")
    }
  }

  it should "match whole Rational literals" in {
    val expr = Literal(Rational(6, 2)) // 3
    expr match {
      case IsIntegral(n) => n shouldBe 3
      case _ => fail("Should match integer")
    }
  }

  it should "not match non-whole Rational" in {
    val expr = Literal(Rational(5, 2)) // 2.5
    expr match {
      case IsIntegral(_) => fail("Should not match non-integer")
      case _ => succeed
    }
  }

  it should "not match Real numbers" in {
    val expr = Literal(3.0)
    expr match {
      case IsIntegral(_) => fail("Should not match Real")
      case _ => succeed
    }
  }

  it should "not match non-Valuable expressions" in {
    val expr = BiFunction(Literal(1), Literal(2), Sum)
    expr match {
      case IsIntegral(_) => fail("Should not match BiFunction")
      case _ => succeed
    }
  }

  behavior of "QuadraticValue"

  it should "match QuadraticRoot with equation and branch" in {
    val eq = QuadraticEquation(1, -1) // x² + x - 1 = 0
    val expr = QuadraticRoot(eq, 0)
    expr match {
      case QuadraticValue(equation, branch) =>
        equation shouldBe eq
        branch shouldBe 0
      case _ => fail("Should match QuadraticRoot")
    }
  }

  it should "match Literal QuadraticSolution" in {
    val qs = QuadraticSolution.phi
    val expr = Literal(qs)
    expr match {
      case QuadraticValue(equation, branch) =>
        equation shouldBe QuadraticEquation(-1, -1)
        branch shouldBe 0 // φ is the positive root
      case _ => fail("Should match Literal QuadraticSolution")
    }
  }

  it should "distinguish between φ and ψ" in {
    val phi = QuadraticRoot(QuadraticEquation(1, -1), 0)
    val psi = QuadraticRoot(QuadraticEquation(1, -1), 1)

    phi match {
      case QuadraticValue(_, 0) => succeed
      case _ => fail("φ should be branch 0")
    }

    psi match {
      case QuadraticValue(_, 1) => succeed
      case _ => fail("ψ should be branch 1")
    }
  }

  behavior of "NthRoot"

  it should "match square root from Root" in {
    val expr = Root.√(3)
    expr match {
      case NthRoot(radicand, n, branch) =>
        radicand.normalize shouldBe Eager(3)
        n shouldBe 2
        branch shouldBe 0
      case _ => fail("Should match square root")
    }
  }

  it should "match square root from Literal QuadraticSolution" in {
    // Assuming √3 can be represented as a QuadraticSolution
    val qs: Solution = QuadraticEquation(0, -3).solve(0) // x² - 3 = 0
    val expr = Literal(qs)

    qs match {
      case QuadraticSolution(base, offset, coefficient, imaginary) =>
        expr match {
          case NthRoot(radicand, 2, branch) =>
            // Test passes if it matches
            succeed
          case _ =>
            fail("Should match square root from QuadraticSolution")
        }
    }
  }

  behavior of "SquareRoot"

  it should "match square roots only" in {
    val sqrt3 = Root.√(3)
    sqrt3 match {
      case SquareRoot(radicand, 0) =>
        radicand.normalize shouldBe Eager(3)
      case _ => fail("Should match square root")
    }
  }

  behavior of "CubeRoot"

  it should "not match square roots" in {
    val sqrt3 = Root.√(3)
    sqrt3 match {
      case CubeRoot(_) => fail("Should not match square root")
      case _ => succeed
    }
  }

  behavior of "Extractor combinations"

  it should "match pattern with multiple extractors" in {
    val sqrt3a = Root.√(3)
    val qs = QuadraticRoot(QuadraticEquation(0, -3), 0)
    val sqrt3b = qs

    (sqrt3a, sqrt3b) match {
      case (SquareRoot(a), SquareRoot(b)) if a == b =>
        succeed // Both represent √3
      case _ => fail("Should match both forms of √3")
    }
  }

  it should "match golden ratio in different forms" in {
    val phiRoot = Root.phi
    val phiLiteral = Literal(QuadraticSolution.phi)

    (phiRoot, phiLiteral) match {
      case (QuadraticValue(eq1, 0), QuadraticValue(eq2, 0)) if eq1 == eq2 =>
        succeed
      case _ => fail("Should match both forms of φ")
    }
  }
}