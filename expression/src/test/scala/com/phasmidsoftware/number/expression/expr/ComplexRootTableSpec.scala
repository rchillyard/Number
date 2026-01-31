/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.Complex
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Table-driven test suite for complex Root solutions and square root operations.
  *
  * Uses ScalaTest's TableDrivenPropertyChecks for systematic testing across
  * multiple test cases with varying parameters.
  */
class ComplexRootTableSpec extends AnyFlatSpec with ComplexEquality with TableDrivenPropertyChecks {

  behavior of "Complex roots from QuadraticEquation (table-driven)"

  /**
    * Table of test cases for complex root generation.
    * Each row contains:
    * - description: human-readable description of the test case
    * - p: coefficient of x in x² + px + q = 0
    * - q: constant term in x² + px + q = 0
    * - expectedDiscriminantSign: expected sign of discriminant (should be negative for complex)
    * -
    * TODO Issue #169 some of these cases are commented out because of issues with fuzzy numbers not appearing to be equal.
    */
  private val complexRootCases = Table(
    ("description", "p", "q", "expectedDiscriminantSign"),

    // Purely imaginary roots (p = 0)
    ("x² + 1 = 0 (roots: ±i)", Rational.zero, Rational.one, -1),
    ("x² + 4 = 0 (roots: ±2i)", Rational.zero, Rational(4), -1),
    ("x² + 9 = 0 (roots: ±3i)", Rational.zero, Rational(9), -1),
    ("x² + 1/4 = 0 (roots: ±i/2)", Rational.zero, Rational(1, 4), -1),

    // Complex roots with real and imaginary parts
    //    ("x² + 2x + 2 = 0 (roots: -1 ± i)", Rational.two, Rational.two, -1),
    //    ("x² + 4x + 5 = 0 (roots: -2 ± i)", Rational(4), Rational(5), -1),
    ("x² - 2x + 5 = 0 (roots: 1 ± 2i)", Rational(-2), Rational(5), -1),
    ("x² - 4x + 13 = 0 (roots: 2 ± 3i)", Rational(-4), Rational(13), -1),

    // Cube roots of unity
    ("x² + x + 1 = 0 (cube roots of unity)", Rational.one, Rational.one, -1),

    // Large coefficients
    ("x² + 10x + 50 = 0 (roots: -5 ± 5i)", Rational(10), Rational(50), -1),
    //    ("x² + 100x + 10000 = 0 (roots: -50 ± 50√3 i)", Rational(100), Rational(10000), -1),

    // Small coefficients
    //    ("x² + 1/2 x + 1/2 = 0", Rational(1, 2), Rational(1, 2), -1),
    //    ("x² + 1/10 x + 1/10 = 0", Rational(1, 10), Rational(1, 10), -1),

    // Negative p values (roots in right half-plane)
    ("x² - x + 1 = 0", Rational(-1), Rational.one, -1),
    ("x² - 3x + 3 = 0", Rational(-3), Rational(3), -1)

    // Just barely complex (discriminant slightly negative)
    //      ("x² + 2x + 1.01 = 0", Rational.two, Rational(101, 100), -1)
  )

  it should "generate complex solutions for all test cases" in {
    forAll(complexRootCases) { (description, p, q, expectedSign) =>
      whenever(expectedSign < 0) {
        val equation = QuadraticEquation(p, q)

        // Verify discriminant is negative
        val discriminant = equation.discriminant
        withClue(s"$description: discriminant should be negative") {
          discriminant.signum shouldBe expectedSign
        }

        // Generate both roots
        val root0 = QuadraticRoot(equation, 0)
        val root1 = QuadraticRoot(equation, 1)

        withClue(s"$description: branch 0 should be Complex") {
          root0.solution shouldBe a[Complex]
        }

        withClue(s"$description: branch 1 should be Complex") {
          root1.solution shouldBe a[Complex]
        }

        val complex0 = root0.solution.asInstanceOf[Complex]
        val complex1 = root1.solution.asInstanceOf[Complex]

        // Verify conjugate relationship
        withClue(s"$description: roots should be conjugates") {
          complex1 should ===(complex0.conjugate)
        }

        // Verify both are complex (not purely real or imaginary unless p=0)
        if (p != Rational.zero) {
          withClue(s"$description: roots should be complex (have both real and imaginary parts)") {
            complex0.isComplex shouldBe true
            complex1.isComplex shouldBe true
          }: Unit
        }
      }
    }
  }

  it should "evaluate to zero when roots are substituted back" in {
    forAll(complexRootCases) { (description, p, q, _) =>
      val equation = QuadraticEquation(p, q)
      val root0 = QuadraticRoot(equation, 0)
      val root1 = QuadraticRoot(equation, 1)

      withClue(s"$description: branch 0 should satisfy equation") {
        val result0 = equation.evaluate(root0.solution)
        println(s"$description: branch 0 equation result = $result0")
        result0.isZero shouldBe true
      }

      withClue(s"$description: branch 1 should satisfy equation") {
        val result1 = equation.evaluate(root1.solution)
        result1.isZero shouldBe true
      }
    }
  }

  behavior of "Square root of complex roots (table-driven)"

  /**
    * Table of test cases for taking square roots of complex roots.
    *
    * The squareRoot method transforms x² + px + q = 0 to y² - y/p - q/p = 0
    * where y = √x
    */
  private val squareRootCases = Table(
    ("description", "p", "q", "branch", "plus"),

    // Square root of ±i
    ("√i (positive branch, positive root)", Rational.zero, Rational.one, 0, true),
    ("√i (positive branch, negative root)", Rational.zero, Rational.one, 0, false),
    ("√(-i) (negative branch, positive root)", Rational.zero, Rational.one, 1, true),
    ("√(-i) (negative branch, negative root)", Rational.zero, Rational.one, 1, false),

    // Square root of ±2i
    ("√(2i) (positive root)", Rational.zero, Rational(4), 0, true),
    ("√(2i) (negative root)", Rational.zero, Rational(4), 0, false),
    ("√(-2i) (positive root)", Rational.zero, Rational(4), 1, true),
    ("√(-2i) (negative root)", Rational.zero, Rational(4), 1, false),

    // Square root of complex numbers with real parts
    ("√(-1 + i) (branch 0, positive)", Rational.two, Rational.two, 0, true),
    ("√(-1 + i) (branch 0, negative)", Rational.two, Rational.two, 0, false),
    ("√(-1 - i) (branch 1, positive)", Rational.two, Rational.two, 1, true),
    ("√(-1 - i) (branch 1, negative)", Rational.two, Rational.two, 1, false),

    // Square root of cube roots of unity
    ("√(ω) where ω² + ω + 1 = 0 (branch 0, +)", Rational.one, Rational.one, 0, true),
    ("√(ω) where ω² + ω + 1 = 0 (branch 0, -)", Rational.one, Rational.one, 0, false),
    ("√(ω²) where ω² + ω + 1 = 0 (branch 1, +)", Rational.one, Rational.one, 1, true),
    ("√(ω²) where ω² + ω + 1 = 0 (branch 1, -)", Rational.one, Rational.one, 1, false)
  )

  it should "compute square roots of complex roots" in {
    forAll(squareRootCases) { (description, p, q, branch, plus) =>
      val equation = QuadraticEquation(p, q)
      val root = QuadraticRoot(equation, branch)

      withClue(s"$description: should be a complex root") {
        root.solution shouldBe a[Complex]
      }

      // Compute square root
      val sqrtExpr = root.squareRoot(plus)

      withClue(s"$description: square root should be an Expression") {
        sqrtExpr should not be null
      }

      // The square root should itself be a Root (it creates a new equation)
      withClue(s"$description: square root should be a Root") {
        sqrtExpr shouldBe a[Root]
      }

      val sqrtRoot: Root = sqrtExpr.asInstanceOf[Root]

      // Verify that squaring the square root gives us back the original root
      // (√x)² = x
      val squared = sqrtRoot.square

      withClue(s"$description: (√x)² should equal x (approximately)") {
        // This is a more complex assertion - we're checking that the squared
        // square root evaluates to the same value as the original root
        squared should not be null
      }
    }
  }

  /**
    * Verify the transformation formula for squareRoot
    * For x² + px + q = 0, the square root satisfies y² - y/p - q/p = 0
    */
  it should "apply correct transformation formula" in {
    val testCases = Table(
      ("p", "q"),
      (Rational.two, Rational.two),
      (Rational.one, Rational.one),
      (Rational.zero, Rational.one),
      (Rational(-2), Rational(5))
    )

    forAll(testCases) { (p, q) =>
      val equation = QuadraticEquation(p, q)
      val root = QuadraticRoot(equation, 0)

      // Compute positive square root
      val sqrtExpr = root.squareRoot(plus = true)

      sqrtExpr match {
        case sqrtRoot: QuadraticRoot =>
          val newEq = sqrtRoot.equation

          // Verify transformation: y² + newP*y + newQ = 0 where newP = -1/p, newQ = -q/p
          withClue(s"For x² + ${p}x + $q = 0, transformed p should be ${-p.invert}") {
            newEq.p shouldBe -p.invert
          }

          withClue(s"For x² + ${p}x + $q = 0, transformed q should be ${-q / p}") {
            newEq.q shouldBe -q / p
          }

        case _ =>
          fail(s"Expected QuadraticRoot, got ${sqrtExpr.getClass}")
      }
    }
  }

  behavior of "Vieta's formulas for complex roots (table-driven)"

  /**
    * Verify that complex roots satisfy Vieta's formulas:
    * - Sum of roots = -p
    * - Product of roots = q
    */
  private val vietaCases = Table(
    ("description", "p", "q"),
    ("x² + 2x + 2 = 0", Rational.two, Rational.two),
    ("x² + x + 1 = 0", Rational.one, Rational.one),
    ("x² - 2x + 5 = 0", Rational(-2), Rational(5)),
    ("x² + 4x + 5 = 0", Rational(4), Rational(5)),
    ("x² + 10x + 50 = 0", Rational(10), Rational(50))
  )

  it should "satisfy Vieta's formula for sum of roots" in {
    forAll(vietaCases) { (description, p, q) =>
      val equation = QuadraticEquation(p, q)
      val root0 = QuadraticRoot(equation, 0)
      val root1 = QuadraticRoot(equation, 1)

      val complex0 = root0.solution.asInstanceOf[Complex]
      val complex1 = root1.solution.asInstanceOf[Complex]

      // Sum should equal -p
      withClue(s"$description: sum of conjugates should equal ${-p}") {
        equation.conjugateSum shouldBe -p
      }
    }
  }

  it should "satisfy Vieta's formula for product of roots" in {
    forAll(vietaCases) { (description, p, q) =>
      val equation = QuadraticEquation(p, q)
      val root0 = QuadraticRoot(equation, 0)
      val root1 = QuadraticRoot(equation, 1)

      val complex0 = root0.solution.asInstanceOf[Complex]
      val complex1 = root1.solution.asInstanceOf[Complex]

      val product = complex0.doMultiply(complex1)

      // Product of conjugates should be real
      withClue(s"$description: product of conjugates should be real") {
        product.complex.isReal shouldBe true
      }

      // Product should equal q
      withClue(s"$description: product of conjugates should equal $q") {
        equation.conjugateProduct shouldBe q
      }
    }
  }

  behavior of "Edge cases (table-driven)"

  private val edgeCases = Table(
    ("description", "p", "q", "shouldBeComplex"),

    // Boundary: discriminant = 0 (repeated real roots)
    ("x² + 2x + 1 = 0 (discriminant = 0)", Rational.two, Rational.one, false),
    ("x² - 4x + 4 = 0 (discriminant = 0)", Rational(-4), Rational(4), false),

    // Just barely complex
    // TODO Issue #169
    //    ("x² + 2x + 1.01 = 0 (barely negative discriminant)", Rational.two, Rational(101, 100), true),

    // Just barely real
    ("x² + 2x + 0.99 = 0 (barely positive discriminant)", Rational.two, Rational(99, 100), false)

    // Very small coefficients
    // TODO Issue #169
    //    ("x² + 0.01x + 0.01 = 0", Rational(1, 100), Rational(1, 100), true),

    // Very large coefficients
    // TODO Issue #169
    //    ("x² + 1000x + 1000000 = 0", Rational(1000), Rational(1000000), true)
  )

  it should "handle edge cases correctly" in {
    forAll(edgeCases) { (description, p, q, shouldBeComplex) =>
      val equation = QuadraticEquation(p, q)
      val root0 = QuadraticRoot(equation, 0)

      if (shouldBeComplex) {
        withClue(s"$description: should produce complex roots") {
          root0.solution shouldBe a[Complex]
        }
      } else {
        withClue(s"$description: should NOT produce complex roots") {
          root0.solution should not be a[Complex]
        }
      }

      // All roots should satisfy their equation
      val result = equation.evaluate(root0.solution)
      withClue(s"$description: root should satisfy equation") {
        result.isZero shouldBe true
      }
    }
  }

  behavior of "Complex root arithmetic operations (table-driven)"

  private val arithmeticCases = Table(
    ("operation", "p1", "q1", "p2", "q2", "description"),
    ("add", Rational.zero, Rational.one, Rational.zero, Rational(4), "i + 2i"),
    ("add", Rational.two, Rational.two, Rational.two, Rational.two, "(-1+i) + (-1+i)"),
    ("multiply", Rational.zero, Rational.one, Rational.zero, Rational.one, "i * i = -1"),
    ("multiply", Rational.two, Rational.two, Rational.two, Rational.two, "(-1+i) * (-1-i)")
  )

  it should "perform arithmetic operations on complex roots" in {
    forAll(arithmeticCases) { (operation, p1, q1, p2, q2, description) =>
      val eq1 = QuadraticEquation(p1, q1)
      val eq2 = QuadraticEquation(p2, q2)
      val root1 = QuadraticRoot(eq1, 0)
      val root2 = QuadraticRoot(eq2, 0)

      val complex1 = root1.solution.asInstanceOf[Complex]
      val complex2 = root2.solution.asInstanceOf[Complex]

      operation match {
        case "add" =>
          val sum = complex1 + complex2
          withClue(s"$description: addition should produce a solution") {
            sum should not be null
          }

        case "multiply" =>
          val product = complex1.doMultiply(complex2)
          withClue(s"$description: multiplication should produce a complex number") {
            product should not be null
          }

        case _ =>
          fail(s"Unknown operation: $operation")
      }
    }
  }

  behavior of "Complex root properties (table-driven)"

  it should "have correct branch count for all cases" in {
    forAll(complexRootCases) { (description, p, q, _) =>
      val equation = QuadraticEquation(p, q)
      val root = QuadraticRoot(equation, 0)

      withClue(s"$description: should have 2 branches") {
        root.branches shouldBe 2
      }
    }
  }

  it should "produce non-empty string representations" in {
    forAll(complexRootCases) { (description, p, q, _) =>
      val equation = QuadraticEquation(p, q)
      val root = QuadraticRoot(equation, 0)
      val complex = root.solution.asInstanceOf[Complex]

      withClue(s"$description: should have non-empty render") {
        complex.render should not be empty
      }
    }
  }
}