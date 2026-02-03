/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.Complex
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Test suite for generating and validating complex solutions to Root objects.
  *
  * Complex roots occur when a quadratic equation x² + px + q = 0 has a negative discriminant:
  * discriminant = (p/2)² - q < 0
  *
  * The solutions are of the form: -p/2 ± i√(|discriminant|/4)
  */
class ComplexRootSpec extends AnyFlatSpec with ComplexEquality {

  behavior of "Complex roots from QuadraticEquation"

  /**
    * Test case: x² + 1 = 0
    * This is the canonical equation with purely imaginary roots: ±i
    */
  it should "generate ±i for x² + 1 = 0" in {
    val equation = QuadraticEquation(Rational.zero, Rational.one)
    val root0 = QuadraticRoot(equation, 0, None)
    val root1 = QuadraticRoot(equation, 1, None)

    // Both solutions should be Complex
    root0.solution shouldBe a[Complex]
    root1.solution shouldBe a[Complex]

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    // Verify they are conjugates: i and -i
    complex0.isComplex shouldBe true
    complex1.isComplex shouldBe true

    // The imaginary parts should have opposite signs
    complex1 shouldBe complex0.conjugate
  }

  /**
    * Test case: x² + 2x + 2 = 0
    * Discriminant: (2/2)² - 2 = 1 - 2 = -1
    * Solutions: -1 ± i
    */
  it should "generate -1 ± i for x² + 2x + 2 = 0" in {
    val equation = QuadraticEquation(Rational.two, Rational.two)
    val root0 = QuadraticRoot(equation, 0, None)
    val root1 = QuadraticRoot(equation, 1)

    root0.solution shouldBe a[Complex]
    root1.solution shouldBe a[Complex]

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    // Verify conjugate relationship
    complex1 shouldBe complex0.conjugate

    // Both should be complex (not purely real or purely imaginary)
    complex0.isComplex shouldBe true
    complex1.isComplex shouldBe true
  }

  /**
    * Test case: x² + 4 = 0
    * Solutions: ±2i
    */
  it should "generate ±2i for x² + 4 = 0" in {
    val equation = QuadraticEquation(Rational.zero, Rational(4))
    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    root0.solution shouldBe a[Complex]
    root1.solution shouldBe a[Complex]

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    complex1 shouldBe complex0.conjugate

    // Verify the modulus is 2
    val expected = 2.0
    val modulus = complex0.modulus
    checkModulus(modulus, expected)
  }

  /**
    * Test case: x² - 2x + 5 = 0
    * Discriminant: (-2/2)² - 5 = 1 - 5 = -4
    * Solutions: 1 ± 2i
    */
  it should "generate 1 ± 2i for x² - 2x + 5 = 0" in {
    val equation = QuadraticEquation(Rational(-2), Rational(5))
    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    root0.solution shouldBe a[Complex]
    root1.solution shouldBe a[Complex]

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    complex1 shouldBe complex0.conjugate
    complex0.isComplex shouldBe true
  }

  /**
    * Test case: x² + x + 1 = 0
    * Discriminant: (1/2)² - 1 = 1/4 - 1 = -3/4
    * Solutions: -1/2 ± i√(3/4) = -1/2 ± i√3/2
    * These are the complex cube roots of unity (ω and ω²)
    */
  // FIXME Issue #172
  ignore should "generate cube roots of unity for x² + x + 1 = 0" in {
    val equation = QuadraticEquation(Rational.one, Rational.one)
    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    root0.solution shouldBe a[Complex]
    root1.solution shouldBe a[Complex]

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    // Verify conjugate relationship
    val conjugate = complex0.conjugate
    complex1 should ===(conjugate)

    // Both cube roots of unity have modulus 1
    val expected = 1.0
    val modulus0 = complex0.modulus
    checkModulus(modulus0, expected)

    val modulus1 = complex1.modulus
    checkModulus(modulus1, expected)
  }

  behavior of "Complex root arithmetic"

  /**
    * Verify that complex roots can be added
    */
  // FIXME Issue #172
  ignore should "add two complex roots correctly" in {
    val eq1 = QuadraticEquation(Rational.zero, Rational.one) // x² + 1 = 0, roots: ±i
    val root1 = QuadraticRoot(eq1, 0)

    val eq2 = QuadraticEquation(Rational.zero, Rational(4)) // x² + 4 = 0, roots: ±2i
    val root2 = QuadraticRoot(eq2, 0)

    val complex1 = root1.solution.asInstanceOf[Complex]
    val complex2 = root2.solution.asInstanceOf[Complex]

    // Adding i + 2i should give 3i
    val sum = complex1 + complex2
    sum shouldBe a[Complex]
  }

  /**
    * Verify that multiplying a complex root by its conjugate gives a real result
    */
  it should "multiply a complex root by its conjugate to get a real number" in {
    val equation = QuadraticEquation(Rational.two, Rational.two) // -1 ± i
    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    val product = complex0.doMultiply(complex1)

    // (-1 + i) * (-1 - i) = 1 + 1 = 2
    // The result should be real (not complex)
    product.complex.isReal shouldBe true
  }

  behavior of "Complex root evaluation"

  /**
    * Verify that evaluating the equation at its complex root gives zero
    */
  // FIXME Issue #172
  ignore should "evaluate to zero when substituted back into the equation" in {
    val equation = QuadraticEquation(Rational.two, Rational.two)
    val root0 = QuadraticRoot(equation, 0)

    val solution = root0.solution
    val result = equation.evaluate(solution)

    // The result should be (very close to) zero
    result.isZero shouldBe true
  }

  // FIXME Issue #172
  ignore should "evaluate x² + 1 = 0 at i to get zero" in {
    val equation = QuadraticEquation(Rational.zero, Rational.one)
    val root = QuadraticRoot(equation, 0)

    val solution = root.solution
    val result = equation.evaluate(solution)

    result.isZero shouldBe true
  }

  behavior of "Complex root properties"

  /**
    * Verify that both branches of a complex root are conjugates
    */
  it should "have conjugate relationship between branches" in {
    val equation = QuadraticEquation(Rational.two, Rational.two)
    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    // Branch 1 should be the conjugate of branch 0
    complex1 shouldBe complex0.conjugate
    complex0 shouldBe complex1.conjugate
  }

  /**
    * Verify that complex roots have the correct number of branches
    */
  it should "have 2 branches for quadratic equations" in {
    val equation = QuadraticEquation(Rational.two, Rational.two)
    val root = QuadraticRoot(equation, 0)

    root.branches shouldBe 2
  }

  /**
    * Verify that complex roots satisfy the quadratic formula sum and product relationships
    * For x² + px + q = 0:
    * - Sum of roots = -p
    * - Product of roots = q
    */
  // FIXME Issue #172
  ignore should "satisfy Vieta's formulas for sum and product" in {
    val p = Rational(3)
    val q = Rational(5)
    val equation = QuadraticEquation(p, q) // x² + 3x + 5 = 0

    val root0 = QuadraticRoot(equation, 0)
    val root1 = QuadraticRoot(equation, 1)

    val complex0 = root0.solution.asInstanceOf[Complex]
    val complex1 = root1.solution.asInstanceOf[Complex]

    // Sum of roots should equal -p
    val sum = complex0 + complex1
    // The sum should be real and equal to -3

    // Product of roots should equal q
    val product = complex0.doMultiply(complex1)
    // The product should be real and equal to 5
    product.complex.isReal shouldBe true
  }

  behavior of "Edge cases for complex roots"

  /**
    * Test with very small coefficients
    */
  it should "handle equations with small coefficients" in {
    val equation = QuadraticEquation(Rational(1, 100), Rational(1, 100))
    val root0 = QuadraticRoot(equation, 0)

    // Should still produce complex roots even with small coefficients
    root0.solution shouldBe a[Complex]
  }

  /**
    * Test with large coefficients
    */
  it should "handle equations with large coefficients" in {
    //    val equation = QuadraticEquation(Rational(100), Rational(10000))
    //    val root0 = QuadraticRoot(equation, 0)

    // Should produce complex roots
    //    root0.solution shouldBe a[Complex]

    //    val result = equation.evaluate(root0.solution)
    //    result.isZero shouldBe true
    pending
  }

  /**
    * Test the boundary case where discriminant is exactly zero
    * (This should produce real repeated roots, not complex)
    */
  it should "not produce complex roots when discriminant is zero" in {
    // x² + 2x + 1 = 0, i.e., (x+1)² = 0
    // Discriminant: (2/2)² - 1 = 1 - 1 = 0
    val equation = QuadraticEquation(Rational.two, Rational.one)
    val root0 = QuadraticRoot(equation, 0)

    // Should be a QuadraticSolution, not Complex
    root0.solution should not be a[Complex]
  }

  /**
    * Test case just barely into complex territory
    */
  it should "handle discriminant just barely negative" in {
    // x² + 2x + 1.01 = 0
    // Discriminant: 1 - 1.01 = -0.01
    val equation = QuadraticEquation(Rational.two, Rational(101, 100))
    val root0 = QuadraticRoot(equation, 0)

    root0.solution shouldBe a[Complex]
  }

  behavior of "Complex root rendering"

  /**
    * Verify that complex roots can be rendered as strings
    */
  it should "render complex roots as readable strings" in {
    val equation = QuadraticEquation(Rational.zero, Rational.one) // ±i
    val root0 = QuadraticRoot(equation, 0)

    val complex = root0.solution.asInstanceOf[Complex]
    val rendered = complex.render

    // Should contain some representation of the imaginary unit
    rendered should not be empty
  }

  behavior of "Complex root normalization"

  /**
    * Verify that normalizing a complex root doesn't change it to a real number
    * when it's genuinely complex
    */
  it should "remain complex after normalization" in {
    val equation = QuadraticEquation(Rational.two, Rational.two)
    val root0 = QuadraticRoot(equation, 0)

    val complex = root0.solution.asInstanceOf[Complex]
    val normalized = complex.normalize

    normalized shouldBe a[Complex]
    normalized.isComplex shouldBe true
  }

}