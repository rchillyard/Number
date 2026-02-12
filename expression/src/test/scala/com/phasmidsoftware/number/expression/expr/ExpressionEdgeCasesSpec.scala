package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{Complex, Eager, QuadraticSolution}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar}
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for previously untested (TESTME) cases in CompositeExpression.
  * These tests cover edge cases and patterns that weren't exercised by existing tests.
  *
  * Note: Tests use symbolic constants (Pi, E, sqrt, phi) rather than simple integers
  * to avoid eager evaluation that would hide whether patterns are matching.
  */
class ExpressionEdgeCasesSpec extends AnyFlatSpec with Matchers {

  behavior of "BiFunction structural simplifications"

  it should "simplify power of power: (a^b)^p → a^(bp)" in {
    // Line 561: case BiFunction(BiFunction(a, b, Power), p, Power)
    val inner = BiFunction(Pi, Expression(3), Power) // π^3
    val outer = BiFunction(inner, Expression(4), Power) // (π^3)^4
    val result = outer.simplify

    // Should simplify to π^(3*4) = π^12
    result shouldBe BiFunction(Pi, Expression(12), Power)
  }

  it should "simplify (-a) * a → -(a²)" in {
    // Line 567: case BiFunction(UniFunction(a, Negate), b, Product) if a == b
    val x = Pi.sqrt // Use √π to avoid eager evaluation
    val negX = UniFunction(x, Negate) // -√π
    val product = BiFunction(negX, x, Product) // (-√π) * √π
    val actual = Expression.matchSimpler(product)
    val expected: Expression = -(x ∧ 2)
    actual shouldBe Expression.em.Match(expected)
  }

  it should "combine powers with same base: x^a * x^b → x^(a+b)" in {
    // Line 551: case BiFunction(BiFunction(w, x, Power), BiFunction(y, z, Power), Product)
    val base = Pi // Use E to avoid eager evaluation
    val pow1 = BiFunction(base, Expression(2), Power) // e^2
    val pow2 = BiFunction(base, Expression(3), Power) // e^3
    val product = BiFunction(pow1, pow2, Product) // e^2 * e^3
    val result = product.simplify

    // Should simplify to e^(2+3) = e^5
    result shouldBe BiFunction(Pi, Expression(5), Power)
  }

  behavior of "BiFunction identity simplifications - Quadratic roots"

  it should "simplify conjugate sum of quadratic roots" in {
    // Lines 503-508: Quadratic root conjugates with Sum
    val eq = QuadraticEquation(Rational(-1), Rational(-1)) // x² - x - 1 = 0 (golden ratio equation)
    val root1 = QuadraticRoot(eq, 0) // positive root (φ)
    val root2 = QuadraticRoot(eq, 1) // negative root (φ̄)
    val sum = BiFunction(root1, root2, Sum)
    val result = sum.simplify

    // Conjugate sum should simplify to -b/a = 1
    result.materialize shouldBe Eager.one
  }

  it should "simplify conjugate product of quadratic roots" in {
    // Lines 503-510: Quadratic root conjugates with Product
    val eq = QuadraticEquation(Rational(-1), Rational(-1)) // x² - x - 1 = 0
    val root1 = QuadraticRoot(eq, 0) // φ
    val root2 = QuadraticRoot(eq, 1) // φ̄
    val product = BiFunction(root1, root2, Product)
    val result = product.simplify

    // Conjugate product should simplify to c/a = -1
    result shouldBe MinusOne
  }

  it should "handle Root in BiFunction with Sum" in {
    // Line 515: case BiFunction(r: Root, x, f)
    val root = Expression(5).sqrt // √5
    val sum = BiFunction(root, Pi, Sum) // √5 + π
    val result = sum.simplify

    // Should create some kind of sum (exact behavior depends on matchRoot)
    // At minimum, should not crash
    noException should be thrownBy result
  }

  it should "handle Root in BiFunction with Product" in {
    // Line 515: case BiFunction(r: Root, x, f)
    val root = Expression(7).sqrt // √7
    val product = BiFunction(root, E, Product) // √7 * e
    val result = product.simplify

    // Should handle the product somehow
    noException should be thrownBy result
  }

  it should "handle commutative operations with Root on right side" in {
    // Lines 516-522: case BiFunction(x, r: Root, f) if f.commutes
    val root = Expression(2).sqrt // √2
    val sum = BiFunction(Pi, root, Sum) // π + √2
    val result = sum.simplify

    // Should handle commutatively
    noException should be thrownBy result
  }

  it should "handle Literal QuadraticSolution in BiFunction with Sum" in {
    // Lines 523-526: QuadraticSolution literals
    val phi = QuadraticSolution.phi
    val literal = Literal(phi)
    val sqrt2 = Expression(2).sqrt

    // Test with Sum
    val sum = BiFunction(literal, sqrt2, Sum) // φ + √2
    val result = sum.simplify

    noException should be thrownBy result
  }

  it should "handle Literal QuadraticSolution commutatively" in {
    // Lines 523-526: QuadraticSolution literals, commutative case
    val phi = QuadraticSolution.phi
    val literal = Literal(phi)
    val sqrt3 = Expression(3).sqrt

    // Test commutative case
    val sum = BiFunction(sqrt3, literal, Sum) // √3 + φ
    val result = sum.simplify

    noException should be thrownBy result
  }

  behavior of "BiFunction identity simplifications - Power operations"

  it should "handle phi literal to whole number power" in {
    // Line 812: case (r@Literal(QuadraticSolution.phi, _), ValueExpression(w: WholeNumber, _))
    val phi = Literal(QuadraticSolution.phi)
    val power = BiFunction(phi, Expression(3), Power) // φ^3
    val result = power.simplify

    // Should apply golden ratio identity
    // The exact result depends on the phi power reduction logic
    result should not be power
  }

  it should "handle Euler's identity with complex cartesian" in {
    // Line 839: case (E, Literal(ComplexCartesian(Number.zero, Number.pi), _))
    val complexNum = ComplexCartesian(numerical.Number.zero, numerical.Number.pi) // 0 + πi
    val power = BiFunction(E, Literal(Complex(complexNum)), Power) // e^(πi)
    val result = power.simplify

    // Euler's identity: e^(πi) = -1
    result shouldBe MinusOne
  }
  it should "handle Euler's identity with complex polar" in {
    // Line 839: case (E, Literal(ComplexCartesian(Number.zero, Number.pi), _))
    val complexNum: numerical.Complex = ComplexPolar(numerical.Number.pi, numerical.Number.piBy2) // 0 + πi
    val magnitude = complexNum.modulus
    println(s"complexNum = $complexNum; magnitude = $magnitude; argument = ${complexNum.argument}; isImaginary = ${complexNum.isImaginary}")
    val power = BiFunction(E, Literal(Complex(complexNum)), Power) // e^(πi)
    val result = power.simplify

    // Euler's identity: e^(πi) = -1
    result shouldBe MinusOne
  }

  it should "handle log inverse: base^(log_base(x)) → x" in {
    // Line 843: case (x, BiFunction(y, z, Log)) if x == z
    // Pattern checks if exponent is log_y(x) and base is x
    val base = E
    val x = Pi
    val log = BiFunction(x, base, Log) // log_e(π)
    val power = BiFunction(x, log, Power) // π^(log_e(π))
    val result = power.simplify

    // This should simplify based on the pattern
    // The exact behavior depends on implementation
    noException should be thrownBy result
  }

  behavior of "Edge cases and defensive patterns"

  it should "not match power combination when bases differ" in {
    // Line 553: The Miss case when bases are not equal
    val pow1 = BiFunction(E, Expression(2), Power) // e^2
    val pow2 = BiFunction(Pi, Expression(3), Power) // π^3
    val product = BiFunction(pow1, pow2, Product) // e^2 * π^3
    val result = product.simplify

    // Should not combine into a single power (bases differ)
    // Should remain as a product or become an aggregate
    result should not be BiFunction(E, Expression(5), Power)
  }

  it should "handle matchLiteral for algebraic quadratics" in {
    // Lines 698, 713: Various matchLiteral miss cases
    val literal = Literal(QuadraticSolution.phi)
    val sqrt2 = Expression(2).sqrt
    val sum = BiFunction(literal, sqrt2, Sum)

    // These are defensive - just verify they don't crash
    noException should be thrownBy sum.simplify
  }

  behavior of "UniFunction terms"

  it should "provide terms for UniFunction" in {
    // Line 247: UniFunction.terms
    val x = Expression(11).sqrt // Use sqrt to avoid simple evaluation
    val uniFunc = UniFunction(x, Negate) // -√11

    val terms = uniFunc.terms
    terms should have length 1
    terms.head shouldBe x
  }

  behavior of "Miss cases - phi identity edge cases"

  it should "handle phi identity when exponent is unity" in {
    // Line 821: Miss case when exponent is 1
    val phi = QuadraticRoot(QuadraticEquation(Rational(-1), Rational(-1)), 0)
    val power = BiFunction(phi, One, Power) // φ^1
    val result = power.simplify

    // Should just be phi (identity operation)
    result shouldBe phi
  }

  it should "handle phi identity when exponent is not suitable" in {
    // Line 824: Miss case when exponent doesn't evaluate properly
    val phi = QuadraticRoot(QuadraticEquation(Rational(-1), Rational(-1)), 0)
    val complexExp = UniFunction(E, Ln) // ln(e) - though this should simplify to 1
    // Use a more complex expression that won't easily evaluate
    val complexExp2 = BiFunction(Pi, E, Sum) // π + e
    val power = BiFunction(phi, complexExp2, Power) // φ^(π+e)

    // Should not crash, even if it can't simplify
    noException should be thrownBy power.simplify
  }

  behavior of "Structural - duplicate pattern check"

  it should "verify line 829 is duplicate of line 561 (power of power)" in {
    // Line 829 in identities: case (BiFunction(x, y, Power), z)
    // This should be the SAME as line 561 in structural
    val inner = BiFunction(Expression(2).sqrt, Expression(3), Power) // (√2)^3
    val outer = BiFunction(inner, Pi, Power) // ((√2)^3)^π
    val result = outer.simplify

    // Should combine exponents: (√2)^(3π)
    result should matchPattern { case BiFunction(_, BiFunction(_, _, Product), Power) => }
  }
}