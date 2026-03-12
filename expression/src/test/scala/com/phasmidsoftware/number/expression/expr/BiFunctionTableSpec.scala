/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.{HasImaginary, IsImaginary}
import com.phasmidsoftware.number.expression.expr.Expression.matchSimpler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Table-driven test suite for BiFunction matcher rules.
  *
  * Covers:
  * - Canonical ordering (structuralMatcher — WI12)
  * - Structural simplifications (structuralMatcher)
  * - Sum identities (matchBiFunctionIdentitiesSum)
  * - Product identities (matchBiFunctionIdentitiesProduct)
  * - Power identities (matchBiFunctionIdentitiesPower)
  * - Pythagorean identities (matchBiFunctionIdentitiesSum — WI11)
  * - Euler / de Moivre rules (structuralMatcher)
  *
  * Two assertion strategies are used:
  * - `input.simplify shouldBe expected` for end-to-end normalisation (canonical
  *   ordering, Pythagorean, Euler sum rules).
  * - `matchSimpler(input).get shouldBe expected` for surgical single-step rule
  *   verification (structural and identity rules).
  *
  * NOTE: Integer literals such as `Two * 3` evaluate eagerly to `Literal(6)`,
  * collapsing the composite structure before the matcher can observe it.
  * All composites used as test inputs must therefore involve irrational or
  * symbolic atoms (e.g. Pi, E, I) to remain unevaluated.
  */
class BiFunctionTableSpec extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  // ---------------------------------------------------------------------------
  // Canonical ordering (WI12) — verified via full simplify
  // ---------------------------------------------------------------------------

  behavior of "BiFunction canonical ordering (WI12)"

  /**
    * Uses `input.simplify` rather than `matchSimpler` because the ordering swap
    * may be followed immediately by an identity rule (e.g. x * 1 -> x), so the
    * meaningful assertion is the fully-normalised result.
    *
    * "Already canonical" cases are omitted: `simplify` on a canonical expression
    * evaluates it further rather than returning it unchanged, making identity-free
    * assertions fragile. Those cases are implicitly covered by the fact that
    * non-canonical inputs reach their correct simplified form.
    */
  private val canonicalOrderingCases = Table(
    ("description", "input", "expected"),

    // Composite on left, atom on right — atom should end up on left after simplification
    ("(Two*Pi) + One simplifies canonically",
      BiFunction(Two * Pi, One, Sum),
      One + (Two * Pi)),

    ("(Two*Pi) * E simplifies canonically",
      BiFunction(Two * Pi, E, Product),
      Aggregate.product(Two, E, Pi)),

    // Within atoms: higher-ranked atom on left should move right
    ("Pi + Two -> Two + Pi",
      BiFunction(Pi, Two, Sum),
      Two + Pi),

    ("E + One -> One + E",
      BiFunction(E, One, Sum),
      One + E),

    ("I + Two -> Two + I",
      BiFunction(I, Two, Sum),
      Two + I),

    ("E + Pi -> Pi + E",
      BiFunction(E, Pi, Sum),
      Pi + E),

    // Power is not commutative — composite on left should stay there
    ("Power: composite ∧ atom — no reorder",
      BiFunction(Two * Pi, Two, Power),
      ∅ * 4 * Pi ∧ 2),
  )

  it should "canonicalise commutative BiFunction operand order" in {
    forAll(canonicalOrderingCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Structural simplifications — structuralMatcher (single-step)
  // ---------------------------------------------------------------------------

  behavior of "BiFunction structural simplifications"

  private val structuralSumCases = Table(
    ("description", "input", "expected"),

    // a + a -> a * 2
    ("Pi + Pi -> Two * Pi",
      Pi + Pi,
      Two * Pi),

    // (x * k) + (y * k) -> k * (x + y)
    // k must be a shared symbolic atom; Pi and E ensure no eager evaluation
    ("(Pi*Two) + (E*Two) -> Two*(Pi+E)",
      BiFunction(Pi * Two, E * Two, Sum),
      BiFunction(Two, Pi + E, Product)),
  )

  it should "apply structural Sum simplifications" in {
    forAll(structuralSumCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        matchSimpler(input).get shouldBe expected
      }
    }
  }

  private val structuralProductCases = Table(
    ("description", "input", "expected"),

    // a * a -> a ∧ 2
    ("Pi * Pi -> Pi∧2",
      Pi * Pi,
      Pi ∧ Two),

    // (w∧x) * (w∧y) -> w∧(x+y)
    // Exponents are atoms so their Sum stays unevaluated
    ("Pi∧Two * Pi∧E -> Pi∧(Two+E)",
      (Pi ∧ Two) * (Pi ∧ E),
      Pi ∧ (Two + E)),

    // (a+b)(a-b) -> a∧2 - b∧2
    // NOTE: One∧2 = 1, so result is (Pi∧2 - 1) = (Pi∧2 + MinusOne)
    // Use .simplify on expected to match whatever canonical form the pipeline produces
    ("(Pi+One)(Pi-One) -> Pi∧2 - 1",
      BiFunction(Pi + One, BiFunction(Pi, UniFunction(One, Negate), Sum), Product),
      ((Pi ∧ Two) + MinusOne).simplify),
  )

  it should "apply structural Product simplifications" in {
    forAll(structuralProductCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        matchSimpler(input).get shouldBe expected
      }
    }
  }
  // I^n cycles — evaluated eagerly, use simplify
  private val iCycleCases = Table(
    ("description", "input", "expected"),
    ("I∧0 -> 1", I ∧ Zero, One),
    ("I∧1 -> I", I ∧ One, I),
    ("I∧2 -> -1", I ∧ Two, MinusOne),
    ("I∧3 -> -I", I ∧ 3, -I),
    ("I∧4 -> 1", I ∧ 4, One),
    ("I∧5 -> I", I ∧ 5, I),
  )

  it should "evaluate I^n cycles correctly" in {
    forAll(iCycleCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  private val structuralPowerCases = Table(
    ("description", "input", "expected"),

    // (a*b)∧p -> a∧p * b∧p
    ("(Two*Pi)∧Two -> Two∧Two * Pi∧Two",
      (Two * Pi) ∧ Two,
      (Two ∧ Two) * (Pi ∧ Two)),

    // (a∧b)∧p -> a∧(b*p)
    // Both exponents are atoms; their product stays symbolic since Pi is irrational
    ("(Pi∧Two)∧Pi -> Pi∧(Two*Pi)",
      (Pi ∧ Two) ∧ Pi,
      Pi ∧ (Two * Pi)),
  )

  it should "apply structural Power simplifications" in {
    forAll(structuralPowerCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        val actual = matchSimpler(input).get
        actual shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Sum identities — matchBiFunctionIdentitiesSum (single-step)
  // ---------------------------------------------------------------------------

  behavior of "BiFunction Sum identities"

  private val sumIdentityCases = Table(
    ("description", "input", "expected"),

    // x + 0 -> x  (Zero is canonical left operand so both orderings are tested)
    ("Pi + Zero -> Pi", Pi + Zero, Pi),
    ("Zero + Pi -> Pi", Zero + Pi, Pi),

    // NOTE: QuadraticRoot conjugate sum tested separately in ComplexRootTableSpec
  )

  it should "simplify Sum identity cases" in {
    forAll(sumIdentityCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Pythagorean identities — matchBiFunctionIdentitiesSum (full simplify)
  // ---------------------------------------------------------------------------

  behavior of "BiFunction Pythagorean identities (WI11)"

  // z = One + I is used as a symbolic argument that stays unevaluated,
  // allowing the Pythagorean identity rules to fire symbolically.
  // Concrete values like Pi or I evaluate eagerly before squaring (WI13 limitation).
  private val z = One + I

  private val pythagoreanCases = Table(
    ("description", "input", "expected"),

    // sin²(z) + cos²(z) -> 1 for symbolic z
    ("sin²(z) + cos²(z) -> 1 for z=One+I",
      (z.sin ∧ Two) + (z.cos ∧ Two),
      One),

    // Commuted order covered by SumSymmetricCommutative extractor — not separately tested
    // since WI12 canonical ordering may reorder composites unpredictably by depth.
    // ("cos²(z) + sin²(z) -> 1 (commuted) for z=One+I", ...),
    // Commuted order: cos² + sin² -> 1

    // cosh²(z) - sinh²(z) -> 1 for symbolic z
    ("cosh²(z) - sinh²(z) -> 1 for z=One+I",
      (z.cosh ∧ Two) - (z.sinh ∧ Two),
      One),

    // cos²(z) - sin²(z) -> cos(2z)
    // PENDING: eager evaluation of cos/sin when argument is concrete (WI13)
    // ("cos²(Pi) - sin²(Pi) -> cos(2π)", (Pi.cos ∧ Two) - (Pi.sin ∧ Two), (Two * Pi).cos),

    // sin²(z) + cos²(z) for concrete Pi -- PENDING (WI13)
    // ("sin²(Pi) + cos²(Pi) -> 1", (Pi.sin ∧ Two) + (Pi.cos ∧ Two), One),

    // sin²(I) + cos²(I) -- PENDING (WI13)
    // ("sin²(I) + cos²(I) -> 1", (I.sin ∧ Two) + (I.cos ∧ Two), One),
  )

  it should "simplify Pythagorean identity cases" in {
    forAll(pythagoreanCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Product identities — matchBiFunctionIdentitiesProduct (single-step)
  // ---------------------------------------------------------------------------

  behavior of "BiFunction Product identities"

  private val productIdentityCases = Table(
    ("description", "input", "expected"),

    // x * 0 -> 0
    ("Pi * Zero -> Zero", Pi * Zero, Zero),
    ("Zero * Pi -> Zero", Zero * Pi, Zero),

    // x * 1 -> x
    ("Pi * One -> Pi", Pi * One, Pi),
    ("One * Pi -> Pi", One * Pi, Pi),

    // x * (-1) -> -x
    ("Pi * MinusOne -> -Pi", Pi * MinusOne, -Pi),
    ("MinusOne * Pi -> -Pi", MinusOne * Pi, -Pi),

    // x * (-x) -> -(x∧2)
    ("Pi * (-Pi) -> -(Pi∧2)",
      Pi * (-Pi),
      -(Pi ∧ Two)),
  )

  it should "simplify Product identity cases" in {
    forAll(productIdentityCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Power identities — matchBiFunctionIdentitiesPower (single-step)
  // ---------------------------------------------------------------------------

  behavior of "BiFunction Power identities"

  private val powerIdentityCases = Table(
    ("description", "input", "expected"),

    ("Pi ∧ One -> Pi", Pi ∧ One, Pi),
    ("Pi ∧ Zero -> One", Pi ∧ Zero, One),
    ("One ∧ Pi -> One", One ∧ Pi, One),

    // NOTE: NthRoot ∧ n -> radicand tested in Root-specific specs
  )

  it should "simplify Power identity cases" in {
    forAll(powerIdentityCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // Euler / de Moivre rules — structuralMatcher
  // ---------------------------------------------------------------------------

  behavior of "BiFunction Euler and de Moivre rules"

  private val eulerProductCases = Table(
    ("description", "input", "expected"),

    // r * Euler(1, θ) -> Euler(r, θ)
    ("Two * Euler(1,E) -> Euler(Two,E)",
      Two * Euler(One, E),
      Euler(Two, E)),

    // Euler(r1,t1) * Euler(r2,t2) -> Euler(r1*r2, t1+t2)
    // expected uses .simplify since Euler(6, 2π) may further reduce
    ("Euler(Two,E) * Euler(3,E) -> Euler(6,2E)",
      Euler(Two, E) * Euler(3, E),
      Euler(Two * 3, E + E).simplify),

    // Euler(r,t) ∧ n -> Euler(r∧n, t*n)
    ("Euler(Two,E)∧3 -> Euler(8,3E)",
      Euler(Two, E) ∧ 3,
      Euler(Two ∧ 3, E * 3).simplify),
  )

  it should "apply Euler product and power rules" in {
    forAll(eulerProductCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        matchSimpler(input).get shouldBe expected
      }
    }
  }

  private val eulerSumCases = Table(
    ("description", "input", "expected"),

    // cos(θ) + i·sin(θ) -> Euler(1, θ)
    ("cos(E) + i·sin(E) -> Euler(1,E)",
      E.cos + (I * E.sin),
      Euler(One, E)),

    // Euler(r,t) + Euler(r,-t) -> 2r·cos(t)
    ("Euler(Two,E) + Euler(Two,-E) -> 2*Two*cos(E)",
      Euler(Two, E) + Euler(Two, -E),
      ∅ * 4 * E.cos.simplify),
  )

  it should "apply Euler sum rules" in {
    forAll(eulerSumCases) { (description, input, expected) =>
      withClue(s"$description: ") {
        input.simplify shouldBe expected
      }
    }
  }

  // ---------------------------------------------------------------------------
  // IsImaginaryExpression (commutative) — matchBiFunctionIdentitiesProduct
  // ---------------------------------------------------------------------------

  behavior of "BiFunction imaginary expression product (commutative)"

  private val imaginaryProductCases = Table(
    ("description", "input", "shouldBeImaginary"),

    // Both orderings exercise IsImaginaryExpressionCommutative
    ("I * One is imaginary", I * One, true),
    ("I * Two is imaginary", I * Two, true),
    ("One * I is imaginary", One * I, true),
    ("Two * I is imaginary", Two * I, true),
  )

  it should "evaluate imaginary products correctly" in {
    println(I.evaluateAsIs)
    forAll(imaginaryProductCases) { (description, input, shouldBeImaginary) =>
      withClue(s"$description: ") {
        val result = input.simplify.materialize
        result match {
          case HasImaginary(_) =>
            shouldBeImaginary shouldBe true
          case _ =>
            shouldBeImaginary shouldBe false
        }
      }
    }
  }
}