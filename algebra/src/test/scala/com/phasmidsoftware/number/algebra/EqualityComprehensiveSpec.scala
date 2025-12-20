package com.phasmidsoftware.number.algebra

import com.phasmidsoftware.number.algebra.misc.FuzzyEq
import com.phasmidsoftware.number.core.inner.Rational
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EqualityComprehensiveSpec extends AnyFlatSpec with Matchers {

  // Scala 3 extension method for infix fuzzy equality on Eager
  extension (x: Eager)
    infix def ~==(y: Eager): Boolean =
      FuzzyEq[Eager].eqv(x, y, 0.5)

  behavior of "=== (Exact Equality)"

  // WholeNumber tests
  it should "compare WholeNumbers with ===" in {
    WholeNumber(5) should ===(WholeNumber(5))
    WholeNumber(5) should !==(WholeNumber(6))
  }

  // RationalNumber tests
  it should "compare RationalNumbers with ===" in {
    RationalNumber(1, 2) should ===(RationalNumber(1, 2))
    RationalNumber(1, 2) should ===(RationalNumber(2, 4)) // equivalent fractions
    RationalNumber(1, 2) should !==(RationalNumber(1, 3))
  }

  // Cross-type: WholeNumber and RationalNumber
  it should "compare WholeNumber with RationalNumber using ===" in {
    WholeNumber(2) should ===(RationalNumber(2, 1))
    WholeNumber(2) should ===(RationalNumber(4, 2))
    WholeNumber(2) should !==(RationalNumber(5, 2))
  }

  // Real tests (exact)
  it should "compare exact Reals with ===" in {
    Real(5.0, None) should ===(Real(5.0, None))
    Real(5.0, None) should !==(Real(5.1, None))
  }

  // Cross-type: WholeNumber and Real
  it should "compare WholeNumber with exact Real using ===" in {
    WholeNumber(5) should ===(Real(5.0, None))
    WholeNumber(5) should !==(Real(5.1, None))
  }

  // Cross-type: RationalNumber and Real
  it should "compare RationalNumber with exact Real using ===" in {
    RationalNumber(1, 2) should ===(Real(0.5, None))
    RationalNumber(1, 3) should !==(Real(0.5, None))
  }

  // Angle tests (ignoring degrees flag)
  it should "compare Angles with === (ignoring degrees flag)" in {
    val piRadians = Angle(RationalNumber.one)
    val pi180Degrees = Angle.degrees(180)

    piRadians should ===(pi180Degrees)
    Angle(RationalNumber.zero) should ===(Angle.zero)
    Angle.piBy2 should ===(Angle(RationalNumber(Rational.half)))
  }

  // Nat tests
  it should "compare Nats with ===" in {
    Nat(5) should ===(Nat(5))
    Nat(5) should !==(Nat(6))
    Nat(0) should ===(NatZero)
  }

  // Solution tests
  it should "compare LinearSolutions with ===" in {
    LinearSolution(WholeNumber(5)) should ===(LinearSolution(WholeNumber(5)))
    LinearSolution(WholeNumber(5)) should !==(LinearSolution(WholeNumber(6)))
  }

  it should "compare QuadraticSolutions with ===" in {
    val q1 = QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 0)
    val q2 = QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 0)
    val q3 = QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 1)

    q1 should ===(q2)
    q1 should !==(q3) // different branch
    QuadraticSolution.phi should ===(QuadraticSolution.phi)
  }

  // NatLog tests
  it should "compare NatLogs with ===" in {
    NatLog(WholeNumber.one) should ===(NatLog(WholeNumber.one))
    NatLog.e should ===(NatLog(WholeNumber.one))
    NatLog(WholeNumber.zero) should !==(NatLog(WholeNumber.one))
  }

  // InversePower tests
  it should "compare InversePowers with ===" in {
    InversePower(2, WholeNumber(4)) should ===(InversePower(2, WholeNumber(4)))
    InversePower(2, WholeNumber(4)) should !==(InversePower(2, WholeNumber(9)))
    InversePower(2, WholeNumber(4)) should !==(InversePower(3, WholeNumber(4))) // different power
  }

  behavior of "~== (Fuzzy Equality)"

  // Real fuzzy tests
  it should "compare fuzzy Reals with ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val fuzzy1 = Real(5.0, Some(Fuzziness.doublePrecision))
    val fuzzy2 = Real(5.0000000001, Some(Fuzziness.doublePrecision))
    val fuzzy3 = Real(6.0, Some(Fuzziness.doublePrecision))

    (fuzzy1 ~== fuzzy2) shouldBe true
    (fuzzy1 ~== fuzzy3) shouldBe false
  }

  it should "compare exact Real with fuzzy Real using ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val exact = Real(5.0, None)
    val fuzzy = Real(5.0000000001, Some(Fuzziness.doublePrecision))

    (exact ~== fuzzy) shouldBe true
  }

  // Angle fuzzy tests
  it should "compare Angles with ~==" in {
    val angle1 = Angle(Real(Math.PI / 2))
    val angle2 = Angle.piBy2

    // These should be approximately equal
    (angle1 ~== angle2) shouldBe true
  }

  it should "handle fuzzy equality for normalized Angles" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val angle1 = Angle(Real(0.0, Some(Fuzziness.doublePrecision)))
    val angle2 = Angle(Real(2 * Math.PI, Some(Fuzziness.doublePrecision)))

    // After normalization, these should be approximately equal
    (angle1.normalize ~== angle2.normalize) shouldBe true
  }

  // Exact types with ~== should just use ===
  it should "use exact equality for RationalNumber with ~==" in {
    (RationalNumber(1, 2) ~== RationalNumber(1, 2)) shouldBe true
    (RationalNumber(1, 2) ~== RationalNumber(1, 3)) shouldBe false
  }

  it should "use exact equality for WholeNumber with ~==" in {
    (WholeNumber(5) ~== WholeNumber(5)) shouldBe true
    (WholeNumber(5) ~== WholeNumber(6)) shouldBe false
  }

  it should "use exact equality for Nat with ~==" in {
    (Nat(5) ~== Nat(5)) shouldBe true
    (Nat(5) ~== Nat(6)) shouldBe false
  }

  // Cross-type fuzzy tests
  it should "compare WholeNumber with fuzzy Real using ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val whole = WholeNumber(5)
    val fuzzy = Real(5.0000000001, Some(Fuzziness.doublePrecision))

    (whole ~== fuzzy) shouldBe true
  }

  it should "compare RationalNumber with fuzzy Real using ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val rational = RationalNumber(1, 2)
    val fuzzy = Real(0.5000000001, Some(Fuzziness.doublePrecision))

    (rational ~== fuzzy) shouldBe true
  }

  it should "compare WholeNumber with RationalNumber using ~==" in {
    val whole = WholeNumber(5)
    val rational = RationalNumber(5, 1)

    (whole ~== rational) shouldBe true
  }

  it should "compare Angle with different representations using ~==" in {
    val piRadians = Angle(RationalNumber.one)
    val pi180Degrees = Angle.degrees(180)

    (piRadians ~== pi180Degrees) shouldBe true
  }

  // QuadraticSolution fuzzy tests
  it should "compare QuadraticSolutions with ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val q1 = QuadraticSolution(
      Real(0.5, Some(Fuzziness.doublePrecision)),
      Real(1.25, Some(Fuzziness.doublePrecision)),
      0
    )
    val q2 = QuadraticSolution(
      Real(0.5000000001, Some(Fuzziness.doublePrecision)),
      Real(1.25000000001, Some(Fuzziness.doublePrecision)),
      0
    )

    (q1 ~== q2) shouldBe true
  }

  // Edge cases
  behavior of "=== and ~== edge cases"

  it should "handle zero comparisons" in {
    WholeNumber.zero should ===(RationalNumber.zero)
    Real.zero should ===(WholeNumber.zero)
    Angle.zero should ===(Angle(RationalNumber.zero))

    // Fuzzy zero comparisons
    (WholeNumber.zero ~== RationalNumber.zero) shouldBe true
    (Real.zero ~== WholeNumber.zero) shouldBe true
  }

  it should "handle one/unity comparisons" in {
    WholeNumber.one should ===(RationalNumber.one)
    Real.one should ===(WholeNumber.one)

    // Fuzzy unity comparisons
    (WholeNumber.one ~== RationalNumber.one) shouldBe true
    (Real.one ~== WholeNumber.one) shouldBe true
  }

  it should "handle negative numbers" in {
    WholeNumber(-5) should ===(WholeNumber(-5))
    WholeNumber(-5) should ===(RationalNumber(-5, 1))
    Real(-5.0, None) should ===(WholeNumber(-5))

    // Fuzzy negative comparisons
    (WholeNumber(-5) ~== RationalNumber(-5, 1)) shouldBe true
    (Real(-5.0, None) ~== WholeNumber(-5)) shouldBe true
  }

  it should "not consider infinity equal to large numbers" in {
    Real.infinity should !==(Real(1e308, None))
    RationalNumber.infinity should !==(RationalNumber(Long.MaxValue, 1))

    // Fuzzy infinity comparisons
    (Real.infinity ~== Real(1e308, None)) shouldBe false
  }

  it should "handle NaN appropriately" in {
    // NaN should not equal itself
    val nan1 = Real(Double.NaN, None)
    val nan2 = Real(Double.NaN, None)

    nan1 should !==(nan2) // NaN != NaN by IEEE 754
    (nan1 ~== nan2) shouldBe false
  }

  // Symmetry tests
  behavior of "symmetry of === and ~=="

  it should "be symmetric for ===" in {
    val w = WholeNumber(5)
    val r = RationalNumber(5, 1)

    (w === r) shouldBe (r === w)
  }

  it should "be symmetric for ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val fuzzy1 = Real(5.0, Some(Fuzziness.doublePrecision))
    val fuzzy2 = Real(5.0000000001, Some(Fuzziness.doublePrecision))

    (fuzzy1 ~== fuzzy2) shouldBe (fuzzy2 ~== fuzzy1)
  }

  it should "be symmetric for cross-type comparisons with ~==" in {
    val whole = WholeNumber(5)
    val rational = RationalNumber(5, 1)

    (whole ~== rational) shouldBe (rational ~== whole)
  }

  // Transitivity tests
  behavior of "transitivity of === and ~=="

  it should "be transitive for ===" in {
    val w = WholeNumber(5)
    val r = RationalNumber(5, 1)
    val real = Real(5.0, None)

    if ((w === r) && (r === real)) {
      w should ===(real)
    }
  }

  it should "demonstrate that ~== respects transitivity for exact types" in {
    val w = WholeNumber(5)
    val r = RationalNumber(5, 1)
    val real = Real(5.0, None)

    if ((w ~== r) && (r ~== real)) {
      (w ~== real) shouldBe true
    }
  }

  // Note: Fuzzy equality is NOT necessarily transitive for fuzzy types!
  it should "note that ~== may not be transitive for fuzzy values" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val a = Real(1.0, Some(Fuzziness.doublePrecision))
    val b = Real(1.0 + 1e-15, Some(Fuzziness.doublePrecision))
    val c = Real(1.0 + 2e-15, Some(Fuzziness.doublePrecision))

    // a ~== b and b ~== c might be true, but a ~== c might be false
    // This is a known property of fuzzy equality and not a bug
    // The test just documents this behavior
    info("Fuzzy equality is not necessarily transitive")
  }

  // Reflexivity tests
  behavior of "reflexivity of === and ~=="

  it should "be reflexive for ===" in {
    val values: Seq[Eager] = Seq(
      WholeNumber(5),
      RationalNumber(1, 2),
      Real(5.0, None),
      Angle.piBy2,
      Nat(5)
    )

    values.foreach { v =>
      v should ===(v)
    }
  }

  it should "be reflexive for ~==" in {
    import com.phasmidsoftware.number.core.numerical.Fuzziness

    val values: Seq[Eager] = Seq(
      WholeNumber(5),
      RationalNumber(1, 2),
      Real(5.0, Some(Fuzziness.doublePrecision)),
      Angle.piBy2,
      Nat(5)
    )

    values.foreach { v =>
      (v ~== v) shouldBe true
    }
  }

  // Special mathematical values
  behavior of "special mathematical values"

  it should "handle pi correctly" in {
    Angle.pi should ===(Angle(RationalNumber.one))
    (Angle.pi ~== Angle.𝛑) shouldBe true
  }

  it should "handle e correctly" in {
    NatLog.e should ===(NatLog(WholeNumber.one))
    (NatLog.e ~== NatLog(WholeNumber.one)) shouldBe true
    (NatLog.e ~== Real(math.E)) shouldBe true
  }

  it should "handle golden ratio (phi) correctly" in {
    QuadraticSolution.phi should ===(QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 0))
    (QuadraticSolution.phi ~== QuadraticSolution.phi) shouldBe true
  }

  it should "handle square roots correctly" in {
    val root2 = InversePower(2, WholeNumber(2))
    val root4 = InversePower(2, WholeNumber(4))

    root2 should ===(root2)
    root2 should !==(root4)
  }

  // Practical usage patterns
  behavior of "practical usage patterns"

  it should "work with collections using ===" in {
    val numbers = Seq(WholeNumber(1), WholeNumber(2), WholeNumber(3))

    numbers should contain(WholeNumber(2))
    numbers.filter(_ === WholeNumber(2)) should have length 1
  }

  it should "work with Option using ===" in {
    val maybeNumber: Option[Eager] = Some(WholeNumber(5))

    maybeNumber.exists(_ === WholeNumber(5)) shouldBe true
    maybeNumber.exists(_ === WholeNumber(6)) shouldBe false
  }

  it should "work in pattern matching contexts" in {
    val number: Eager = WholeNumber(5)

    val result = number match {
      case n if n === WholeNumber(5) => "five"
      case n if n === WholeNumber(6) => "six"
      case _ => "other"
    }

    result shouldBe "five"
  }
}