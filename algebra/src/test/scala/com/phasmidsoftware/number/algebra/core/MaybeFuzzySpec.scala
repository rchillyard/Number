package com.phasmidsoftware.number.algebra.core

import com.phasmidsoftware.number.algebra.eager.{Angle, InversePower, NaturalExponential, Real}
import com.phasmidsoftware.number.core.numerical.{AbsoluteFuzz, Box, Gaussian, RelativeFuzz}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * The `MaybeFuzzySpec` class contains a suite of unit tests for validating the behavior
  * of extension methods applied to mathematical concepts with potential fuzziness, such as `Real`, `Angle`,
  * `InversePower`, and `NaturalExponential`. These methods facilitate rendering and representation adjustments
  * for both exact and fuzzy values under various configurations.
  *
  * TODO these tests document the current behavior of these methods and types but this isn't
  * necessarily the best way to do it. We should consider revising the API to make it more intuitive.
  *
  * The tests cover the following primary scenarios:
  * - Rendering exact and fuzzy values using absolute fuzziness (`asAbsolute`).
  * - Rendering fuzzy values using relative fuzziness (`asRelative`).
  * - Representing values in percentage notation (`asPercentage`).
  * - Special cases and edge conditions, such as handling very small or large fuzziness and non-standard fuzz shapes like `Box`.
  *
  * Test cases are grouped and organized under behavioral descriptions specific to the type being tested.
  *
  * Features tested:
  * - Proper conversion and rendering of exact numerical values.
  * - Accurate handling of fuzzy values derived from absolute or relative fuzz inputs.
  * - Edge cases for specific configurations, ensuring correctness under extreme conditions.
  *
  * Behaviour of key mathematical types:
  * - `Real`: Validates rendering formats for exact, absolute, and relative fuzzy values.
  * - `Angle`: Tests the rendering of angles in radians with fuzziness incorporated.
  * - `InversePower`: Ensures proper handling of fuzzy square roots and similar expressions.
  * - `NaturalExponential`: Validates fuzzy natural logarithmic expressions.
  *
  * Additional focus is given to correct formatting and precision in the rendered outputs
  * for all supported types and fuzziness configurations.
  */
class MaybeFuzzySpec extends AnyFlatSpec with Matchers {

  behavior of "MaybeFuzzy extension methods for Real"

  it should "render exact Real with asAbsolute" in {
    val r = Real("3.14159")
    r.show shouldBe "3.14159*"
    r.asAbsolute shouldBe "3.14159*"
  }

  it should "render fuzzy Real with absolute fuzz using asAbsolute" in {
    val r = Real(100, Some(AbsoluteFuzz(0.5, Gaussian)))
    r.show shouldBe "100.0±0.5%"
  }

  it should "render fuzzy Real with relative fuzz using asAbsolute" in {
    val r = Real(100.0, Some(RelativeFuzz(0.01, Gaussian))) // 1% relative
    r.show shouldBe "100.0±1%"
    //    r.asAbsolute shouldBe "1.00(1)E+02" // Converts to absolute notation
  }

  it should "render fuzzy Real with asRelative" in {
    val r = Real(100.0, Some(RelativeFuzz(0.01, Gaussian)))
    r.show shouldBe "100.0±1%"
  }

  it should "render fuzzy Real with absolute fuzz using asRelative" in {
    val r = Real(100.0, Some(AbsoluteFuzz(1.0, Gaussian)))
    r.render shouldBe "1.00(1)E+02"
    //    r.asRelative shouldBe "100.0±1.0%" // Converts to relative decimal (1.0/100.0 = 0.01)
  }

  it should "render fuzzy Real with asPercentage" in {
    val r = Real(100.0, Some(RelativeFuzz(0.01, Gaussian)))
    r.show shouldBe "100.0±1%"
  }

  behavior of "MaybeFuzzy extension methods for Angle"

  it should "render exact Angle 𝛑/4 with asAbsolute" in {
    val a = Angle.piBy4 // 45 degrees
    a.show shouldBe "¼\uD835\uDED1" // Not sure of exact format
    a.asAbsolute shouldBe a.render
  }

  it should "render fuzzy Angle 𝛑/4 with absolute fuzz using asAbsolute" in {
    val a = Angle(Real(0.25, Some(AbsoluteFuzz(0.01, Gaussian))))
//    a.show shouldBe "0.25(1)\uD835\uDED1"
    //    a.asAbsolute shouldBe "0.78(1)"
  }

  it should "render fuzzy Angle 𝛑/4 with relative fuzz using asAbsolute" in {
    val a = Angle(Real(0.25, Some(RelativeFuzz(0.02, Gaussian))))
    a.show shouldBe "0.785±2%"
  }

  it should "render fuzzy Angle 𝛑/4 with asRelative" in {
    val a = Angle(Real(1.0 / 4, Some(RelativeFuzz(0.02, Gaussian))))
    println(a.convert(Real.zero))
    a.show shouldBe "0.785±2%"
  }

  it should "show fuzzy Angle pi/4 with asPercentage" in {
    val a = Angle(Real(0.25, Some(RelativeFuzz(0.02, Gaussian))))
    a.show should include("±")
    a.show should include("2%") // Percentage form
  }

  behavior of "MaybeFuzzy extension methods for InversePower"

  it should "render exact InversePower with asAbsolute" in {
    val ip = InversePower(2, Real(2).fuzzy) // √2
    ip.toString shouldBe "InversePower(2,Real(2.0,None))"
    ip.render shouldBe "√2.0*"
    ip.asAbsolute shouldBe ip.render
  }

  it should "render fuzzy InversePower with absolute fuzz using asAbsolute" in {
    val ip = InversePower(2, Real(2, Some(AbsoluteFuzz(0.001, Gaussian))))
    ip.render shouldBe "√2.000(1)"
    ip.asAbsolute shouldBe "1.4142(5)"
  }

  it should "render fuzzy InversePower with relative fuzz using asAbsolute" in {
    val ip = InversePower(2, Real(2, Some(RelativeFuzz(0.05, Gaussian))))
    ip.render shouldBe "√2.0±5%"
    ip.asAbsolute shouldBe "1.414(35)"
  }

  it should "render fuzzy InversePower with asRelative" in {
    val ip = InversePower(2, Real(2, Some(RelativeFuzz(0.05, Gaussian))))
    ip.render shouldBe "√2.0±5%"
    ip.show shouldBe "√2.0±5%"
  }

  it should "render fuzzy InversePower with asPercentage" in {
    val ip = InversePower(2, Real(2, Some(RelativeFuzz(0.05, Gaussian))))
    ip.show shouldBe "√2.0±5%"
  }

  behavior of "MaybeFuzzy extension methods for NaturalExponential"

  it should "render exact NaturalExponential with asAbsolute" in {
    val nl = NaturalExponential(Real(1.0, Some(AbsoluteFuzz(0.01, Gaussian))))
    nl.show shouldBe "e^1.00(1)"
    // This creates a fuzzy input, so output will be fuzzy
    nl.asAbsolute shouldBe "2.718(27)"
  }

  it should "render fuzzy NaturalExponential with absolute fuzz using asAbsolute" in {
    val nl = NaturalExponential(Real(1.0, Some(AbsoluteFuzz(0.01, Gaussian))))
    nl.show shouldBe "e^1.00(1)"
    nl.asAbsolute shouldBe "2.718(27)"
  }

  it should "render fuzzy NaturalExponential with relative fuzz using asAbsolute" in {
    val nl = NaturalExponential(Real(1, Some(RelativeFuzz(0.03, Gaussian))))
    nl.show shouldBe "e^1.0±3%"
    val result = nl.asAbsolute
    result shouldBe "2.71(22)"
  }

  it should "render fuzzy NaturalExponential with asRelative" in {
    val nl = NaturalExponential(Real(1, Some(RelativeFuzz(0.03, Gaussian))))
    nl.show shouldBe "e^1.0±3%"
  }

  it should "render fuzzy NaturalExponential with asPercentage" in {
    val nl = NaturalExponential(Real(1, Some(RelativeFuzz(0.03, Gaussian))))
    nl.show shouldBe "e^1.0±3%"
  }

  behavior of "MaybeFuzzy extension methods edge cases"

  it should "handle Box shape fuzziness for Real" in {
    val r = Real(50.0, Some(AbsoluteFuzz(2.0, Box)))
    r.show shouldBe "50.0±4%"
    //    r.asAbsolute shouldBe "5.0[2]E+01"
  }

  it should "handle very small relative fuzziness" in {
    val r = Real(1000.0, Some(RelativeFuzz(0.0001, Gaussian)))
    //    r.asPercentage shouldBe "1.0000(1)E+03"
  }

  it should "handle very large relative fuzziness" in {
    val r = Real(10.0, Some(RelativeFuzz(0.5, Gaussian)))
    r.show should include("50%")
  }
}