package com.phasmidsoftware.number.top


import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.inner.Rational.RationalHelper
import com.phasmidsoftware.number.core.numerical.Constants.one
import com.phasmidsoftware.number.core.numerical.Number.zeroR
import com.phasmidsoftware.number.core.numerical.Real.RealOps
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Constants, Field, Fuzzy, Number, Real}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class RealWorksheetSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Real worksheet"
  it should "perform all operations" in {
    val three: Field = 2 + one
    three shouldBe Real(Number.three)

    val half: Field = Real(r"1/2")
    half shouldBe Real(Rational.half)

    val root3: Field = three power half
    root3 shouldBe ComplexPolar(Number.root3, zeroR, 2)

    val twoFuzzy: Field = (root3 add one) multiply (root3 add -one)
    twoFuzzy.isSame(ComplexCartesian(Number.two,Number.zero)) shouldBe true

    val ok = implicitly[Fuzzy[Field]].same(0.8)(twoFuzzy, Constants.two)
    ok shouldBe true

    val two: Real = 1 + one
    two shouldBe Real(2)

    val halfToo = 1 :/ 2
    halfToo shouldBe Real(Rational.half)

    val infinity = 1 :/ 0 // should be infinity
    infinity shouldBe Real(Rational.infinity)

    // NOTE Demonstrate that sin(π/4) is an exact number, viz. 1 over root 2.
    val piBy4 = Number.pi doDivide 4
    val sinePiBy4 = piBy4.sin
    val expected = Number.root2.invert
    Real(sinePiBy4) shouldBe expected

    val oneHalf = (sinePiBy4 doMultiply sinePiBy4).normalize
    oneHalf shouldBe Real(Number.half)

    // TODO turn the following into test assertions

    // Parsing from Strings: Exact
    Real("1.0") // fewer than 15 decimal places
    Real("0.57721566490153") // ditto

    // Parsing from Strings: Fuzzy
    Real("1.100*").render // ends in Asterisk
    Real("1.100...").render // ends in Ellipsis
    Real("1.1000[1]").render // has explicit (box) error bounds
    Real("1.1000[5]").render // has explicit (box) error bounds
    Real("1.1000(5)").render // has explicit (Gaussian) error bounds
    Real("0.577215664901533").render // 15 or more decimal places
    Real("1.<3>").render // 4/3

  }
}
