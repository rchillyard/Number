/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.Angle.𝛑
import com.phasmidsoftware.number.algebra.eager.WholeNumber.convIntWholeNumber
import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, Functional, NaturalExponential, RationalNumber, Real, WholeNumber}
import com.phasmidsoftware.number.algebra.util.AlgebraException
import com.phasmidsoftware.number.core.numerical.ComplexCartesian
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EagerSpec extends AnyFlatSpec with should.Matchers {

  behavior of "~="

  it should "~= Real/Real" in {
    val x: Eager = Real(1.0)
    val y: Eager = Real(1.0)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= Angle/Angle" in {
    val x: Eager = Angle(Real(1.0))
    val y: Eager = Angle(Real(1.0))
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= WholeNumber/WholeNumber" in {
    val x: Eager = WholeNumber(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/RationalNumber" in {
    val x: Eager = RationalNumber(1)
    val y: Eager = RationalNumber(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/WholeNumber" in {
    val x: Eager = RationalNumber(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/Functional" in {
    val x: Functional = NaturalExponential(1)
    val y: Functional = NaturalExponential(1)
    x == y shouldBe true
    x.~=(y) shouldBe true
  }
  it should "~= Real/WholeNumber" in {
    val x: Eager = Real(1)
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/RationalNumber" in {
    val x: Eager = Real(1)
    val y: Eager = RationalNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= RationalNumber/Real" in {
    val y: Eager = Real(1)
    val x: Eager = RationalNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/Functional (1)" in {
    val x: Eager = Real(scala.math.Pi)
    val y: Eager = Angle(Real(1.0))
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Real/Functional (2)" in {
    val x: Eager = Real(math.E)
    val y: Eager = NaturalExponential(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/Real" in {
    val y: Eager = Real(scala.math.Pi)
    val x: Eager = Angle(Real(1.0))
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= Functional/WholeNumber" in {
    val x: Eager = Angle(Real(1.0 / math.Pi))
    val y: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }
  it should "~= WholeNumber/Functional" in {
    val y: Eager = Angle(Real(1.0 / math.Pi))
    val x: Eager = WholeNumber(1)
    x == y shouldBe false
    x.~=(y) shouldBe true
  }

  behavior of "Imaginary"
  it should "work for i" in {
    Eager.i match {
      case Imaginary(IsUnity(x)) =>
        x.isExact shouldBe true
      case _ =>
        fail("should be Imaginary")
    }
  }
  it should "work for 2i" in {
    Eager.imaginary(2) match {
      case Imaginary(x) =>
        x.isExact shouldBe true
        x should ===(WholeNumber(2))
      case _ =>
        fail("should be Imaginary")
    }
  }
  it should "work for 2.i" in {
    import Eager.IntToImaginary
    val y = 2.i // to give 2i
    y match {
      case Imaginary(x) =>
        x.isExact shouldBe true
        x should ===(WholeNumber(2))
      case _ =>
        fail("should be Imaginary")
    }
  }
  it should "not work for -2i" in {
    an[AlgebraException] should be thrownBy (Eager.imaginary(-2))
  }
  it should "not provide an exact value i𝛑" in {
    Eager.imaginary(𝛑) match {
      case Imaginary(x) =>
        x.isExact shouldBe false
        x.toDouble shouldBe math.Pi +- 1E-8
      case _ =>
        fail("should be Imaginary")
    }
  }

  behavior of "asComplex"
  it should "work for i" in {
    Eager.i.asComplex shouldBe Some(Complex(ComplexCartesian(0, 1)))
  }
  it should "work for i𝛑" in {
    val inversePower = Eager.imaginary(𝛑)
    val asComplex = inversePower.asComplex
    asComplex.isDefined shouldBe true
    asComplex.get.complex.isSame(ComplexCartesian(0, math.Pi)) shouldBe true
  }
  it should "work for 2i" in {
    Eager.imaginary(2).asComplex shouldBe Some(Complex(ComplexCartesian(0, 2)))
  }

  behavior of "SUM"
  //
  //  it should "~= Real/Real" in {
  //    val x: Eager = Real(1.0)
  //    val y: Eager = Real(1.0)
  ////    x.sum(y) shouldBe true
  //  }
  //  it should "~= Angle/Angle" in {
  //    val x: Eager = Angle(Real(1.0))
  //    val y: Eager = Angle(Real(1.0))
  //    x == y shouldBe true
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= WholeNumber/WholeNumber" in {
  //    val x: Eager = WholeNumber(1)
  //    val y: Eager = WholeNumber(1)
  //    x == y shouldBe true
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= RationalNumber/RationalNumber" in {
  //    val x: Eager = RationalNumber(1)
  //    val y: Eager = RationalNumber(1)
  //    x == y shouldBe true
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= RationalNumber/WholeNumber" in {
  //    val x: Eager = RationalNumber(1)
  //    val y: Eager = WholeNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Functional/Functional" in {
  //    val x: Functional = NaturalExponential(1)
  //    val y: Functional = NaturalExponential(1)
  //    x == y shouldBe true
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Real/WholeNumber" in {
  //    val x: Eager = Real(1)
  //    val y: Eager = WholeNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Real/RationalNumber" in {
  //    val x: Eager = Real(1)
  //    val y: Eager = RationalNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= RationalNumber/Real" in {
  //    val y: Eager = Real(1)
  //    val x: Eager = RationalNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Real/Functional (1)" in {
  //    val x: Eager = Real(scala.math.Pi)
  //    val y: Eager = Angle(Real(1.0))
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Real/Functional (2)" in {
  //    val x: Eager = Real(math.E)
  //    val y: Eager = NaturalExponential(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Functional/Real" in {
  //    val y: Eager = Real(scala.math.Pi)
  //    val x: Eager = Angle(Real(1.0))
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= Functional/WholeNumber" in {
  //    val x: Eager = Angle(Real(1.0 / math.Pi))
  //    val y: Eager = WholeNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
  //  it should "~= WholeNumber/Functional" in {
  //    val y: Eager = Angle(Real(1.0 / math.Pi))
  //    val x: Eager = WholeNumber(1)
  //    x == y shouldBe false
  //    x.~=(y) shouldBe true
  //  }
}
