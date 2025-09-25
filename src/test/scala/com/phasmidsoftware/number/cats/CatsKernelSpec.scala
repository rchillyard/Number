/*
 * Tests for Cats Kernel instances: Rational/ExactNumber/Number
 */

package com.phasmidsoftware.number.cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.phasmidsoftware.number.cats.CatsKernel._
import cats.kernel.{Eq, Order, PartialOrder}
import cats.Show
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.order._
import cats.syntax.show._

import com.phasmidsoftware.number.core.{ExactNumber, Number}
import com.phasmidsoftware.number.core.{FuzzyNumber, AbsoluteFuzz, Box, Gaussian}
import com.phasmidsoftware.number.core.inner.{Rational, Value}
import com.phasmidsoftware.number.core.inner.{PureNumber, Radian}

class CatsKernelSpec extends AnyFlatSpec with Matchers {

  behavior of "Cats instances for Rational"

  it should "provide Eq/Order/Show for Rational" in {
    val r1: Option[Rational] = implicitly[Numeric[Rational]].parseString("3.1415927")
    val r2: Option[Rational] = Some(Rational(31415927, 10000000))
    val r3: Option[Rational] = Some(Rational(4))

    // Eq
    
    r1 === r2 shouldBe true

    Eq[Option[Rational]].neqv(r1, r3) shouldBe true

    // Order
    Order[Option[Rational]].compare(r1, r2) shouldBe 0
    Order[Option[Rational]].lt(r1, r3) shouldBe true

    // Show
    r1.show should not be empty
    Rational(1, 2).show shouldBe "½"
  }

  it should "sort Rational via Cats Order (bridged to scala Ordering)" in {
    // Using Cats Order instance, bridge to scala.math.Ordering
    implicit val ordR: Ordering[Rational] = Order[Rational].toOrdering
    val r = List(Rational(1, 2), Rational(2, 3), Rational(1, 3))
    val x = r.sorted
    x.head shouldBe Rational(1, 3)
    x.tail.head shouldBe Rational(1, 2)
    x.last shouldBe Rational(2, 3)
  }

  behavior of "Cats instances for ExactNumber"

  it should "provide Eq/Order/Show for ExactNumber" in {
    val e1 = ExactNumber(Value.fromRational(Rational(1)), PureNumber)
    val e2 = ExactNumber(Value.fromRational(Rational(1)), PureNumber)
    val e3 = ExactNumber(Value.fromRational(Rational(2)), PureNumber)

    // Eq
    
    Eq[ExactNumber].eqv(e1, e2) shouldBe true
    Eq[ExactNumber].eqv(e1, e3) shouldBe false

    // Order
    Order[ExactNumber].compare(e1, e2) shouldBe 0
    Order[ExactNumber].lt(e1, e3) shouldBe true

    // Show
    e1.show should not be empty
  }

  behavior of "Cats instances for Number"

  it should "provide PartialOrder/Eq/Show for Number" in {
    val n1: Number = Number(1)
    val n2: Number = Number(2)
    val n1b: Number = Number(1)

    // PartialOrder
    val po = PartialOrder[Number]
    po.partialCompare(n1, n1b).isNaN shouldBe false
    po.partialCompare(n1, n1b) shouldBe 0.0
    po.lteqv(n1, n2) shouldBe true

    // Eq (strict zero-difference, excluding NaN)
    Eq[Number].eqv(n1, n1b) shouldBe true
    Eq[Number].eqv(n1, n2) shouldBe false

    // Show
    n1.show should not be empty
  }

  it should "Eq[Number] structural: Fuzzy same triple are equal" in {
    val f1: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Box)))
    val f2: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Box)))
    Eq[Number].eqv(f1, f2) shouldBe true
  }

  it should "Eq[Number] structural: different fuzz not equal" in {
    import com.phasmidsoftware.number.cats.CatsKernel
    val a: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Box)))
    val b: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.2, Box)))
    val c: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, None)
    Eq[Number].eqv(a, b) shouldBe false
    Eq[Number].eqv(a, c) shouldBe false
  }

  it should "Eq[Number] structural: different factor not equal" in {
    val x: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, None)
    val y: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), Radian, None)
    Eq[Number].eqv(x, y) shouldBe false
  }

  it should "Eq[Number]/PartialOrder[Number] respect Gaussian fuzz" in {
    val g1: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Gaussian)))
    val g2: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Gaussian)))
    val b1: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.1, Box)))
    val g3: Number = FuzzyNumber(Value.fromDouble(Some(1.0)), PureNumber, Some(AbsoluteFuzz(0.2, Gaussian)))

    Eq[Number].eqv(g1, g2) shouldBe true
    Eq[Number].eqv(g1, b1) shouldBe false
    Eq[Number].eqv(g1, g3) shouldBe false

    val po = PartialOrder[Number]
    po.partialCompare(g1, b1) should not be 0.0
    po.partialCompare(g1, g2) shouldBe 0.0
  }

  it should "Eq[Number] structural: reflexive for Fuzzy" in {
    val z: Number = FuzzyNumber(Value.fromDouble(Some(2.0)), PureNumber, Some(AbsoluteFuzz(0.05, Box)))
    Eq[Number].eqv(z, z) shouldBe true
  }

  it should "Eq[Number] structural: NaN equals NaN" in {
    val n1: Number = Number.NaN
    val n2: Number = Number.NaN
    Eq[Number].eqv(n1, n2) shouldBe true
  }
}


