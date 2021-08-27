package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.ExpressionOps
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ConstantsSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  import com.phasmidsoftware.number.core.Number.FuzzOps

  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  val sBoltzmann = "1380649.E-29" // J K ^ -1
  val sPlanck = "6.6260701500E-34" // J Hz ^ -1
  val sC = "299792458" // m sec ^ -1
  val sMu = "1836.15267343(11)" // (dimensionless)

  behavior of "constants"
  it should "have root2" in {
    val target = Constants.root2
    target.isExact(None) shouldBe true
    target.isExact(Some(Scalar)) shouldBe false
    val value = target.normalize
    value should ===(Number(math.sqrt(2)))
    target.multiply(target) shouldBe Number(2)
  }
  it should "have root2 a" in {
    val target: Number = Constants.root2
    val expected = Number(Rational.half, Log2)
    target should ===(expected)
  }
  it should "have root3" in {
    val target = Constants.root3
    target.isExact(None) shouldBe true
    val value = target.normalize
    value should ===(Number(math.sqrt(3)))
    target.multiply(target) shouldBe Number(3)
  }
  it should "have root5" in {
    val target = Constants.root5
    target.isExact(None) shouldBe true
    val value = target.normalize
    value should ===(Number(math.sqrt(5)))
    target.multiply(target) shouldBe Number(5)
  }
  it should "have G" in {
    val target = Constants.G
    target.isExact(None) shouldBe false
    target should ===(Number(6.67430E-11))
    target.render shouldBe sG
  }
  it should "have avagadro" in {
    val target = Constants.avagadro
    target.isExact(None) shouldBe true
    target shouldBe Number(6.0221407600E23)
    target.render shouldBe "602214076000000000000000"
  }
  it should "have boltzmann" in {
    val target = Constants.boltzmann
    target.isExact(None) shouldBe true
    target shouldBe Number("1.38064900E-23")
    target.render shouldBe "1.380649E-23"
  }
  it should "have planck" in {
    val target = Constants.planck
    target.isExact(None) shouldBe true
    // TODO find out why we have to put the following in quotes (compare with avagadro above).
    target shouldBe Number("6.6260701500E-34")
    target.render shouldBe "6.62607015E-34"
  }
  it should "have c" in {
    val target = Constants.c
    target.isExact(None) shouldBe true
    target.toInt shouldBe Some(299792458)
    target.render shouldBe sC
  }
  it should "have mu" in {
    val target = Constants.mu
    target.isExact(None) shouldBe false
    target shouldBe 1836.15267343 ~ 11
    target.render shouldBe sMu
  }
  it should "have phi" in {
    val target = Constants.phi
    target.isExact(None) shouldBe false
    val goldenRatio = Expression.phi
    val maybeNumber: Option[Number] = goldenRatio.asNumber
    maybeNumber.isDefined shouldBe true
    maybeNumber.get should ===(target)
    val result: Number = goldenRatio
    result.render shouldBe "1.618033988749895(21)"
  }
}
