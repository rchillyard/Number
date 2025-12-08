package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.expression.Expression.ExpressionOps
import com.phasmidsoftware.number.core.expression.{Expression, Literal, Root}
import com.phasmidsoftware.number.core.inner.{Log2, Rational}
import com.phasmidsoftware.number.core.numerical.Constants.sGamma
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.language.postfixOps
import scala.math.Numeric.Implicits.infixNumericOps

class ConstantsSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  import com.phasmidsoftware.number.core.numerical.Number.FuzzOps

  val sG = "6.67430(15)E-11" // m ∧ 3 kg ∧ -1 s ∧ -2
  val sBoltzmann = "1380649.E-29" // J K ∧ -1
  val sPlanck = "6.6260701500E-34" // J Hz ∧ -1
  val sC = "299792458" // m sec ∧ -1
  val sMu = "1836.15267343(11)" // (dimensionless)

  behavior of "constants"
  it should "have root2" in {
    val target = Constants.root2
    val value = target.normalize
    value match {
      case Real(n) => n should ===(Number(math.sqrt(2)))
      case _ => fail(s"wrong result: $value")
    }
    target.multiply(target) shouldBe Constants.two
  }
  it should "have root2 a" in {
    val target: Field = Constants.root2
    val expected = Real(Number(Rational.half, Log2))
    target should ===(expected)
  }
  it should "have root3" in {
    val target = Constants.root3
    target.isExact shouldBe true
    val value = target.normalize
    value should ===(Real(math.sqrt(3)))
    target.multiply(target) shouldBe Real(3)
  }
  it should "have root5" in {
    val target = Constants.root5
    target.isExact shouldBe true
    val value = target.normalize
    value should ===(Real(math.sqrt(5)))
    target.multiply(target) shouldBe Real(5)
  }
  it should "have G" in {
    val target = Constants.G
    target.isExact shouldBe false
    target should ===(Number(6.67430E-11))
    target.render shouldBe sG
  }
  it should "have gamma" in {
    val target = Constants.gamma
    target.isExact shouldBe false
    target should ===(Number(0.577215664901533))
    target.render shouldBe (sGamma.substring(0, 17) + "9*")
  }
  it should "have avagadro" in {
    val target = Constants.avagadro
    target.isExact shouldBe true
    target should ===(Real(6.0221407600E23))
    target.render shouldBe "602214076000000000000000"
  }
  it should "have boltzmann" in {
    val target = Constants.boltzmann
    target.isExact shouldBe true
    target shouldBe Real("1.38064900E-23")
    target.render shouldBe "1.380649E-23"
  }
  it should "have planck" in {
    val target = Constants.planck
    target.isExact shouldBe true
    // TODO find out why we have to put the following in quotes (compare with avagadro above).
    target shouldBe Real("6.6260701500E-34")
    target.render shouldBe "6.62607015E-34"
  }
  it should "have c" in {
    val target: Real = Constants.c
    target.isExact shouldBe true
    target.toInt shouldBe 299792458
    target.render shouldBe sC
  }
  it should "have mu" in {
    val target = Constants.mu
    target.isExact shouldBe false
    target shouldBe Real(1836.15267343 ~ 11)
    target.render shouldBe sMu
  }
  it should "have phi" in {
    val target = Constants.phi
    target.isExact shouldBe true
    val goldenRatio = Root.phi
    val maybeNumber: Option[Number] = goldenRatio.asNumber
    maybeNumber.isDefined shouldBe true
    Real(maybeNumber.get) should ===(target)
    goldenRatio.render shouldBe "\uD835\uDED7"
    val result: Field = goldenRatio.materialize
    result.render shouldBe "1.6180339887498950(47)"
  }
  it should "have alpha" in {
    val target = Constants.alpha
    target.isExact shouldBe false
    val alpha = for (number <- (target invert).asNumber; x <- number.toNominalDouble) yield x
    alpha.get shouldBe 137.0 +- 0.04
  }

  behavior of "toString"
  it should "work for G" in {
    val target = Constants.G
    target.toString shouldBe sG
  }
}
