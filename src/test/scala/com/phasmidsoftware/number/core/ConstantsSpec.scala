package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.{ExpressionOps, one}
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

  private val sAlpha = "0.0072973525693(11)"
  //
  //  val sPhi = "1.618033988749894"
  //  val sGamma = "0.57721566490153286060651209008240243104215933593992"
  //  val sG = "6.67430(15)E-11" // m ^ 3 kg ^ -1 s ^ -2
  //  val sAvagadro = "6.0221407600E23" // mole ^ -1
  //  val sBoltzmann = "1380649.E-29" // J K ^ -1
  //  val sPlanck = "6.6260701500E-34" // J Hz ^ -1
  //  val sC = "299792458" // m sec ^ -1
  //  val sMu = "1836.15267343(11)" // (dimensionless)
  //  val one: Number = Number.one
  //  val zero: Number = Number.zero
  //  val pi: Number = Number.pi
  //  val e: Number = Number.e
  //  val i: Complex = Complex.i
  //  val root2: Number = Number.root2
  //  val root3: Number = Number.root3
  //  val root5: Number = Number.root5
  //  import com.phasmidsoftware.number.core.Number.FuzzOps
  //  lazy val phi: Number = Number(sPhi)
  //  lazy val gamma: Number = Number(sGamma)
  //  lazy val G: Number = Number(sG)
  //  lazy val alpha: Number = 0.0072973525693~11 // (dimensionless)
  //  lazy val avagadro: Number = Number(6.0221407600E23)
  //  lazy val boltzmann: Number = Number(sBoltzmann)
  //  lazy val planck: Number = Number(sPlanck)
  //  lazy val c: Number = Number(sC)
  //  lazy val mu: Number = Number(sMu)

  behavior of "constants"
  it should "have root2" in {
    val root2 = Constants.root2
    val value = root2.normalize
    value should ===(Number(math.sqrt(2)))
    root2.multiply(root2) shouldBe Number(2)
  }
  it should "have root2 a" in {
    val root2: Number = Constants.root2
    val expected = Number(Rational.half, Log2)
    root2 should ===(expected)
  }
  it should "have root3" in {
    val root3 = Constants.root3
    val value = root3.normalize
    value should ===(Number(math.sqrt(3)))
    root3.multiply(root3) shouldBe Number(3)
  }
  it should "have root5" in {
    val root5 = Constants.root5
    val value = root5.normalize
    value should ===(Number(math.sqrt(5)))
    root5.multiply(root5) shouldBe Number(5)
  }

  it should "have phi" in {
    val phi = Constants.phi
    val z = one + Constants.root5
    println(Constants.root5.scale(Scalar))
    println(z.asNumber)
    val goldenRatio = Expression.phi
    println(goldenRatio)
    println(Expression.em.simplifier(goldenRatio))
    val maybeNumber: Option[Number] = goldenRatio.asNumber
    maybeNumber.isDefined shouldBe true
    println(maybeNumber)
    maybeNumber.get should ===(phi)
  }
}
