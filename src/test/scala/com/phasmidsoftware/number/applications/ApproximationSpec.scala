package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.applications.Approximation.evaluateWithoutDerivative
import com.phasmidsoftware.number.core.Number
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class ApproximationSpec extends AnyFlatSpec with should.Matchers with PrivateMethodTester {

  behavior of "Approximation"

  val newtonsPolynomial: Double => Double = x => x * x * x + 6 * x * x + 10 * x - 1
  val newtonsDerivative: Double => Double = x => 3 * x * x + 12 * x + 10
  val newtonsSecondDerivative: Double => Double = x => 6 * x + 12

  val cosineFunction: Double => Double = x => math.cos(x) - x
  val cosineDerivative: Double => Double = x => -math.sin(x) - 1
  val cosineSecondDerivative: Double => Double = x => -math.cos(x)

  it should "solve Newton's original problem" in {
    val result = Approximation.solve(0.9,
      newtonsPolynomial,
      newtonsDerivative
    )(Number.zero)
    result.isSuccess shouldBe true
  }

  it should "solve cosine problem" in {
    val result = Approximation.solve(0.9,
      cosineFunction,
      cosineDerivative
    )(Number.half)
    result.isSuccess shouldBe true
  }

  it should "solve cosine problem using Halley's method" in {
    val result = Approximation.solve(0.9,
      cosineFunction,
      cosineDerivative,
      cosineSecondDerivative
    )(Number.half)
    result.isSuccess shouldBe true
  }

  it should "solve Newton's original problem using Halley's method" in {
    val result = Approximation.solve(0.9,
      newtonsPolynomial,
      newtonsDerivative,
      newtonsSecondDerivative,
    )(Number.zero)
    result.isSuccess shouldBe true
  }

  it should "solve the inverse square root problem for 0.15625" in {
    val x = 0.15625
    val inverseSquareRoot: Double => Double = y => y * y - 1.0 / x
    val inverseDerivative: Double => Double = y => 2 * y

    Approximation.solve(0.9, inverseSquareRoot, inverseDerivative)(Number(2.5)).isSuccess shouldBe true
  }

  it should "evaluate newtonsPolynomial" in {
    newtonsPolynomial(0.1) shouldBe 0.06099999999999994
    newtonsPolynomial(0.094) shouldBe -0.006153416000000078
    newtonsPolynomial(0.09455) shouldBe -1.653612862495546E-5
  }

  it should "evaluate newtonsDerivative" in {
    newtonsDerivative(0.1) shouldBe 11.23
    newtonsDerivative(0.094) shouldBe 11.154508
    newtonsDerivative(0.09455) shouldBe 11.1614191075
  }

  it should "evaluate newtonsSecondDerivative" in {
    newtonsSecondDerivative(0.1) shouldBe 12.6
    newtonsSecondDerivative(0.094) shouldBe 12.564
    newtonsSecondDerivative(0.09455) shouldBe 12.5673
  }

  it should "evaluate newtons method" in {
    0.1 - newtonsPolynomial(0.1) / newtonsDerivative(0.1) shouldBe 0.09456812110418523
    0.094 - newtonsPolynomial(0.094) / newtonsDerivative(0.094) shouldBe 0.09455165283847572
    0.09455 - newtonsPolynomial(0.09455) / newtonsDerivative(0.09455) shouldBe 0.0945514815435623
  }

  it should "converged" in {
    Approximation.converged(newtonsPolynomial, newtonsDerivative)(0.9)(Number(0.0945514815423266)) shouldBe Success(true)
  }

  it should "iterate" in {
    val iterate: Number => Try[Number] = Approximation.iterate(newtonsPolynomial, newtonsDerivative)
    val zy = iterate(Number(0.1))
    println(zy)
    zy.isSuccess shouldBe true
  }

  it should "delta" in {
    val zy = Approximation.evaluate(newtonsPolynomial, newtonsDerivative)(Number(0.09456812110))
    println(zy)
    zy.isSuccess shouldBe true

  }

  it should "do correction for Newton's polynomial" in {
    val correctionSymbol = Symbol("correction")
    val z = PrivateMethod[Try[Number]](correctionSymbol)
    val x = Number(0.1)
    val py = evaluateWithoutDerivative(newtonsPolynomial)(x) // x^3 + 6x^2 + 10x - 1
    val qy = evaluateWithoutDerivative(newtonsDerivative)(x) // 3x^2 + 12x + 10
    val ry = for (p <- py; q <- qy) yield p divide q
    val cy: Try[Number] = Approximation invokePrivate z(ry.get, qy.get, x, Seq(newtonsPolynomial, newtonsDerivative, newtonsSecondDerivative))
    cy.isSuccess shouldBe true
    cy.get.toDouble.isDefined shouldBe true
    cy.get.toDouble.get shouldBe -0.001024882810531 +- 1E-7
  }

  it should "do correction for cosine problem" in {
    val correctionSymbol = Symbol("correction")
    val z = PrivateMethod[Try[Number]](correctionSymbol)
    val x = Number.half
    val py = evaluateWithoutDerivative(cosineFunction)(x) // cos(x) - x
    val qy = evaluateWithoutDerivative(cosineDerivative)(x) // -sin(x) - 1
    val ry = for (p <- py; q <- qy) yield p divide q
    val cy: Try[Number] = Approximation invokePrivate z(ry.get, qy.get, x, Seq(cosineFunction, cosineDerivative, cosineSecondDerivative))
    cy.isSuccess shouldBe true
    cy.get.toDouble.isDefined shouldBe true
    cy.get.toDouble.get shouldBe -0.177386686 +- 1E-7
  }

}
