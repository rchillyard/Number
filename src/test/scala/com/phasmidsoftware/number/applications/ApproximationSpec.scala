package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class ApproximationSpec extends AnyFlatSpec with should.Matchers {

  behavior of "ApproximationSpec"

  val newtonsPolynomial: Double => Double = x => x * x * x + 6 * x * x + 10 * x - 1
  val newtonsDerivative: Double => Double = x => 3 * x * x + 12 * x + 10

  it should "solve Newton's original problem" in {
    val result = Approximation.solve(0.9,
      newtonsPolynomial,
      newtonsDerivative
    )(Number.zero)
    result.isSuccess shouldBe true
  }

  it should "solve the inverse square root problem for 0.15625" in {
    val x = 0.15625
    val inverseSquareRoot: Double => Double = y => y * y - 1.0/x
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

  it should "evaluate newtons method" in {
    0.1 - newtonsPolynomial(0.1) / newtonsDerivative(0.1) shouldBe 0.09456812110418523
    0.094 - newtonsPolynomial(0.094) / newtonsDerivative(0.094) shouldBe 0.09455165283847572
    0.09455 - newtonsPolynomial(0.09455) / newtonsDerivative(0.09455) shouldBe 0.0945514815435623
  }

  it should "converged" in {
    Approximation.converged(newtonsPolynomial)(0.9)(Number(0.0945514815423266)) shouldBe Success(true)
  }

  it should "iterate" in {
    val iterate: Number => Try[Number] = Approximation.iterate(newtonsPolynomial, newtonsDerivative)
    val zy = iterate(Number(0.1))
    println(zy)
    zy.isSuccess shouldBe true
  }

  it should "delta" in {
    val zy = Approximation.evaluate(newtonsPolynomial)(Number(0.09456812110))
    println(zy)
    zy.isSuccess shouldBe true

  }

}
