package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Number.{one, root5, two}
import com.phasmidsoftware.number.core.Real.convertFromNumber
import com.phasmidsoftware.number.core._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibonacciSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "Fibonacci"

  it should "psi" in {
    val expression: Expression = Expression(Constants.one) - Constants.root5
    val psi: Expression = expression / Constants.two
    psi shouldBe BiFunction(expression, Expression(Constants.two).reciprocal, Product)
  }

  it should "phi" in {
    val expression: Expression = Expression(Constants.one) plus Constants.root5
    val phi: Expression = expression / Constants.two
    phi shouldBe BiFunction(expression, Expression(Constants.two).reciprocal, Product)
  }

  val psi: Expression = (Expression(Constants.one) - Constants.root5) / Constants.two
  val phi: Expression = (Expression(Constants.one) plus Constants.root5) / Constants.two

  it should "fib0" in {
    val phi0: Expression = phi ^ 0
    phi0.materialize shouldBe Constants.one
    val psi0: Expression = psi ^ 0
    psi0.materialize shouldBe Constants.one
    val top: Expression = phi0 - psi0
    top.materialize shouldBe Constants.zero
    val fib0 = top / (phi - psi)
    fib0.materialize shouldBe Constants.zero
  }

  it should "fib1" in {
    val diff: Expression = phi - psi
    diff shouldBe BiFunction(phi, psi.unary_-, Sum)
    val fib1 = diff / diff
    fib1.materialize shouldBe Constants.one
  }

  it should "fib2" in {
    val phi2: Expression = phi ^ 2
    val psi2: Expression = psi ^ 2
    val top: Expression = phi2 - psi2
    val bottom = phi - psi
    bottom shouldBe BiFunction(phi, psi.unary_-, Sum)
    // TODO reinsert the following
    //    bottom.materialize shouldBe Constants.root5
    //    val fib2 = top / bottom
    //    fib2.materialize shouldBe Constants.one
  }

  it should "psiFuzzy" in {
    val expected: Field = ((one - root5) / two)
    val actual: Field = Fibonacci.psi.materialize
    actual should ===(expected)
  }

  it should "fibFuzzy" in {
    Fibonacci.fibExpression(0).materialize should ===((core.Constants.zero))
    Fibonacci.fibExpression(1).materialize should ===((core.Constants.one))
    Fibonacci.fibExpression(2).materialize should ===((core.Constants.one))
    Fibonacci.fibExpression(3).materialize should ===((core.Constants.two))
  }

  it should "fib" in {
    Fibonacci.fib(0) shouldBe BigInt(0)
    // TODO reinsert the following
    //    Fibonacci.fib(1) shouldBe BigInt(1)
    //    Fibonacci.fib(2) shouldBe BigInt(1)
    //    Fibonacci.fib(3) shouldBe BigInt(2)
    //    Fibonacci.fib(4) shouldBe BigInt(3)
    //    Fibonacci.fib(5) shouldBe BigInt(5)
  }

}
