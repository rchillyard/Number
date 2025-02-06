package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Number.{one, root5, two}
import com.phasmidsoftware.number.core.Real.convertFromNumber
import com.phasmidsoftware.number.core._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibonacciSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "Fibonacci"

  ignore should "psi" in {
    val expression: Expression = Expression(Constants.one) - Constants.root5
    val psi: Expression = expression / Constants.two
    psi shouldBe BiFunction(expression, Expression(Constants.two).reciprocal, Product)
  }

  ignore should "phi" in {
    val expression: Expression = Expression(Constants.one) plus Constants.root5
    val phi: Expression = expression / Constants.two
    phi shouldBe BiFunction(expression, Expression(Constants.two).reciprocal, Product)
  }

  val psi: Expression = Psi //(Expression(Constants.one) - Constants.root5) / Constants.two
  val phi: Expression = Phi // (Expression(Constants.one) plus Constants.root5) / Constants.two

  ignore should "fib0" in {
    val phi0: Expression = phi ^ 0
    phi0.materialize shouldBe Constants.one
    val psi0: Expression = psi ^ 0
    psi0.materialize shouldBe Constants.one
    val top: Expression = phi0 - psi0
    top.materialize shouldBe Constants.zero
    val fib0 = top / (phi - psi)
    fib0.materialize shouldBe Constants.zero
  }

  ignore should "fib1" in {
    val diff: Expression = phi - psi
    println(diff)
    diff shouldBe BiFunction(phi, -psi, Sum)
    val fib1 = diff / diff
    fib1.materialize shouldBe Constants.one
  }

  ignore should "fib2" in {
    val phi2: Expression = phi ^ 2
    val psi2: Expression = psi ^ 2
    val top: Expression = phi2 - psi2
    val bottom = phi - psi
    bottom shouldBe BiFunction(phi, -psi, Sum)
    bottom.materialize should ===(Constants.root5)
    // TODO reinsert the following
    val fib2 = top / bottom
    val fib2M = fib2.materialize
    top match {
      case BiFunction(x, y, f) =>
        println(s"x:$x y:$y f:$f")
        val t1 = x / Constants.root5
        val t2 = y / Constants.root5
        val q = t1 plus t2
        println(s"t1:$t1")
        println(s"t2:$t2")
        println(s"q:$q")
        // TODO restore the following
        //        q shouldBe Constants.one
        q.materialize should ===(Constants.one)
    }
    fib2M should ===(Constants.one)
    // TODO restore the following
    //    fib2M shouldBe Constants.one
  }

  ignore should "psiFuzzy" in {
    val expected: Field = (one - root5) / two
    val actual: Field = Fibonacci.psi.materialize
    actual should ===(expected)
  }

  ignore should "fibFuzzy" in {
    Fibonacci.fibExpression(0).materialize should ===(core.Constants.zero)
    Fibonacci.fibExpression(1).materialize should ===(core.Constants.one)
    Fibonacci.fibExpression(2).materialize should ===(core.Constants.one)
    Fibonacci.fibExpression(3).materialize should ===(core.Constants.two)
  }

  ignore should "fib" in {
    Fibonacci.fib(0) shouldBe BigInt(0)
    // TODO reinsert the following
    //        Fibonacci.fib(1) shouldBe BigInt(1)
    //        Fibonacci.fib(2) shouldBe BigInt(1)
    //        Fibonacci.fib(3) shouldBe BigInt(2)
    //        Fibonacci.fib(4) shouldBe BigInt(3)
    //        Fibonacci.fib(5) shouldBe BigInt(5)
  }

}
