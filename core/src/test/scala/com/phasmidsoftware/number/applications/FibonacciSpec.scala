package com.phasmidsoftware.number.applications

import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.expression._
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Number.{one, root5, two}
import com.phasmidsoftware.number.core.numerical._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FibonacciSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "Fibonacci"
  it should "psi" in {
    val expression: Expression = Constants.one - Constants.root5
    val psi: Expression = expression / Constants.two
    psi shouldBe BiFunction(expression, Expression(Constants.two).reciprocal, Product)
  }
  it should "phi" in {
    val expression: Expression = Constants.one + Constants.root5
    val phi: Expression = expression / Constants.two
    phi shouldBe BiFunction(expression, Two.reciprocal, Product)
  }
  it should "render phi+1 as a Field" in {
    val target: Field = Algebraic.phi + Constants.one
    val actual = target.render
    actual shouldBe "2.6180339887498950(55)"
  }
  it should "render phi+1 as an Expression" in {
    val target: Expression = Root.phi plus One
    target.simplify.toString shouldBe "BiFunction{\uD835\uDED7 + 1}"
    val simplified = target.simplify
    println(simplified)
    simplified should matchPattern { case BiFunction(`phi`, One, Sum) => }
    simplified.materialize should ===(2.618033988749895)
  }
  val psi: Expression = Root.psi //(Expression(Constants.one) - Constants.root5) / Constants.two
  val phi: Expression = Root.phi // (Expression(Constants.one) plus Constants.root5) / Constants.two
  it should "fib0" in {
    val phi0: Expression = phi ∧ 0
    phi0.simplify shouldBe One
    val psi0: Expression = psi ∧ 0
    psi0.simplify shouldBe One
    val top: Expression = phi0 - psi0
    top.materialize shouldBe Constants.zero
    val fib0 = top / (phi - psi)
    fib0.materialize shouldBe Constants.zero
  }
  it should "fib1" in {
    val diff: Expression = phi - psi
    diff shouldBe BiFunction(phi, -psi, Sum)
    val fib1 = diff / diff
    fib1.materialize shouldBe Constants.one
  }
  it should "fib2" in {
    val phi2: Expression = phi ∧ 2
    println(s"phi2 = ${phi2.render}")
    val psi2: Expression = psi ∧ 2
    println(s"psi2 = ${psi2.render}")
    val top: Expression = phi2 - psi2
    val bottom = phi - psi
    bottom shouldBe BiFunction(phi, -psi, Sum)
    val materialized = bottom.materialize
    materialized should ===(Constants.root5)
    // TODO reinsert the following
    val fib2 = top / bottom
    val fib2M = fib2.materialize
    top match {
      case BiFunction(x, y, _) =>
        val t1 = x / Constants.root5
        val t2 = y / Constants.root5
        val q = t1 plus t2
      // TODO restore the following
      //       q shouldBe Constants.one (really?  Doesn't seem like it)
//        q.materialize should ===(Constants.one)
    }
    fib2M should ===(Constants.one)
    // TODO restore the following
    //  fib2M shouldBe Constants.one
  }
  it should "psiFuzzy" in {
    import scala.language.implicitConversions

    implicit def convertFromNumber(x: Number): Field = Real(x)

    val expected: Field = (one - root5) / two
    val actual: Field = Fibonacci.psi.materialize
    actual should ===(expected)
  }
  it should "fibFuzzy" in {
    Fibonacci.fibExpression(0).materialize should ===(numerical.Constants.zero)
    Fibonacci.fibExpression(1).materialize should ===(numerical.Constants.one)
    Fibonacci.fibExpression(2).materialize should ===(numerical.Constants.one)
    Fibonacci.fibExpression(3).materialize should ===(numerical.Constants.two)
  }
  it should "fib" in {
    Fibonacci.fib(0) shouldBe BigInt(0)
    // TODO reinsert the following

    //println(s"fib(1) = ${Fibonacci.fib(1)}")
    //println(s"Fibonacci.fibExpression(1).materialize = ${Fibonacci.fibExpression(1).materialize}")
    //println(s"Fibonacci.fibExpression(1) = ${Fibonacci.fibExpression(1)}")

    //Fibonacci.fib(1) shouldBe BigInt(1)
    //Fibonacci.fib(2) shouldBe BigInt(1)
    //Fibonacci.fib(3) shouldBe BigInt(2)
    //Fibonacci.fib(4) shouldBe BigInt(3)
    //Fibonacci.fib(5) shouldBe BigInt(5)
  }
}
