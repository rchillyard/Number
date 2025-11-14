/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.ComplexPolar.±
import com.phasmidsoftware.number.core.algebraic.Quadratic.phiApprox
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.inner.{NatLog, Rational, SquareRoot}
import com.phasmidsoftware.number.core.{ComplexCartesian, ComplexPolar, ExactNumber, GeneralNumber, NumberException, Real}
import com.phasmidsoftware.number.expression.core.FuzzyEquality
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.RationalNumber.half
import com.phasmidsoftware.number.algebra.Valuable.{valuableToField, valuableToMaybeField}
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, em, pi}
import com.phasmidsoftware.number.expression.expr.ExpressionHelper.math
import com.phasmidsoftware.number.expression.expr.Root.phi
import com.phasmidsoftware.number.expression.parse.ShuntingYardParser
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

  type OldExpression = com.phasmidsoftware.number.expression.expr.Expression
  type OldNumber = com.phasmidsoftware.number.core.Number

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: GeneralNumber => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "evaluate"
  it should "evaluate 1 + -1" in {
    val x: Expression = Expression(1) + -1
    x.evaluateAsIs shouldBe Some(Valuable.zero)
  }
  it should "evaluate 1 * -1" in {
    val x: Expression = Expression(1) * -1
    x.evaluateAsIs shouldBe Some(WholeNumber(-1))
  }
  // FIXME this has to do with imaginary numbers
  ignore should "evaluate i * 2" in {
    val x: Expression = ConstI * 2
    val result: Option[Valuable] = x.evaluateAsIs
    result.isDefined shouldBe true
    val expected = Valuable(core.Real(ExactNumber(-4, SquareRoot)))
    result.get shouldBe expected
  }

  behavior of "parse"
  private val syp: ShuntingYardParser.type = ShuntingYardParser
  //  it should "parse 1" in {
  //    syp.parseInfix("1") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix("(1)") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix(" (1)") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix(" (1 )") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix(" (1 )") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix("( 1 ) ") should matchPattern { case Success(Stack(List(Expr(TerminalExpression(Number.one))))) => }
  //    syp.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
  //    syp.parseInfix("( ( 0.5 + 3.00* ) + ( 2.00* * ( 0.5 + 3.00* ) ) )") should matchPattern { case Success(_) => }
  //  }
  it should "parse and evaluate sqrt(3)" in {
    val eo: Option[Expression] = Expression.parse("3 ∧ ( 2 ∧ -1 )")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(Expression(3), BiFunction(Expression(2), Expression(-1), Power), Power)
  }
  it should "parse and evaluate half" in {
    val eo: Option[Expression] = Expression.parse("2 ∧ -1")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(Expression(2), Expression(-1), Power)
  }

  // NOTE that the apply function always takes a Field and returns a Field. Not to be confused with applyExact.
  behavior of "ExpressionMonoFunction"
  it should "work for Negate" in {
    val f: ExpressionMonoFunction = Negate
    f(Valuable.zero) shouldBe Valuable.zero
    f(Valuable.one) shouldBe Valuable.minusOne
    f(Valuable.minusOne) shouldBe Valuable.one
    f(f(Valuable.two)) shouldBe Valuable.two
  }
  it should "work for Reciprocal" in {
    val f: ExpressionMonoFunction = Reciprocal
    f(Valuable.zero) shouldBe Valuable.infinity
    f(Valuable.one) shouldBe Valuable.one
    f(Valuable.half) should ===(Valuable.two)
    f(Valuable.two) shouldBe Valuable.half
    //    f(Valuable.e) shouldBe Real(ExactNumber(-1, NatLog)) TODO fix this later
  }
  it should "work for Exp" in {
    val f: ExpressionMonoFunction = Exp
    f(Valuable.zero) shouldBe Valuable.one
    f(Valuable.one) shouldBe Valuable.e
  }
  it should "work for Ln" in {
    val f: ExpressionMonoFunction = Ln
    f(Valuable.one) shouldBe Valuable.zero
    f(Valuable.e) shouldBe Valuable.one
  }
  it should "work for Sine" in {
    val f: ExpressionMonoFunction = Sine
    f(Angle.piBy2) shouldBe Valuable.one
    f(Angle.zero) shouldBe Valuable.zero
  }
  it should "work for Cosine" in {
    val f: ExpressionMonoFunction = Cosine
    f(Angle.piBy2) shouldBe Valuable.zero
    f(Angle.zero) shouldBe Valuable.one
  }


  behavior of "evaluateAsIs for UniFunction"
  it should "work for Exp(1)" in {
    val x = expr.UniFunction(One, Exp)
    val result = x.evaluateAsIs
    result shouldBe Some(Valuable.e)
  }
  it should "work for Reciprocal" in {
    expr.UniFunction(Two, Reciprocal).evaluateAsIs shouldBe Some(Valuable.half)
  }
  it should "work for Ln(-1)" in {
    val x = expr.UniFunction(MinusOne, Ln)
    val result = x.evaluateAsIs
    result shouldBe Some(Complex(ComplexPolar(core.Number.pi, core.Number.piBy2.makeNegative, 1)))
  }

  behavior of "materialize UniFunction"
  it should "work for Exp(1)" in {
    val x = expr.UniFunction(One, Exp)
    val result = x.materialize
    result shouldBe Valuable.e
  }
  it should "work for Reciprocal" in {
    expr.UniFunction(Two, Reciprocal).materialize shouldBe Valuable.half
  }
  it should "work for Exp(Ln(2))" in {
    val x = expr.UniFunction(expr.UniFunction(Two, Ln), Exp)
    val result = x.materialize
    result shouldBe Valuable.two
  }

  behavior of "Expression"
  it should "simplifyAndEvaluate" in {
    val x1 = Valuable.one
    val x2 = Valuable.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    val result: Valuable = e.materialize
    valuableToMaybeField(result) shouldEqual core.Real(core.Number(Math.PI + 1))
  }
  it should "render" in {
    val x1 = Valuable.one
    val x2 = Valuable.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    e.toString shouldBe "BiFunction{Literal(WholeNumber(1),None) + Literal(\uD835\uDED1,None)}"
    e.render shouldBe "4.1415926535897930(67)"
    e.materialize.render shouldBe "4.1415926535897930(67)"
  }
  it should "evaluate 3 5 + 7 2 – *" in {
    val expression = (Expression(3) + 5) * (7 - 2)
    val result = expression.simplify.materialize
    result shouldEqual WholeNumber(40)
  }

  behavior of "ExpressionOps"
  it should "evaluate +" in {
    val x: Expression = Expression(1) + 2
    x shouldBe BiFunction(Expression(1), Expression(2), Sum)
  }
  it should "evaluate -" in {
    val x = Expression(1) - 2
    val simplifiedExpression = x.simplify
    val result = simplifiedExpression.materialize
    result shouldBe Valuable.minusOne
  }
  it should "evaluate *" in {
    val x = Expression(3) * 2
    x.materialize shouldEqual Valuable(6)
  }
  it should "evaluate /" in {
    // CONSIDER should this be Valuable(3)?
    val x = Expression(6) / 2
    x.materialize shouldEqual RationalNumber(3)
  }
  it should "evaluate ∧ 2" in {
    val x = Expression(6) ∧ 2
    x.materialize shouldEqual Valuable(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = Expression(36).sqrt
    x.materialize shouldEqual ±(6)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = ConstPi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Valuable.one
  }
  // FIXME stack overflow
  ignore should "evaluate atan" in {
    val zero: Expression = expr.Zero
    zero.atan(Valuable.one).materialize shouldBe Angle.piBy2
    One.atan(0).materialize shouldBe Angle.zero
    One.atan(Valuable.root3).evaluateAsIs shouldBe Some(Angle.piBy3)
    One.atan(One).evaluateAsIs shouldBe Some(Valuable.piBy4)
  }
  it should "evaluate log 2" in {
    val base = Two
    One.log(base).materialize shouldBe Valuable.zero
    Two.log(base).materialize shouldBe Valuable.one
    //    Expression(4).log(base).materialize.asNumber shouldBe Some(Number.two)
  }
  it should "evaluate log e" in {
    val base = ConstE
    One.log(base).materialize shouldBe Valuable.zero
    ConstE.log(base).materialize shouldBe Valuable.one
  }
  it should "evaluate log 10" in {
    val base = Expression(10)
    One.log(base).materialize shouldBe Valuable.zero
    Expression(10).log(base).materialize shouldBe Valuable.one
  }
  it should "fail to evaluate log 1 x or log 0 x" in {
    val base = Expression(1)
    a[NumberException] should be thrownBy Two.log(base).materialize
  }
  it should "evaluate ln E" in {
    val x: Expression = ConstE
    val y: Expression = x.ln
    y.materialize shouldBe Valuable.one
  }
  it should "evaluate ln 2E" in {
    val x: Expression = ConstE * 2
    val y: Expression = x.ln
    val result = y.materialize
    val expected = Real("1.693147180559945(13)")
    result shouldEqual expected
  }
  it should "evaluate xxx" in {
    val x: Expression = ConstE.log(Two) // lg E with value close to √2
    val y: Expression = x.reciprocal.simplify
    val result = y.materialize
    val expected = Real("0.6931471805599453(13)")
    result shouldEqual expected
  }
  // (fixed) we should be able to compare y with L2 (this tests for Issue #125)
  it should "evaluate xxx 2" in {
    val x: Expression = ConstE.log(Two) // lg E with value close to √2
    val y: Expression = x.reciprocal.simplify
    y === L2.simplify
  }

  behavior of "toString"
  it should "work for (sqrt 7)∧2" in {
    val seven: Expression = 7
    val result: Expression = seven.sqrt ∧ 2
    result.toString shouldBe BiFunction(BiFunction(WholeNumber(7), RationalNumber(Rational.half), Power), 2, Power).toString
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    val z: Valuable = (ConstE * 2).materialize
    val q = valuableToField(z).normalize
    q.toString shouldBe "5.436563656918091[15]"
  }

  behavior of "isExact"
  it should "be true for any constant Valuable" in {
    Valuable.one.isExact shouldBe true
    Valuable.pi.isExact shouldBe true
  }
  it should "be true for any sum of exact Numbers of the same factor (not e)" in {
    (One + Valuable.two).isExact shouldBe true
    (ConstPi + Valuable.pi).isExact shouldBe true
  }
  it should "be false for any product of exact Numbers and a NatLog factor (except for one)" in {
    (Expression(2) * Valuable.e).isExact shouldBe false
  }
  it should "be true for product of one exact Numbers and a NatLog factor" in {
    val expression = Expression(1) * Valuable.e
    expression.isExact shouldBe true
  }
  it should "be true for product of zero exact Numbers and a NatLog factor" in {
    (Expression(0) * Valuable.e).isExact shouldBe true
  }

  behavior of "depth"
  it should "be 1 for any atomic expression" in {
    Expression(1).depth shouldBe 1
    Expression.one.depth shouldBe 1
    Expression(1).depth shouldBe 1
    pi.depth shouldBe 1
  }
  it should "be 2 for any UniFunction expression" in {
    expr.UniFunction(1, Negate).depth shouldBe 2
    expr.UniFunction(1, Cosine).depth shouldBe 2
    expr.UniFunction(1, Sine).depth shouldBe 2
  }
  it should "be more than 1 for other expression" in {
    (ConstE * 2).depth shouldBe 2
    (ConstE * 2 / 2).depth shouldBe 3
    val expression = (Expression(7).sqrt ∧ 2).simplify
    expression.depth shouldBe 2
  }

  behavior of "Euler"
  it should "prove Euler's identity 1" in {
    val iPi = ComplexCartesian(0, core.Number.pi)
    val euler: Expression = Expression(Valuable.e) ∧ Complex(iPi)
    euler.materialize shouldBe Valuable.minusOne
  }
  it should "prove Euler's identity 2" in {
    val iPi = core.Complex.convertToPolar(ComplexCartesian(0, core.Number.pi))
    val euler: Expression = Expression(Valuable.e) ∧ Complex(iPi)
    euler.materialize shouldBe Valuable.minusOne
  }

  behavior of "FieldExpression"
  it should "Zero be equal to zero" in {
    val target = Literal(Valuable.zero)
    target shouldBe Zero
    target should matchPattern { case Literal(Valuable.zero, _) => }
    target should matchPattern { case ValueExpression(Valuable.zero, _) => }
  }
  it should "One be equal to one" in {
    val target = Literal(Valuable.one)
    target shouldBe One
    target should matchPattern { case Literal(Valuable.one, _) => }
    target should matchPattern { case ValueExpression(Valuable.one, _) => }
  }

  behavior of "simplifyConstant"
  it should "simplify biFunction expressions" in {
    val em: ExpressionMatchers = Expression.em
    Expression.simplifyConstant(BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Expression(-2))
    BiFunction(Two, MinusOne, Product).simplify shouldBe Expression(-2)
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some(Valuable.zero)
  }

  behavior of "simplifyExact"
  it should "simplify biFunction expressions" in {
    val em: ExpressionMatchers = Expression.em
    Expression.simplifyExact(BiFunction(Literal(Angle.twoPi, None), RationalNumber.half, Product)) should matchPattern { case em.Match(Literal(Angle.pi, _)) => }
  }

  behavior of "simplify"
  it should "simplify field expressions" in {
    Expression(1).simplify shouldBe Expression(1)
    ConstPi.simplify shouldBe ConstPi
    val simplify: Expression = phi.simplify
    simplify shouldBe Expression(Valuable(Algebraic.phi))
    phi.simplify.materialize should ===(phiApprox)
  }
  it should "simplify function expressions" in {
    expr.UniFunction(expr.UniFunction(One, Negate), Negate).simplify shouldBe One
    expr.UniFunction(Two, Reciprocal).simplify shouldBe Expression(Valuable.half)
    expr.UniFunction(Valuable.pi, Sine).simplify shouldBe Expression(Valuable.zero)
  }
  it should "simplify biFunction expressions" in {
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).simplify shouldBe Zero
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some(Valuable.zero)
  }

  // XXX appears to have fixed infinite recursion
  it should "simplify aggregate expressions" in {
    Aggregate.total(BiFunction(Two, MinusOne, Product), Two).simplify shouldBe Zero
    Aggregate.total(Two).simplify shouldBe Two
    Aggregate.empty(Sum).simplify shouldBe Zero
  }
  it should "aggregate 2" in {
    val target = (One * ConstPi * Two * MinusOne).simplify
    val expected = Expression("-2 * 𝛑")
    target shouldBe expected
  }
  it should "evaluate e * e" in {
    val expression: Expression = ConstE * ConstE
    expression.simplify shouldBe Literal(Valuable(core.Real(ExactNumber(2, NatLog))))
  }
  it should "evaluate phi * phi" in {
    val phi = expr.Root(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val simplified = expression.simplify
    simplified.approximation().get.value === 2.61803398875
    simplified shouldBe Literal(Valuable(Algebraic_Quadratic(Quadratic(-3, 1), pos = true)))
  }
  it should "evaluate 1 / phi" in {
    val phi = expr.Root(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi.reciprocal
    val simplified = expression.simplify
    println(s"simplified = $simplified")
    simplified.approximation().get.value === 0.61803398875
    simplified shouldBe Literal(Valuable(Algebraic_Quadratic(Quadratic(1, -1), pos = true)))
  }
  it should "evaluate - phi" in {
    val phi = expr.Root(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi.negate
    val simplified = expression.simplify
    simplified.approximation().get.value === -1.61803398875
    val expected = Algebraic_Quadratic(Quadratic(1, -1), pos = false)
    val actual = simplified.asInstanceOf[QuadraticRoot].algebraic
    actual shouldBe expected
  }

  behavior of "Sum"
  it should "add pi to -pi" in {
    val x1 = ConstPi
    val x2 = ConstPi * MinusOne
    val e: Expression = x1 + x2
    val simplify = e.simplify
    simplify.materialize shouldBe Angle.zero
  }

  behavior of "simplifyComposite"
  it should "evaluate 1/2 * 2" in {
    import Expression.ExpressionOps
    val expression: Expression = Literal(half) * 2
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyComposite(x)
    y shouldBe em.Match(One)
  }
  it should "evaluate e * e" in {
    val expression: Expression = ConstE * ConstE
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyComposite(x)
    y shouldBe em.Match(BiFunction(ConstE, ValueExpression(2), Power))
    val simplified = y.get.simplify
    simplified shouldBe Literal(Valuable(core.Real(ExactNumber(2, NatLog))))
  }
  it should "evaluate phi * phi" in {
    val phi = expr.Root(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyComposite(x)
    y shouldBe em.Match(BiFunction(phi, ValueExpression(2), Power))
  }

  behavior of "math"
  it should "" in {
    // NOTE than when you explicitly specify "math," you don't need to add a type annotation of Expression
    val expression = math"√(4)"
    expression shouldBe BiFunction(ValueExpression(4), RationalNumber(Rational.half), Power)
  }
}
