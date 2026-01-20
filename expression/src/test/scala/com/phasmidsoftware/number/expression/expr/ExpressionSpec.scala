/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.FuzzyEq
import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.algebra.eager.Eager.eagerToField
import com.phasmidsoftware.number.algebra.eager.RationalNumber.half
import com.phasmidsoftware.number.core.inner.{NatLog, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexPolar, ExactNumber}
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import com.phasmidsoftware.number.expression.core.FuzzyEquality
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, em, pi}
import com.phasmidsoftware.number.expression.expr.Root.phi
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

  type OldExpression = com.phasmidsoftware.number.expression.expr.Expression
  type OldNumber = com.phasmidsoftware.number.core.numerical.Number

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean =
      b match {
        case v: Eager =>
          a.compare(Literal(v)) == 0
        case n: numerical.Number =>
          new ExpressionOps(a).compare(Literal(n)) == 0
        case _ =>
          false
      }
  }

  behavior of "evaluate"
  it should "evaluate 1 + -1" in {
    val x: Expression = Expression(1) :+ -1
    x.evaluateAsIs shouldBe Some(Eager.zero)
  }
  it should "evaluate 1 * -1" in {
    val x: Expression = Expression(1) * -1
    x.evaluateAsIs shouldBe Some(WholeNumber(-1))
  }
  // TODO #Issue 149 this has to do with imaginary numbers
  it should "evaluate i * 2" in {
//    val x: Expression = ConstI * 2
//    val result: Option[Valuable] = x.evaluateAsIs
//    result.isDefined shouldBe true
//    val expected = Eager(numerical.Real(ExactNumber(-4, SquareRoot)))
//    result.get shouldBe expected
    pending
  }

  behavior of "parse"
//  private val syp: ShuntingYardParser.type = ShuntingYardParser
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
    f(Eager.zero) shouldBe Eager.zero
    f(Eager.one) shouldBe Eager.minusOne
    f(Eager.minusOne) shouldBe Eager.one
    f(f(Eager.two)) shouldBe Eager.two
  }
  it should "work for Reciprocal" in {
    val f: ExpressionMonoFunction = Reciprocal
    f(Eager.zero) shouldBe Eager.infinity
    f(Eager.one) shouldBe Eager.one
    f(Eager.half) should ===(Eager.two)
    f(Eager.two) shouldBe Eager.half
    //    f(Eager.e) shouldBe Real(ExactNumber(-1, NatLog)) TODO fix this later
  }
  it should "work for Exp" in {
    val f: ExpressionMonoFunction = Exp
    f(Eager.zero) shouldBe Eager.one
    f(Eager.one) shouldBe Eager.e
  }
  it should "work for Ln" in {
    val f: ExpressionMonoFunction = Ln
    f(Eager.one) shouldBe Eager.zero
    f(Eager.e) shouldBe Eager.one
  }
  it should "work for Sine" in {
    val f: ExpressionMonoFunction = Sine
    f(Angle.piBy2) shouldBe Eager.one
    f(Angle.zero) shouldBe Eager.zero
  }
  it should "work for Cosine" in {
    val f: ExpressionMonoFunction = Cosine
    f(Angle.piBy2) shouldBe Eager.zero
    f(Angle.zero) shouldBe Eager.one
  }


  behavior of "evaluateAsIs for UniFunction"
  it should "work for Exp(1)" in {
    val x = expr.UniFunction(One, Exp)
    val result = x.evaluateAsIs
    result shouldBe Some(Eager.e)
  }
  it should "work for Reciprocal" in {
    expr.UniFunction(Two, Reciprocal).evaluateAsIs shouldBe Some(Eager.half)
  }
  it should "work for Ln(-1)" in {
    val x = expr.UniFunction(MinusOne, Ln)
    val result = x.evaluateAsIs
    result shouldBe Some(Complex(ComplexPolar(numerical.Number.pi, numerical.Number.piBy2.makeNegative, 1)))
  }

  behavior of "materialize UniFunction"
  it should "work for Exp(1)" in {
    val x = expr.UniFunction(One, Exp)
    val result = x.materialize
    result shouldBe Eager.e
  }
  it should "work for Reciprocal" in {
    expr.UniFunction(Two, Reciprocal).materialize shouldBe Eager.half
  }
  it should "work for Exp(Ln(2))" in {
    val x = expr.UniFunction(expr.UniFunction(Two, Ln), Exp)
    val result = x.materialize
    result shouldBe Eager.two
  }

  behavior of "Expression"
  it should "simplifyAndEvaluate" in {
    val x1 = Eager.one
    val x2 = Eager.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    val result: Eager = e.materialize
    eagerToField(result) shouldEqual numerical.Real(numerical.Number(Math.PI + 1))
  }
  it should "render" in {
    val x1 = Eager.one
    val x2 = Eager.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    e.toString shouldBe """(1 + 𝛑)"""
    e.render shouldBe "(1 + 𝛑)"
    e.materialize.render shouldBe "4.1415926535897930(67)"
  }
  it should "evaluate 3 5 + 7 2 – *" in {
    val expression = (Expression(3) :+ 5) * (7 - 2)
    val result = expression.simplify.materialize
    result shouldEqual WholeNumber(40)
  }

  behavior of "ExpressionOps"
  it should "evaluate +" in {
    val x: Expression = Expression(1) :+ 2
    x shouldBe BiFunction(Expression(1), Expression(2), Sum)
  }
  it should "evaluate -" in {
    val x = Expression(1) - 2
    val simplifiedExpression = x.simplify
    val result = simplifiedExpression.materialize
    result shouldBe Eager.minusOne
  }
  it should "evaluate *" in {
    val x = Expression(3) * 2
    x.materialize shouldEqual Eager(6)
  }
  // TODO Issue #140
  it should "evaluate /" in {
    // CONSIDER should this be Valuable(3)?
    val x = Expression(6) / 2
    x.materialize shouldEqual RationalNumber(3)
  }
  // TODO Issue #140
  it should "evaluate ∧ 2" in {
    val x = Expression(6) ∧ 2
    x.materialize shouldEqual Eager(36)
  }
  // TODO Issue #150
  it should "evaluate sqrt 36" in {
//    val x: Expression = Expression(36).sqrt
//    x.materialize shouldEqual ±(6)
    pending
  }
  // TODO Issue #140
  it should "evaluate sin pi/2" in {
    val x: Expression = Pi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Eager.one
  }
  // FIXME stack overflow
  it should "evaluate atan" in {
    val zero: Expression = expr.Zero
    zero.atan(Eager.one).materialize shouldBe Angle.piBy2
    One.atan(0).materialize shouldBe Angle.zero
    One.atan(Eager.root3).evaluateAsIs shouldBe Some(Angle.piBy3)
    One.atan(One).evaluateAsIs shouldBe Some(Eager.piBy4)
  }
  it should "evaluate log 2" in {
    val base = Two
    One.log(base).materialize shouldBe Eager.zero
    Two.log(base).materialize shouldBe Eager.one
    //    Expression(4).log(base).materialize.asCoreNumber shouldBe Some(Number.two)
  }
  it should "evaluate log e" in {
    val base = E
    One.log(base).materialize shouldBe Eager.zero
    E.log(base).materialize shouldBe Eager.one
  }
  it should "evaluate log 10" in {
    val base = Expression(10)
    One.log(base).materialize shouldBe Eager.zero
    Expression(10).log(base).materialize shouldBe Eager.one
  }
  it should "fail to evaluate log 1 x or log 0 x" in {
    val base = Expression(1)
    a[numerical.CoreException] should be thrownBy Two.log(base).materialize
  }
  // TODO Issue #140
  it should "evaluate ln E" in {
    val x: Expression = E
    val y: Expression = x.ln
    y.materialize shouldBe Eager.one
  }
  // TODO Issue #140
  it should "evaluate ln 2E" in {
    val x: Expression = E * 2
    val y: Expression = x.ln
    val result = y.materialize
    val expected = Real("1.693147180559945(13)")
    result === expected
  }
  // TODO Issue #140
  it should "evaluate xxx" in {
    val x: Expression = E.log(Two) // lg E with value close to √2
    val y: Expression = x.reciprocal.simplify
    val result: Eager = y.materialize
    val expected: Eager = eager.Real("0.6931471805599453(13)")
    result should ===(expected)
  }
  // (fixed) we should be able to compare y with L2 (this tests for Issue #125)
  it should "evaluate xxx 2" in {
    val x: Expression = E.log(Two) // lg E with value close to √2
    val y: Expression = x.reciprocal.simplify
    val z = L2.simplify
    y === z
  }

  behavior of "toString"
  // TODO Issue #140
  it should "work for (sqrt 7)∧2" in {
    val seven: Expression = 7
    val result: Expression = seven.sqrt ∧ 2
    result.toString shouldBe BiFunction(BiFunction(WholeNumber(7), RationalNumber(Rational.half), Power), 2, Power).toString
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    val z: Eager = (E * 2).materialize
    val q = eagerToField(z).normalize
    q.toString shouldBe "5.436563656918090[51]"
  }

  behavior of "isExact"
  // TODO Issue #140
  it should "be true for any constant Valuable" in {
    Eager.one.isExact shouldBe true
    Eager.pi.isExact shouldBe true
  }
  it should "be true for any sum of exact Numbers of the same factor (not e)" in {
    (One :+ Eager.two).isExact shouldBe true
    (Pi :+ Eager.pi).isExact shouldBe true
  }
  it should "be false for any product of exact Numbers and a NatLog factor (except for one)" in {
    (Expression(2) * Eager.e).materialize.isExact shouldBe false
    (Expression(1) * Eager.e).materialize.isExact shouldBe true
    (Expression(2) * Eager.one).materialize.isExact shouldBe true
  }
  it should "be true for product of one exact Numbers and a NatLog factor" in {
    val expression = Expression(1) * Eager.e
    expression.isExact shouldBe true
  }
  it should "be true for product of zero exact Numbers and a NatLog factor" in {
    (Expression(0) * Eager.e).isExact shouldBe true
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
    (E * 2).depth shouldBe 2
    (E * 2 / 2).depth shouldBe 3
    (Expression(7).sqrt ∧ 2).depth shouldBe 3
  }

  behavior of "Euler"
  // TODO Issue #151
  it should "prove Euler's identity 1" in {
//    val iPi = ComplexCartesian(0, numerical.Number.pi)
//    val euler: Expression = Expression(Eager.e) ∧ Complex(iPi)
//    euler.materialize shouldBe Eager.minusOne
    pending
  }
  // TODO Issue #151
  it should "prove Euler's identity 2" in {
//    val iPi = numerical.Complex.convertToPolar(ComplexCartesian(0, numerical.Number.pi))
//    val euler: Expression = Expression(Eager.e) ∧ Complex(iPi)
//    euler.materialize shouldBe Eager.minusOne
    pending
  }

  behavior of "FieldExpression"
  it should "Zero be equal to zero" in {
    val target = Literal(Eager.zero)
    target shouldBe Zero
    target should matchPattern { case Literal(Eager.zero, _) => }
    target should matchPattern { case ValueExpression(Eager.zero, _) => }
  }
  it should "One be equal to one" in {
    val target = Literal(Eager.one)
    target shouldBe One
    target should matchPattern { case Literal(Eager.one, _) => }
    target should matchPattern { case ValueExpression(Eager.one, _) => }
  }

  behavior of "simplifyConstant"
  // TODO Issue #140
  it should "simplify biFunction expressions" in {
    val em: ExpressionMatchers = Expression.em
    Expression.simplifyConstant(BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Expression(-2))
    BiFunction(Two, MinusOne, Product).simplify shouldBe Expression(-2)
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some(Eager.zero)
  }

  behavior of "simplifyExact"
  // TODO Issue #140
  it should "simplify biFunction expressions" in {
    val em: ExpressionMatchers = Expression.em
    Expression.simplifyExact(BiFunction(Literal(Angle.twoPi, None), RationalNumber.half, Product)) should matchPattern { case em.Match(Literal(Angle.pi, _)) => }
  }

  behavior of "simplify"

  // XXX oops! This was working before the latest change
  it should "simplify field expressions" in {
    Expression(1).simplify shouldBe Expression(1)
    Pi.simplify shouldBe Pi
    val simplify: Expression = phi.simplify
    val expected = Literal(QuadraticSolution.phi)
    (expected, simplify) match {
      case (Literal(x: QuadraticSolution, _), Literal(y: QuadraticSolution, _)) =>
        x.eqv(y).get shouldBe true
      case _ => fail(s"expected $expected, got $simplify")
    }
    extension (x: Eager)
      infix def ~==(y: Eager): Boolean =
        FuzzyEq[Eager].eqv(x, y, 0.5)

    val eager = simplify.normalize.materialize
    (eager ~== Eager.phi) shouldBe true
  }
  it should "simplify constant expressions" in {
    Expression(1).simplify shouldBe Expression(1)
    Pi.simplify shouldBe Pi
  }
  it should "simplify unary expressions" in {
    expr.UniFunction(One, Negate).simplify shouldBe MinusOne
    expr.UniFunction(Two, Reciprocal).simplify shouldBe Half
  }
  it should "simplify binary expressions" in {
    BiFunction(Two, MinusOne, Product).simplify shouldBe Expression(-2)
  }
  it should "simplify function expressions" in {
    expr.UniFunction(expr.UniFunction(One, Negate), Negate).simplify shouldBe One
    expr.UniFunction(Two, Reciprocal).simplify shouldBe Expression(Eager.half)
    expr.UniFunction(Eager.pi, Sine).simplify shouldBe Expression(Eager.zero)
  }
  it should "simplify biFunction expressions" in {
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).simplify shouldBe Zero
    BiFunction(BiFunction(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some(Eager.zero)
  }

  // XXX appears to have fixed infinite recursion
  it should "simplify aggregate expressions" in {
    Aggregate.total(BiFunction(Two, MinusOne, Product), Two).simplify shouldBe Zero
    Aggregate.total(Two).simplify shouldBe Two
    Aggregate.empty(Sum).simplify shouldBe Zero
  }
  it should "aggregate 2" in {
    val target = (One * Pi * Two * MinusOne).simplify
    // CONSIDER is this correct? It doesn't seem right.
    val expected = Literal(Angle(WholeNumber(-2)), Some("0𝛑"))
    target shouldBe expected
  }
  it should "evaluate e * e" in {
    val expression: Expression = E * E
    expression.simplify shouldBe Literal(Eager(numerical.Real(ExactNumber(2, NatLog))))
  }
  // TODO Issue #140
  it should "evaluate phi * phi" in {
    val phi = expr.Root(QuadraticEquation.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val simplified = expression.simplify
    simplified.approximation().get.value === 2.61803398875
  }
  // TODO Issue #140
  it should "evaluate 1 / phi" in {
    val phi = expr.Root(QuadraticEquation.goldenRatioEquation, 0)
    val expression: Expression = phi.reciprocal
    val simplified = expression.simplify
    println(s"simplified = $simplified")
    simplified.approximation().get.value === 0.61803398875
  }
  it should "evaluate - phi" in {
    val phi: Root = expr.Root(QuadraticEquation.goldenRatioEquation, 0)
    val expression: Expression = phi.negate
    val simplified = expression.simplify
    simplified.approximation().get.value === -1.61803398875
    simplified.materialize shouldBe QuadraticEquation(1, -1).solve(1)
  }

  behavior of "Sum"
  it should "add pi to -pi" in {
    val x1 = Pi
    val x2 = Pi * MinusOne
    val e: Expression = x1 :+ x2
    val simplify = e.simplify
    simplify.materialize shouldBe Angle.zero
  }

  behavior of "simplifyStructural"
  it should "evaluate 1/2 * 2" in {
    import Expression.ExpressionOps
    val expression: Expression = Literal(half) * 2
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyStructural(x)
    y shouldBe em.Match(One)
  }
  it should "evaluate e * e" in {
    val expression: Expression = E * E
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyStructural(x)
    y shouldBe em.Match(BiFunction(E, ValueExpression(2), Power))
    val simplified = y.get.simplify
    simplified shouldBe Literal(Eager(numerical.Real(ExactNumber(2, NatLog))))
  }
  it should "evaluate phi * phi" in {
    val phi = expr.Root(QuadraticEquation.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyStructural(x)
    y shouldBe em.Match(BiFunction(phi, ValueExpression(2), Power))
  }

  behavior of "Expression.apply(String)"
  it should "" in {
    // NOTE than when you explicitly specify "math," you don't need to add a type annotation of Expression
    val expression = Expression("√(4)")
    expression shouldBe BiFunction(ValueExpression(4), RationalNumber(Rational.half), Power)
  }

  behavior of "Expression worksheet"
  it should "work" in {

    val six = ∅ + 1 + 2 + 3
    six.materialize.render shouldBe "6"

    val fortyTwo = ∅ * 6 * 7
    fortyTwo.materialize.render shouldBe "42"

    val root3 = Expression(3).sqrt

    val root3PlusOne: Expression = root3 + Expression.one
    val root3MinusOne = root3 + Expression(-1)

    // Note the use of an implicit converter from Expression to Number.
    val x: Eager = root3PlusOne.materialize
    val y: Eager = root3MinusOne.materialize

    (x ~= Real(2.732050807568877)) shouldBe true
    (y ~= Real(0.732050807568877)) shouldBe true

    // Two should be exactly 2
    val two: Eager = (root3PlusOne * root3MinusOne).materialize
    two shouldBe WholeNumber.two

    val half: Expression = One / 2
    // This should be rendered as ½
    half.materialize shouldBe RationalNumber.half

  }
}
