/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression

import com.phasmidsoftware.number.core.ComplexPolar.±
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.algebraic.Quadratic.phiApprox
import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.inner.{NatLog, Radian, SquareRoot}
import com.phasmidsoftware.number.core.{Complex, ComplexCartesian, ComplexPolar, Constants, ExactNumber, Field, FuzzyEquality, GeneralNumber, Number, NumberException, Real}
import com.phasmidsoftware.number.expression
import com.phasmidsoftware.number.expression.Expression.{ExpressionOps, em, pi}
import com.phasmidsoftware.number.expression.Root.phi
import com.phasmidsoftware.number.mill.{Expr, Stack}
import com.phasmidsoftware.number.parse.ShuntingYardParser
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: GeneralNumber => new ExpressionOps(a).compare(Literal.apply(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "evaluate"
  it should "evaluate 1 + -1" in {
    val x: Expression = Expression.apply(1) + -1
    x.evaluateAsIs shouldBe Some.apply(Constants.zero)
  }
  it should "evaluate 1 * -1" in {
    val x: Expression = Expression.apply(1) * -1
    x.evaluateAsIs shouldBe Some.apply(Real.apply(-1))
  }
  it should "evaluate i * 2" in {
    val x: Expression = ConstI * 2
    val result: _root_.scala.Option[_root_.com.phasmidsoftware.number.core.Field] = x.evaluateAsIs
    result.isDefined shouldBe true
    val expected: Real = Real.apply(ExactNumber.apply(-4, SquareRoot))
    result.get shouldBe expected
  }

  behavior of "parse"
  private val syp: ShuntingYardParser.type = ShuntingYardParser
  it should "parse 1" in {
    syp.parseInfix("1") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix("(1)") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix(" (1)") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix(" (1 )") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix(" (1 )") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix("( 1 ) ") should matchPattern.apply { case Success(Stack(List(Expr(One)))) => }
    syp.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern.apply { case Success(_) => }
    syp.parseInfix("( ( 0.5 + 3.00* ) + ( 2.00* * ( 0.5 + 3.00* ) ) )") should matchPattern.apply { case Success(_) => }
  }
  it should "parse and evaluate sqrt(3)" in {
    val eo: Option[Expression] = Expression.parse("3 ∧ ( 2 ∧ -1 )")
    eo should matchPattern.apply { case Some(_) => }
    eo.get shouldBe BiFunction.apply(Expression.apply(3), BiFunction.apply(Expression.apply(2), Expression.apply(-1), Power), Power)
  }
  it should "parse and evaluate half" in {
    val eo: Option[Expression] = Expression.parse("2 ∧ -1")
    eo should matchPattern.apply { case Some(_) => }
    eo.get shouldBe BiFunction.apply(Expression.apply(2), Expression.apply(-1), Power)
  }

  // NOTE that the apply function always takes a Field and returns a Field. Not to be confused with applyExact.
  behavior of "ExpressionMonoFunction"
  it should "work for Negate" in {
    val f: ExpressionMonoFunction = Negate
    f.apply(Constants.zero) shouldBe Constants.zero
    f.apply(Constants.one) shouldBe Constants.minusOne
    f.apply(Constants.minusOne) shouldBe Constants.one
    f.apply(f.apply(Constants.two)) shouldBe Constants.two
  }
  it should "work for Reciprocal" in {
    val f: ExpressionMonoFunction = Reciprocal
    f.apply(Constants.zero) shouldBe Constants.infinity
    f.apply(Constants.one) shouldBe Constants.one
    f.apply(Constants.half) should ===(Constants.two)
    f.apply(Constants.two) shouldBe Constants.half
//    f(Constants.e) shouldBe Real(ExactNumber(-1, NatLog)) TODO fix this later
  }
  it should "work for Exp" in {
    val f: ExpressionMonoFunction = Exp
    f.apply(Constants.zero) shouldBe Constants.one
    f.apply(Constants.one) shouldBe Constants.e
  }
  it should "work for Ln" in {
    val f: ExpressionMonoFunction = Ln
    f.apply(Constants.one) shouldBe Constants.zero
    f.apply(Constants.e) shouldBe Constants.one
  }
  it should "work for Sine" in {
    val f: ExpressionMonoFunction = Sine
    f.apply(Constants.piBy2) shouldBe Constants.one
    f.apply(Constants.zero) shouldBe Constants.zero
  }
  it should "work for Cosine" in {
    val f: ExpressionMonoFunction = Cosine
    f.apply(Constants.piBy2) shouldBe Constants.zero
    f.apply(Constants.zero) shouldBe Constants.one
  }


  behavior of "evaluateAsIs for UniFunction"
  it should "work for Exp(1)" in {
    val x = expression.UniFunction.apply(One, Exp)
    val result = x.evaluateAsIs
    result shouldBe Some.apply(Constants.e)
  }
  it should "work for Reciprocal" in {
    expression.UniFunction.apply(Two, Reciprocal).evaluateAsIs shouldBe Some.apply(Constants.half)
  }
  it should "work for Ln(-1)" in {
    val x = expression.UniFunction.apply(MinusOne, Ln)
    val result = x.evaluateAsIs
    result shouldBe Some.apply(ComplexPolar.apply(Number.pi, Number.piBy2.makeNegative, 1))
  }

  behavior of "materialize UniFunction"
  it should "work for Exp(1)" in {
    val x = expression.UniFunction.apply(One, Exp)
    val result = x.materialize
    result shouldBe Constants.e
  }
  it should "work for Reciprocal" in {
    expression.UniFunction.apply(Two, Reciprocal).materialize shouldBe Constants.half
  }
  it should "work for Exp(Ln(2))" in {
    val x = expression.UniFunction.apply(expression.UniFunction.apply(Two, Ln), Exp)
    val result = x.materialize
    result shouldBe Constants.two
  }

  behavior of "Expression"
  it should "simplifyAndEvaluate" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction.apply(Literal.apply(x1), Literal.apply(x2), Sum)
    val result = e.materialize
    convertToNumber(result) shouldEqual Number.apply(Math.PI + 1)
  }
  it should "render" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction.apply(Literal.apply(x1), Literal.apply(x2), Sum)
    // TODO let's make the representations of pi consistent. This was previously 𝛑
    e.toString shouldBe "BiFunction{1 + π}"
    e.render shouldBe "4.1415926535897930(41)"
    e.materialize.render shouldBe "4.1415926535897930(41)"
  }
  it should "evaluate 3 5 + 7 2 – *" in {
    val expression = (Expression.apply(3) + 5) * (7 - 2)
    val result = expression.simplify.materialize
    result shouldEqual Real.apply(40)
  }

  behavior of "ExpressionOps"
  it should "evaluate +" in {
    val x: Expression = Expression.apply(1) + 2
    val y: Number = x
    y shouldEqual Number.apply(3)
  }
  it should "evaluate -" in {
    val x = Expression.apply(1) - 2
    val result = x.simplify.materialize.asNumber
    result shouldEqual Some.apply(Number.apply(-1))
  }
  it should "evaluate *" in {
    val x = Expression.apply(3) * 2
    x shouldEqual Number.apply(6)
  }
  it should "evaluate /" in {
    val x = Expression.apply(6) / 2
    x shouldEqual Number.apply(3)
  }
  it should "evaluate ∧ 2" in {
    val x = Expression.apply(6) ∧ 2
    x shouldEqual Number.apply(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = Expression.apply(36).sqrt
    x.materialize.normalize shouldEqual ±(6)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = ConstPi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Constants.one
  }
  it should "evaluate atan" in {
    Zero.atan(One).materialize.asNumber shouldBe Some.apply(Number.piBy2)
    Zero.atan(1).materialize.asNumber shouldBe Some.apply(Number.piBy2)
    One.atan(0).materialize.asNumber shouldBe Some.apply(Number.apply(0, Radian))
    Number.one.atan(Number.zero) shouldBe Number.apply(0, Radian)
    One.atan(Constants.root3).evaluateAsIs shouldBe Some.apply(Constants.piBy3)
    One.atan(One).evaluateAsIs shouldBe Some.apply(Constants.piBy4)
  }
  it should "evaluate log 2" in {
    val base = Two
    One.log(base).materialize.asNumber shouldBe Some.apply(Number.zero)
    Two.log(base).materialize.asNumber shouldBe Some.apply(Number.one)
//    Expression(4).log(base).materialize.asNumber shouldBe Some(Number.two)
  }
  it should "evaluate log e" in {
    val base = ConstE
    One.log(base).materialize.asNumber shouldBe Some.apply(Number.zero)
    ConstE.log(base).materialize.asNumber shouldBe Some.apply(Number.one)
  }
  it should "evaluate log 10" in {
    val base = Expression.apply(10)
    One.log(base).materialize.asNumber shouldBe Some.apply(Number.zero)
    Expression.apply(10).log(base).materialize.asNumber shouldBe Some.apply(Number.one)
  }
  it should "fail to evaluate log 1 x or log 0 x" in {
    val base = Expression.apply(1)
    a[NumberException] should be thrownBy Two.log(base).materialize.asNumber
  }
  it should "evaluate ln E" in {
    val x: Expression = ConstE
    val y: Expression = x.ln
    y.materialize shouldBe Constants.one
  }
  it should "evaluate ln 2E" in {
    val x: Expression = ConstE * 2
    val y: Expression = x.ln
    val result = y.materialize
    val expected = Real.apply("1.693147180559945(13)")
    result shouldEqual expected
  }
  it should "evaluate xxx" in {
    val x: Expression = ConstE.log(Two) // lg E with value close to √2
    val y: Expression = x.reciprocal.simplify
    val result = y.materialize
    val expected = Real.apply("0.6931471805599453(13)")
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
    result.toString shouldBe "BiFunction{√7 ∧ 2}"
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    val z: Field = (ConstE * 2).materialize
    val q = convertToNumber(z).normalize
    q.toString shouldBe "5.436563656918091[15]"
  }

  behavior of "isExact"
  it should "be true for any constant Number" in {
    Number.one.isExact shouldBe true
    Number.pi.isExact shouldBe true
  }
  it should "be true for any sum of exact Numbers of the same factor (not e)" in {
    (One + Constants.two).isExact shouldBe true
    (ConstPi + Constants.pi).isExact shouldBe true
  }
  it should "be false for any product of exact Numbers and a NatLog factor (except for one)" in {
    (Expression.apply(2) * Constants.e).isExact shouldBe false
  }
  it should "be true for product of one exact Numbers and a NatLog factor" in {
    val expression = Expression.apply(1) * Constants.e
    expression.isExact shouldBe true
  }
  it should "be true for product of zero exact Numbers and a NatLog factor" in {
    (Expression.apply(0) * Constants.e).isExact shouldBe true
  }

  behavior of "depth"
  it should "be 1 for any atomic expression" in {
    Expression.apply(1).depth shouldBe 1
    Expression.one.depth shouldBe 1
    Expression.apply(1).depth shouldBe 1
    pi.depth shouldBe 1
  }
  it should "be 2 for any UniFunction expression" in {
    expression.UniFunction.apply(1, Negate).depth shouldBe 2
    expression.UniFunction.apply(1, Cosine).depth shouldBe 2
    expression.UniFunction.apply(1, Sine).depth shouldBe 2
  }
  it should "be more than 1 for other expression" in {
    (ConstE * 2).depth shouldBe 2
    (ConstE * 2 / 2).depth shouldBe 3
    val expression = Expression.apply(7).sqrt ∧ 2
    expression.depth shouldBe 2
  }

  behavior of "Euler"
  it should "prove Euler's identity 1" in {
    val iPi = ComplexCartesian.apply(0, Number.pi)
    val euler: Expression = Expression.apply(Constants.e) ∧ iPi
    euler.materialize shouldBe Constants.minusOne
  }
  it should "prove Euler's identity 2" in {
    val iPi = Complex.convertToPolar(ComplexCartesian.apply(0, Number.pi))
    val euler: Expression = Expression.apply(Constants.e) ∧ iPi
    euler.materialize shouldBe Constants.minusOne
  }

  behavior of "FieldExpression"
  it should "Zero be equal to zero" in {
    val target = Literal.apply(Constants.zero)
    target shouldBe Zero
    target should matchPattern.apply { case Literal(Constants.zero, _) => }
    target should matchPattern.apply { case FieldExpression(Constants.zero, _) => }
  }
  it should "One be equal to one" in {
    val target = Literal.apply(Constants.one)
    target shouldBe One
    target should matchPattern.apply { case Literal(Constants.one, _) => }
    target should matchPattern.apply { case FieldExpression(Constants.one, _) => }
  }

  behavior of "simplifyConstant"
  it should "simplify biFunction expressions" in {
    val em: ExpressionMatchers = Expression.em
    Expression.simplifyConstant.apply(BiFunction.apply(Two, MinusOne, Product)) shouldBe em.Match.apply(Expression.apply(-2))
    BiFunction.apply(Two, MinusOne, Product).simplify shouldBe Expression.apply(-2)
    BiFunction.apply(BiFunction.apply(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some.apply(Constants.zero)
  }

  behavior of "simplify"
  it should "simplify field expressions" in {
    Expression.apply(1).simplify shouldBe Expression.apply(1)
    ConstPi.simplify shouldBe ConstPi
    val simplify = phi.simplify
    simplify shouldBe Expression.apply(Algebraic.phi)
    phi.simplify.materialize should ===(phiApprox)
  }
  it should "simplify function expressions" in {
    expression.UniFunction.apply(expression.UniFunction.apply(One, Negate), Negate).simplify shouldBe One
    expression.UniFunction.apply(Two, Reciprocal).simplify shouldBe Expression.apply(Constants.half)
    expression.UniFunction.apply(Constants.pi, Sine).simplify shouldBe Expression.apply(Constants.zero)
  }
  it should "simplify biFunction expressions" in {
    BiFunction.apply(BiFunction.apply(Two, MinusOne, Product), Two, Sum).simplify shouldBe Zero
    BiFunction.apply(BiFunction.apply(Two, MinusOne, Product), Two, Sum).evaluateAsIs shouldBe Some.apply(Constants.zero)
  }

  // XXX appears to have fixed infinite recursion
  it should "simplify aggregate expressions" in {
    Aggregate.total(BiFunction.apply(Two, MinusOne, Product), Two).simplify shouldBe Zero
    Aggregate.total(Two).simplify shouldBe Two
    Aggregate.empty(Sum).simplify shouldBe Zero
  }
  it should "aggregate 2" in {
    val target = (One * ConstPi * Two * MinusOne).simplify
    target shouldBe Expression.apply(-2 * Constants.pi)
  }
  it should "evaluate e * e" in {
    val expression: Expression = ConstE * ConstE
    expression.simplify shouldBe Literal.apply(Real.apply(ExactNumber.apply(2, NatLog)))
  }
  it should "evaluate phi * phi" in {
    val phi = Root.apply(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val simplified = expression.simplify
    simplified.approximation.get.toDouble === 2.61803398875
    simplified shouldBe Literal.apply(Algebraic_Quadratic.apply(Quadratic.apply(-3, 1), pos = true))
  }
  it should "evaluate 1 / phi" in {
    val phi = Root.apply(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi.reciprocal
    val simplified = expression.simplify
    println(s"simplified = $simplified")
    simplified.approximation.get.toDouble === 0.61803398875
    simplified shouldBe Literal.apply(Algebraic_Quadratic.apply(Quadratic.apply(1, -1), pos = true))
  }
  it should "evaluate - phi" in {
    val phi = Root.apply(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi.negate
    val simplified = expression.simplify
    simplified.approximation.get.toDouble === -1.61803398875
    val expected = Algebraic_Quadratic.apply(Quadratic.apply(1, -1), pos = false)
    val actual = simplified.asInstanceOf[QuadraticRoot].algebraic
    actual shouldBe expected
  }

  behavior of "Sum"
  it should "add pi to -pi" in {
    val x1 = ConstPi
    val x2 = ConstPi * MinusOne
    val e: Expression = x1 + x2
    val simplify = e.simplify
    simplify.materialize.asNumber shouldBe Some.apply(Number.zeroR)
  }

  behavior of "simplifyComposite"
  it should "evaluate e * e" in {
    val expression: Expression = ConstE * ConstE
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyComposite.apply(x)
    y shouldBe em.Match.apply(BiFunction.apply(ConstE, Literal.apply(2), Power))
    val simplified = y.get.simplify
    simplified shouldBe Literal.apply(Real.apply(ExactNumber.apply(2, NatLog)))
  }
  it should "evaluate phi * phi" in {
    val phi = Root.apply(Quadratic.goldenRatioEquation, 0)
    val expression: Expression = phi * phi
    val x: CompositeExpression = expression.asInstanceOf[CompositeExpression]
    val y: em.MatchResult[Expression] = x.simplifyComposite.apply(x)
    y shouldBe em.Match.apply(BiFunction.apply(phi, Literal.apply(2), Power))
  }
}
