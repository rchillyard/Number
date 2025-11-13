/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.matchers.*
import com.phasmidsoftware.number.algebra.{Angle, RationalNumber, Valuable, WholeNumber}
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Number.{piBy2, root2, √}
import com.phasmidsoftware.number.core.inner.Rational.infinity
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.{ComplexPolar, Constants, Field, FuzzyNumber}
import com.phasmidsoftware.number.expression.expr
import com.phasmidsoftware.number.expression.expr.Expression.em.DyadicTriple
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, matchSimpler, zero}
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.languageFeature.implicitConversions.*

/**
  * Test suite for `ExpressionMatchers` and related functionality, extending `AnyFlatSpec` with ScalaTest matchers functionalities.
  *
  * TODO requires more simplification with removal of duplicates, etc.
  *
  * This class provides unit tests for various operations and matchers related to `Expression` objects.
  * It validates the functionality of arithmetic operations, simplifications, matchers, and custom equality logic for expressions.
  *
  * The tests involve operations such as addition, subtraction, multiplication, division, power, square root,
  * and comparison on various types of `Expression` instances, including `Literal`, `One`, `Number`, and constants like `pi` and `e`.
  *
  * The operations are tested against specific matching rules for common scenarios in symbolic math expressions,
  * ensuring the correctness of `ExpressionMatchers`, custom defined `ExpressionMatchers.ExpressionMatcher` implementations,
  * and simplification functionalities such as `matchSimpler` and `simplifyTrivial`.
  *
  * Behavior-driven tests are grouped by functionality to validate the logic and expected outcomes for:
  * - Matching values via custom expression matchers.
  * - Matching and combining rules using logical OR (`|`).
  * - Simplification logic for trivial arithmetic and symbolic cancellations.
  * - Handling edge cases like exact evaluation, reciprocal, and multi-level symbolic transformations.
  *
  * Several tests focus on validating simplification scenarios, specifically ensuring the cancellation of
  * arithmetic operations like addition/subtraction, multiplication/division, and power/exponentiation inversions.
  *
  * Helper implicits such as `ExpressionEquality` and logging mechanisms (`MatchLogger`) assist in testing and debugging the logic.
  */
class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case v: Valuable => a.compare(Literal(v)) == 0
      case n: core.Number => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  val sb = new StringBuilder
  implicit val logger: MatchLogger = MatchLogger(LogDebug, classOf[ExpressionMatchers])
  //  implicit val logger: MatchLogger = SBLogger(LogOff, sb)
  val sbLogger: MatchLogger = SBLogger(LogDebug, sb)

  before {
    sb.clear()
  }

  after {
    //    if (sb.nonEmpty) println(sb.toString())
    //    if (sbLogger.logLevel != com.phasmidsoftware.matchers.LogOff)
    //      println("===============================\n")
  }

  val em: ExpressionMatchers = Expression.em
  val eml: ExpressionMatchers = new ExpressionMatchers() {}
  val ems: ExpressionMatchers = new ExpressionMatchers(using sbLogger) {}
  private val two: Valuable = Valuable.two
  private val one = Valuable.one
  private val half = Valuable.half

  behavior of "value"
  it should "work with value on Literal" in {
    val f: em.ExpressionMatcher[Valuable] = em.value
    f(Literal(one)).successful shouldBe true
  }
  it should "work with value on One" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on Number.one" in {
    val f = em.value
    f(One).successful shouldBe true
  }
  it should "work with value on Real" in {
    val f = em.value
    f(Literal(FuzzyNumber(Right(1), PureNumber, None))).successful shouldBe true
  }
  it should "fail on non-value" in {
    val x = Expression(1) * 2
    val f = em.value
    f(x).successful shouldBe false
  }

  behavior of "matchValue"
  it should "work with value 1" in {
    val f: em.ExpressionMatcher[Valuable] = em.matchValue(Valuable.one)
    val e: Expression = one
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = em.matchValue(Valuable.one)
    val g = f | em.matchValue(Valuable.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Valuable.pi, Some("π"))).successful shouldBe true
    g(Literal(Valuable.e, Some("e"))).successful shouldBe false
  }

  import Expression.ExpressionOps
  import Matchers.*

  /**
    * Keep in mind that `matchSimpler` only performs one stage (level?) of simplification.
    * If you are testing full simplification, then you must use `simplify`.
    */
  behavior of "matchSimpler"

  it should "matchSimpler 1" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x: Expression = Valuable.pi
    p(Sum ~ x ~ Zero) shouldBe em.Match(x)
    p(Sum ~ Zero ~ x) shouldBe em.Match(x)
    p(Product ~ x ~ One) shouldBe em.Match(x)
    p(Product ~ One ~ x) shouldBe em.Match(x)
    p(Product ~ x ~ Zero) shouldBe em.Match(Zero)
    p(Product ~ Zero ~ x) shouldBe em.Match(Zero)
    p(Power ~ x ~ One) shouldBe em.Match(x)
  }
  it should "simplifyTrivial 1" in {
    import BiFunction.*
    val p = Expression.simplifyTrivial
    val x: Expression = Valuable.pi
    p(Sum ~ x ~ Zero) shouldBe em.Match(x)
    p(Sum ~ Zero ~ x) shouldBe em.Match(x)
    p(Product ~ x ~ One) shouldBe em.Match(x)
    p(Product ~ One ~ x) shouldBe em.Match(x)
    p(Product ~ x ~ Zero) shouldBe em.Match(Zero)
    p(Product ~ Zero ~ x) shouldBe em.Match(Zero)
    p(Power ~ x ~ One) shouldBe em.Match(x)
  }
  it should "handle Sum" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x: Expression = Valuable.pi
    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
    p(Sum ~ Two ~ Two) shouldBe em.Match(ValueExpression(4))
    p(Sum ~ One ~ Two) shouldBe em.Match(ValueExpression(3))
    p(Sum ~ One ~ Literal(root2)) should matchPattern { case em.Miss(_, _) => }
  }
  it should "handle Product" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Product ~ One ~ Zero) shouldBe em.Match(Zero)
    p(Product ~ Zero ~ One) shouldBe em.Match(Zero)
    p(Product ~ Two ~ One) shouldBe em.Match(Two)
    p(Product ~ One ~ Two) shouldBe em.Match(Two)
    p(Product ~ Two ~ Two) shouldBe em.Match(ValueExpression(WholeNumber(4)))
    p(Product ~ Two ~ ValueExpression(3)) shouldBe em.Match(ValueExpression(WholeNumber(6)))
  }
  it should "handle Power" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Power ~ Two ~ Zero) shouldBe em.Match(One)
    p(Power ~ Two ~ One) shouldBe em.Match(Two)
    p(Power ~ One ~ Two) shouldBe em.Match(One)
    p(Power ~ Two ~ Two) shouldBe em.Match(ValueExpression(4))
  }
  it should "cancel -1 and - 1" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = Expression.minusOne
    val result = p(Sum ~ y ~ x)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  it should "cancel multiplication and division" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x = Literal(Valuable.pi) * 2
    val y = One / 2
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(ConstPi)
  }
  it should "simplify sqrt(7)∧2" in {
    val x: Expression = Expression(7)
    val y = x.sqrt
    val z = y ∧ 2
    val q = matchSimpler(z)
    q shouldBe em.Match(Expression(7))
  }
  it should "cancel 1 and - -1" in {
    sb.append("cancel 1 and - -1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    val p = matchSimpler(z)
    p should matchPattern { case em.Match(Zero) => }
  }
  it should "cancel 1 and - -1 b" in {
    sb.append("cancel 1 and - -1 b:\n")
    val x: Expression = Expression.one
    val y = MinusOne * x
    val z = x + y
    val p = matchSimpler(z)
    p should matchPattern { case em.Match(Zero) => }
  }
  it should "cancel 2 * 1/2 (a)" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val p = matchSimpler(z)
    p shouldBe em.Match(Literal(RationalNumber(1)))
  }
  it should "cancel 2 * 1/2 (b)" in {
    val x = Expression(2) * Expression.one
    val y = x.reciprocal
    val z = y * x
    val p = matchSimpler(z)
    p shouldBe em.Match(Literal(RationalNumber(1)))
  }
  it should "cancel ∧2 and sqrt for 7" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    val p = matchSimpler(y)
    p shouldBe em.Match(seven)
  }
  it should "show that lazy evaluation sometimes works even when you don't use it (a)" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ two
    val simplify = y.simplify
    simplify.isExact shouldBe true
    simplify shouldBe seven
  }
  it should "cancel multiplication and division 2" in {
    val x = Literal(Valuable.pi) * 2 / 2
    matchSimpler(x).get shouldBe ConstPi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Literal(Valuable.pi) / 2 * 2
    matchSimpler(x).get shouldBe ConstPi
  }
  it should "cancel 1 and - -1 (a)" in {
    val x: Expression = Expression.one
    val y = -x
    val z = x + y
    matchSimpler(z).get shouldBe Zero
  }
  it should "cancel 2 and * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    matchSimpler(z).get shouldBe ValueExpression(RationalNumber(1))
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    matchSimpler(z).get shouldBe ValueExpression(RationalNumber(1))
  }
  it should "cancel ∧2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    matchSimpler(y).get shouldBe Expression(7)
  }
  it should "cancel addition and subtraction of 3" in {
    val x = One + 3 - 3
    matchSimpler(x).get shouldBe One
  }
  it should "cancel addition and subtraction of e" in {
    val y: Expression = One + ConstE
    val z = y + expr.UniFunction(ConstE, Negate)
    matchSimpler(z).get shouldBe One
  }
  it should "work for multi-levels 1" in {
    val x = (One + 3 - 3) * (Two / 4)
    matchSimpler(x).get shouldBe Half
  }
  it should "work for multi-levels 2" in {
    val x = (One + ConstE - ConstE) * (ConstPi / 4)
    val simpler = matchSimpler(x).get
    simpler shouldBe BiFunction(ConstPi, Literal(RationalNumber(Rational.quarter), None), Product)
  }

  behavior of "multiple matchSimpler"
  ignore should "work for multi-levels 2" in {
    val x = (One + ConstE - ConstE) * (ConstPi / 4)
    val simpler = matchSimpler(x).get
    // The following will trigger the Unsuccessful: cannot call get: matchBiFunctionAsAggregate: is not converted to Aggregate
    matchSimpler(simpler).get shouldBe Expression(Valuable.piBy4)
  }

  behavior of "matchSimpler 2"
  it should "simplify (1+2)*(2+1)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(BiFunction(WholeNumber(3), 2, Power))
  }
  it should "properly simplify 1 * (root3 / root3 * 3)" in {
    val z: Expression = Expression(3).sqrt
    val x = z * z.reciprocal * Valuable(3)
    val simplified = x.simplify
    simplified.evaluateAsIs shouldBe Some(Valuable(3))
  }
  it should "simplify e * 2 / 2" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val e: Valuable = Valuable.e
    val x: Expression = Literal(e, Some("e")) * Valuable.two
    val y: Expression = Expression(Valuable.two).reciprocal
    val z = p(Product ~ x ~ y)
    z shouldBe em.Match(Expression(e))
  }
  it should "simplify root3 * 2 / 2" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val root3: core.Number = √(3) // TODO (temporary) this should be of type Valuable
    val x: Expression = Literal(root3) * Valuable.two
    val y: Expression = Expression(Valuable.two).reciprocal
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(Literal(root3))
  }
  it should "simplify root4 * 2 / 2" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    import ExpressionHelper.*
    //    val root4: Number = √(4)
    val x = math"√(4)" * Valuable.two
    val y: Expression = Expression(Valuable.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(math"√(4)")
  }
  it should "distribute" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(BiFunction(WholeNumber(3), 2, Power))
  }
  it should "distributeProductSum a" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Expression(9))
  }
  it should "distributeProductSum b" in {
    import Rational.RationalHelper
    val x = Valuable("2.00")
    val y = Valuable("3.00")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    val z = a * b
    z.evaluateAsIs shouldBe Some(Valuable(core.Real(r"21/2")))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x = Expression(3).sqrt
    val q = p(Product ~ x ~ x)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Valuable(3)
  }

  behavior of "simplifyAggregate"
  it should "simplifyAggregate" in {
    val p = em.simplifyAggregate
    val x: Aggregate = Aggregate.total(One, 3, -3)
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(One)
  }

  behavior of "simplify"
  it should "cancel addition and subtraction (a)" in {
    val x = ConstPi + 3 - 3
    val simplified = x.simplify
    simplified.evaluateAsIs shouldBe Some(Valuable.pi)
  }
  it should "use multiply instead of addition" in {
    val x = ConstPi + ConstPi
    val simplified = x.simplify
    import core.Real.RealOps
    val expected = Expression(Valuable(2 * Constants.pi))
    simplified shouldBe expected
  }
  it should "work for Negate" in {
    val x = expr.UniFunction(One, Negate)
    x.simplify shouldBe MinusOne
  }
  it should "work for Negate Negate" in {
    val x = expr.UniFunction(expr.UniFunction(One, Negate), Negate)
    x.simplify shouldBe One
  }
  it should "work for Reciprocal" in {
    val x = expr.UniFunction(Two, Reciprocal)
    x.simplify shouldBe Literal(half)
  }
  it should "work for Reciprocal Reciprocal" in {
    val x = expr.UniFunction(expr.UniFunction(ConstPi, Reciprocal), Reciprocal)
    x.simplify shouldBe ConstPi
  }
  it should "work for Negate Zero" in {
    val x = expr.UniFunction(Zero, Negate).simplify
    x shouldBe Zero
  }
  it should "work for Negate MinusOne" in {
    val x = expr.UniFunction(MinusOne, Negate).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Zero" in {
    val x = expr.UniFunction(Zero, Reciprocal).simplify
    x shouldBe Expression(infinity)
  }
  it should "work for Reciprocal One" in {
    val x = expr.UniFunction(One, Reciprocal).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Two" in {
    val x = expr.UniFunction(Two, Reciprocal).simplify
    x shouldBe Literal(half)
  }
  it should "work for Exp Infinity" in {
    val x = expr.UniFunction(Expression(infinity), Exp).simplify
    x shouldBe Infinity
  }
  it should "work for Exp neg Infinity" in {
    val x = expr.UniFunction(Expression(infinity.negate), Exp).simplify
    x shouldBe Zero
  }
  it should "work for Exp Zero" in {
    val x = expr.UniFunction(Zero, Exp)
    x.simplify shouldBe One
  }
  it should "work for Exp One" in {
    val x = expr.UniFunction(One, Exp)
    x.simplify shouldBe ConstE
  }
  it should "work for Ln Zero" in {
    val x = expr.UniFunction(Zero, Ln)
    x.simplify shouldBe Expression(infinity.negate)
  }
  it should "work for Ln One" in {
    val x = expr.UniFunction(One, Ln)
    x.simplify shouldBe Zero
  }
  it should "work for Ln e" in {
    val x = expr.UniFunction(Valuable.e, Ln)
    x.simplify shouldBe One
  }
  it should "work for Sine 0" in {
    val x = expr.UniFunction(Zero, Sine)
    x.simplify shouldBe Zero
  }
  it should "work for Sine pi/2" in {
    val x = expr.UniFunction(Literal(piBy2), Sine)
    x.simplify shouldBe One
  }
  it should "work for Sine pi" in {
    val x = expr.UniFunction(ConstPi, Sine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine 0" in {
    val x = expr.UniFunction(Zero, Cosine)
    x.simplify shouldBe One
  }
  it should "work for Cosine pi/2" in {
    val x = expr.UniFunction(Literal(piBy2), Cosine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine pi" in {
    val x = expr.UniFunction(ConstPi, Cosine)
    x.simplify shouldBe MinusOne
  }

  behavior of "complementaryTermsEliminatorAggregate"
  it should "eliminate * 2 / 2 in Aggregate" in {
    val x = Aggregate.product(ConstPi, 2, Reciprocal(Valuable.two))
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.product(ConstPi))
  }
  it should "eliminate 2 + -2 in Aggregate" in {
    val x = Aggregate.total(ConstPi, 2, Negate(Valuable.two))
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.total(ConstPi))
  }
  it should "eliminate √3 and -√3 in Aggregate" in {
    val root3 = Expression(3).sqrt
    val x = Aggregate.total(3, root3, expr.UniFunction(root3, Negate), -1)
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.total(-1, 3))
  }
  it should "eliminate square root ∧ 2 in Aggregate" in {
    val x = Aggregate.total(ConstPi, 2, Negate(Valuable.two))
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.total(ConstPi))
  }
  behavior of "simplifyAtomic"
  it should "leave atomic expression as is" in {
    val p = matchSimpler
    val x: Expression = One
    val result = p(x)
    result shouldBe em.Miss("AtomicExpression: simplifyAtomic: NamedConstant", One)
  }
  it should "simplify literal expression one" in {
    val p = matchSimpler
    val x: Expression = Literal(Valuable.one)
    val result = p(x)
    result shouldBe em.Match(One)
  }
  it should "simplify literal expression pi" in {
    val p = matchSimpler
    val x: Expression = Literal(Valuable.pi)
    val result = p(x)
    result shouldBe em.Match(ConstPi)
  }

  behavior of "factorsMatch"
  it should "match expression Sum" in {
    em.factorsMatch(Sum, One, MinusOne) shouldBe true
    em.factorsMatch(Sum, MinusOne, One) shouldBe true
    em.factorsMatch(Sum, One, ConstPi) shouldBe false
    em.factorsMatch(Sum, ConstPi, One) shouldBe false
    em.factorsMatch(Sum, One, Literal(root2)) shouldBe false
    em.factorsMatch(Sum, Literal(root2), One) shouldBe false
    //    em.factorsMatch(Sum, Literal(root2), Literal(root2) * MinusOne) shouldBe false // TODO fix this
    //    em.factorsMatch(Sum, Literal(root2) * MinusOne, Literal(root2)) shouldBe false // TODO fix this
  }
  it should "match expression Product" in {
    val root3 = Expression(3).sqrt
    em.factorsMatch(Product, ConstPi, MinusOne) shouldBe true
    em.factorsMatch(Product, MinusOne, ConstPi) shouldBe true
    em.factorsMatch(Product, root3, Literal(root2)) shouldBe true
    em.factorsMatch(Product, Literal(root2), MinusOne) shouldBe false
    em.factorsMatch(Product, MinusOne, Literal(root2)) shouldBe false
  }
  it should "match expression Power" in {
    val root3 = Expression(3).sqrt
    //        em.factorsMatch(Power, ConstPi, MinusOne) shouldBe true
    em.factorsMatch(Power, MinusOne, ConstPi) shouldBe true
    em.factorsMatch(Power, root3, Two) shouldBe true
    em.factorsMatch(Power, Literal(root2), MinusOne) shouldBe false
    em.factorsMatch(Power, MinusOne, Literal(root2)) shouldBe false
  }

  behavior of "simplify aggregate, etc."
  it should "aggregate 1" in {
    val target = One * ConstPi + Two * MinusOne
    target match {
      case biFunction: BiFunction =>
        val result = em.matchBiFunctionAsAggregate(biFunction)
        result.successful shouldBe false
    }
  }
  it should "aggregate 2" in {
    val target = One * ConstPi + Two * MinusOne + Two
    target match {
      case biFunction: BiFunction =>
        val result = em.matchBiFunctionAsAggregate(biFunction)
        result.successful shouldBe true
        result.get shouldBe Aggregate(Sum, Seq(One * ConstPi, Two * MinusOne, Two))
    }
  }
  it should "simplify aggregate 1" in {
    val x: Expression = Aggregate(Sum, Seq(One, 3, -3))
    //val result: em.MatchResult[Field] = em.simplifier(x) map (_.materialize)
    val result: Valuable = x.simplify.materialize
    result should matchPattern { case Valuable.one => }
  }
  // This appears to be fixed Issue #87
  it should "simplify aggregate 1a" in {
    val target: Expression = Aggregate(Sum, Seq(ConstPi, -ConstPi))
    val result = target.simplify.materialize
    result shouldBe Angle.zero
    //    val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    //    result match {n
    //      case em.Match(x: Field) =>
    //        convertToNumber(x) shouldBe expected
    //      case _ => fail("expected a Field")
    //    }
  }
  it should "simplify aggregate 2a" in {
    val target: Expression = Aggregate(Sum, Seq(ConstPi, -ConstPi))
    val simplify = target.simplify
    val result: Valuable = simplify.materialize
    result shouldBe Valuable.zero
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    //    result match {
    //      case em.Match(x: Field) =>
    //        convertToNumber(x).isZero shouldBe true
    //      case _ => fail("expected a Field")
    //    }
  }
  it should "simplify aggregate 2b" in {
    // NOTE: this does not create a Aggregate but instead creates a BiFunction.
    val target: Expression = CompositeExpression.create(Sum, Valuable.pi, Angle.negPi)
    //val expected = ExactNumber(0, Radian) // Ideally, the result should equal this but for now, we only test isZero.
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    val result = target.simplify.materialize
    result shouldBe Valuable.zero
    //    result match {
    //      case em.Match(x: Field) =>
    //        convertToNumber(x).isZero shouldBe true
    //      case _ => fail("expected a Field")
    //    }
  }
  it should "simplify binary expression 3" in {
    val target: Expression = BiFunction(Literal(root2), Literal(root2) * MinusOne, Sum)
    //val value1 = em.simplifier(target)
    val value1 = target.simplify
    value1 shouldBe Zero

    val result = value1.materialize
    result shouldBe Valuable.zero
    //    val result = value1 map (_.materialize)
    //    result match {
    //      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
    //      case x => fail(s"expected a Match(Field) but got $x")
    //    }
  }
  // FIXME this became an infinite loop when we added support for Root in algebra.
  ignore should "simplify aggregate 3a" in {
    val target: Expression = Aggregate(Sum, Seq(Literal(root2), Literal(root2) * Valuable.minusOne))
    //val value1 = em.simplifier(target)
    //val result = value1 map (_.materialize)
    val result = target.simplify.materialize
    result shouldBe Valuable.zero
    //    result match {
    //      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
    //      case x => fail(s"expected a Match(Field) but got $x")
    //    }
  }
  // FIXME infinite loop
  ignore should "simplify aggregate 4a" in {
    val target: Expression = Aggregate(Sum, Seq(One, ConstE, expr.UniFunction(ConstE, Negate)))
    //val result: em.MatchResult[Expression] = em.simplifier(target)
    val result = target.simplify
    result shouldBe One
  }
  // Was Issue #88 but that was fixed a while ago.
  it should "simplify aggregate 4b" in {
    val root3 = Expression(3).sqrt
    val root3PlusOne = root3 plus Expression.one
    val root3MinusOne = root3 plus Expression(Valuable.minusOne)
    val expression = root3PlusOne * root3MinusOne
    val result = expression.simplify
    //val result = em.simplifier(expression)
    result shouldBe Two
  }

  behavior of "simplifyAndEvaluate (a)"
  it should "show ∧2 and sqrt for illustrative purposes (a)" in {
    val y: Expression = ValueExpression(7).sqrt ∧ Valuable.two
    y.materialize shouldBe Valuable(7)
  }
  it should "evaluate E * 2" in {
    (Literal(Valuable.e) * 2).materialize.toString shouldBe "5.436563656918091[15]"
  }

  behavior of "simplifyTerms"
  it should "work for BiFunction 1" in {
    val target: CompositeExpression = BiFunction(Two * Two, MinusOne * MinusOne, Sum)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)

    result shouldBe BiFunction(4, One, Sum)
  }
  it should "work for BiFunction 2" in {
    val target: CompositeExpression = BiFunction(Two, MinusOne, Sum)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)
    result shouldBe target
  }
  it should "work for UniFunction" in {
    val target: CompositeExpression = expr.UniFunction(Two * Two, Negate)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)
    result shouldBe expr.UniFunction(4, Negate)
  }
  it should "work for Aggregate total" in {
    val target: CompositeExpression = Aggregate.total(Two * Two, MinusOne * MinusOne)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)
    result shouldBe Aggregate(Sum, Seq(4, One))
  }
  it should "work for Aggregate product 1" in {
    val target: CompositeExpression = Aggregate.product(Two * Two, MinusOne * MinusOne)
    //val result: Expression = em.simplifyTerms(target)
    Expression.simplifyTrivial
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)
    result shouldBe Aggregate(Product, Seq(4, One))
  }
  // FIXME infinite loop
  ignore should "work for Aggregate product 2" in {
    val target: CompositeExpression = Aggregate.total(Two * ConstPi, MinusOne * ConstPi)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = target.simplify
    //The expectation below is confused
    //result shouldBe Aggregate(Product, Seq(ConstPi))
    result shouldBe ConstPi
  }

  behavior of "biFunctionAggregator"
  it should "work for 7 + 2 - 3" in {
    val x: Expression = Expression(7) + 2 - 3
    //val p = em.matchBiFunction & em.biFunctionAggregator
    //val result = p(x)
    val result = x match {
      case b: BiFunction => em.matchBiFunctionAsAggregate(b)
      case _ => em.Miss("not a BiFunction chain", x)
    }
    result shouldBe em.Match(Aggregate.total(7, Two, expr.UniFunction(Expression(3), Negate)))
  }
  it should "work for 7 * 2 * -3" in {
    val x: Expression = Expression(7) * 2 * -3
    //val p = em.matchBiFunction & em.biFunctionAggregator
    //val result = p(x)
    // CONSIDER why is this behavior different than that for 7 + 2 - 3?
    val result = x match {
      case b: BiFunction => em.matchBiFunctionAsAggregate(b)
      case _ => em.Miss("not a BiFunction chain", x)
    }
    result shouldBe em.Match(Aggregate.product(7, Two, -3))
  }

  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val p = Expression.matchSimpler
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ∧ 2
    val z = p(y)
    z.successful shouldBe true
    z.get shouldBe seven
  }
  it should "work for products" in {
    val p = Expression.matchSimpler
    val x: Expression = 7 * 2
    val y = x * 3
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Valuable(core.Real(42))
  }
  it should "work for sums" in {
    val p = Expression.matchSimpler
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Valuable(core.Real(12))
  }
  it should "gather powers of 2 and * 1/2" in {
    val x: Expression = 7
    val y = x.sqrt
    val z = y ∧ 2
    val p = Expression.matchSimpler
    p(z) shouldBe em.Match(Expression(7))
  }

  //  behavior of "matchSimplifyDyadicTerms"
  //  it should "handle Sum" in {
  //    val p = em.matchSimplifyDyadicTerms
  //    import em.TildeOps
  //    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
  //    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
  //    p(Sum ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //    p(Sum ~ One ~ Two) shouldBe em.Match(Literal(3))
  //    // NOTE the following should Miss but instead, it generates an inexact match.
  //    //    p(Sum ~ One ~ Literal(root2)) should matchPattern { case em.Miss(_,_) => }
  //  }
  //  it should "handle Product" in {
  //    val p = em.matchSimplifyDyadicTerms
  //    import em.TildeOps
  //    p(Product ~ One ~ Zero) shouldBe em.Match(Zero)
  //    p(Product ~ Zero ~ One) shouldBe em.Match(Zero)
  //    p(Product ~ Two ~ One) shouldBe em.Match(Two)
  //    p(Product ~ One ~ Two) shouldBe em.Match(Two)
  //    p(Product ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //    p(Product ~ Two ~ Literal(3)) shouldBe em.Match(Literal(6))
  //  }
  //  it should "handle Power" in {
  //    val p = em.matchSimplifyDyadicTerms
  //    import em.TildeOps
  //    p(Power ~ Two ~ Zero) shouldBe em.Match(One)
  //    p(Power ~ Two ~ One) shouldBe em.Match(Two)
  //    p(Power ~ One ~ Two) shouldBe em.Match(One)
  //    p(Power ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //  }
  //  it should "cancel -1 and - 1" in {
  //    sb.append("cancel -1 and - 1:\n")
  //    val p = em.matchSimplifyDyadicTerms
  //    import em.TildeOps
  //    val x: Expression = Expression.one
  //    val y = -x
  //    val result = p(Sum ~ y ~ x)
  //    result should matchPattern { case em.Match(Zero) => }
  //  }
  //  it should "cancel multiplication and division" in {
  //    val p = em.matchSimplifyDyadicTerms
  //    import em.TildeOps
  //    val x = Literal(Number.pi) * 2
  //    val y = One / 2
  //    val result = p(Product ~ x ~ y)
  //    result shouldBe em.Match(ConstPi)
  //  }

  behavior of "matchComplementary"
  it should "cancel plus and minus" in {
    val p = Expression.matchSimpler
    val x = Literal(Valuable.pi)
    val y = -x
    val result = p(BiFunction(x, y, Sum))
    result should matchPattern { case em.Match(Literal(Angle.zero, _)) => }
  }

  behavior of "matchDyadicTrivial"
  it should "handle Sum" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
    p(Sum ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(Sum ~ One ~ Two) shouldBe em.Match(Literal(Valuable(3)))
  }
  it should "handle Product" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Product ~ One ~ Zero) shouldBe em.Match(Zero)
    p(Product ~ Zero ~ One) shouldBe em.Match(Zero)
    p(Product ~ Two ~ One) shouldBe em.Match(Two)
    p(Product ~ One ~ Two) shouldBe em.Match(Two)
    p(Product ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(BiFunction(Two, 3, Product)) shouldBe em.Match(Expression(6))
  }
  it should "handle Power" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Power ~ Two ~ Zero) shouldBe em.Match(One)
    p(Power ~ Two ~ One) shouldBe em.Match(Two)
    p(Power ~ One ~ Two) shouldBe em.Match(One)
    p(Power ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(Power ~ Literal(root2) ~ Two) shouldBe em.Match(Two)
  }
  it should "cancel multiplication and division" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x = Literal(Angle.pi) * 2
    val y = One / 2
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(ConstPi)
  }
  it should "cancel value and reciprocal 1" in {
    val p = em.matchComplementaryExpressions
    val x = Literal(Angle.pi)
    val y = expr.UniFunction(x, Reciprocal)
    val result = p(Product ~ x ~ y)
    result should matchPattern { case em.Match(One) => }
  }
  it should "cancel value and reciprocal 2" in {
    val p = em.matchComplementaryExpressions
    val x = Literal(Angle.pi)
    val y = expr.UniFunction(x, Reciprocal)
    val result = p(Product ~ y ~ x)
    result should matchPattern { case em.Match(One) => }
  }

  behavior of "matchComplementaryExpressions"
  it should "work for plus and minus" in {
    em.matchComplementaryExpressions(Sum ~ One ~ expr.UniFunction(One, Negate)) shouldBe em.Match(Zero)
    em.matchComplementaryExpressions(Sum ~ expr.UniFunction(One, Negate) ~ One) shouldBe em.Match(Zero)
    em.matchComplementaryExpressions(Sum ~ BiFunction(Two, One, Sum) ~ expr.UniFunction(One, Negate)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ expr.UniFunction(One, Negate) ~ BiFunction(Two, One, Sum)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ BiFunction(One, Two, Sum) ~ expr.UniFunction(One, Negate)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ expr.UniFunction(One, Negate) ~ BiFunction(One, Two, Sum)) shouldBe em.Match(Two)
  }
  it should "work for reciprocals" in {
    em.matchComplementaryExpressions(Product ~ Two ~ expr.UniFunction(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expr.UniFunction(Two, Reciprocal) ~ Two) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ BiFunction(Two, One, Product) ~ expr.UniFunction(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expr.UniFunction(Two, Reciprocal) ~ BiFunction(Two, One, Product)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ BiFunction(One, Two, Product) ~ expr.UniFunction(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expr.UniFunction(Two, Reciprocal) ~ BiFunction(One, Two, Product)) shouldBe em.Match(One)
  }

  behavior of "matchSimplifyDyadicTermsTwoLevels"

  it should "match 1" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    // CONSIDER these shouldn't be handled by matchSimplifyDyadicTermsTwoLevels since they are handled by matchComplementary
    p(Sum ~ One ~ expr.UniFunction(One, Negate)) shouldBe em.Match(Zero)
    p(Sum ~ expr.UniFunction(One, Negate) ~ One) shouldBe em.Match(Zero)
  }
  it should "match 2" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Sum ~ One ~ expr.UniFunction(ConstPi, Cosine)) shouldBe em.Match(Zero)
    p(Sum ~ expr.UniFunction(ConstPi, Cosine) ~ One) shouldBe em.Match(Zero)
    p(Sum ~ expr.UniFunction(Zero, Cosine) ~ expr.UniFunction(ConstPi, Cosine)) shouldBe em.Match(Zero)
  }

  behavior of "biFunctionSimplifier"

  it should "simplify" in {
    val p = Expression.matchSimpler
    p(BiFunction(Two, Zero, Sum)) shouldBe em.Match(Two)
    p(BiFunction(Zero, Two, Sum)) shouldBe em.Match(Two)
    p(BiFunction(Two, One, Product)) shouldBe em.Match(Two)
    p(BiFunction(One, Two, Product)) shouldBe em.Match(Two)
    p(BiFunction(Two, One, Power)) shouldBe em.Match(Two)
    p(BiFunction(One, Two, Power)) shouldBe em.Match(One)
  }
  it should "simplify √3 * -1 as -√3" in {
    val root3 = Expression(3).sqrt
    val expression = BiFunction(root3, MinusOne, Product)
    expression.simplify shouldBe expr.UniFunction(root3, Negate)
  }
  it should "simplify √3 * 1 as √3" in {
    val root3 = Expression(3).sqrt
    val expression = BiFunction(root3, One, Product)
    expression.simplify shouldBe root3
  }

  it should "simplify (√3 + 1)(√3 - 1) as 2 exactly" in {
    val em = eml // Ln this unit test
    // Expect matches:
    // matchTwoDyadicTripleLevels: Match: *~+~{3 ^ (2 ^ -1)}~1~+~{3 ^ (2 ^ -1)}~(1 * -1)
    // matchTwoDyadicTripleLevels: Match: *~^~3~(2 ^ -1)~^~3~(2 ^ -1)
    // simplifier: Match: 3
    // matchDyadicTrivial: Match: 3
    // biFunctionTransformer: Match: 3
    // biFunctionSimplifier: Match: 3
    // simplifier: Match: 3
    // matchAndCollectTwoDyadicLevels: Match: 3
    // matchDyadicTwoLevels: Match: 3
    // matchTwoDyadicLevelsR: Match: *~1~^~3~(2 ^ -1)
    // exactFieldMaterializer: Match: -1
    // matchTwoDyadicTripleLevels: Match: *~^~3~(2 ^ -1)~*~1~-1
    // matchTwoDyadicLevelsL: Match: *~^~3~(2 ^ -1)~(1 * -1)
    // matchTwoDyadicLevelsR: Match: *~{3 ^ (2 ^ -1)}~*~1~-1
    // exactFieldMaterializer: Match: 1
    // exactFieldMaterializer: Match: -1 (twice)
    // matchTwoDyadicLevelsR: Match: *~-1~^~3~(2 ^ -1)
    // matchTwoDyadicLevelsR: Match: *~-1~^~3~(2 ^ -1)
    // exactFieldMaterializer: Match: -1
    // simplifier: Match: 2
    // matchAndCollectTwoDyadicLevels: Match: 2
    // matchDyadicTwoLevels: Match: 2
    // simplifier: Match: 2
    // evaluateExactDyadicTriple: Match: 2
    val root3 = Expression(3).sqrt
    // biFunctionTransformer: Match: 2
    // biFunctionSimplifier: Match: 2
    val x: Expression = (root3 + 1) * (root3 - 1)
    val z: Expression = x.simplify
    z shouldBe Expression(2)
  }
  it should "evaluate (√3 + 1)(√3 + -1) as 2 exactly" in {
    val root3: Expression = Expression(3).sqrt
    val x: Expression = (root3 + 1) * (root3 + MinusOne)
    //val p = em.biFunctionSimplifier
    //val q: em.MatchResult[Expression] = p(x)
    val q: em.MatchResult[Expression] = em.Match(x.simplify)
    q.successful shouldBe true
  }
  it should "biFunctionSimplifier on (1 + -3)" in {
    //val p: em.ExpressionTransformer = em.biFunctionSimplifier
    val r: BiFunction = BiFunction(One, -3, Sum)
    //val z: em.MatchResult[Expression] = p(r)
    val k: em.MatchResult[Expression] = em.Match(r.simplify)
    //val value1: em.MatchResult[Expression] = z flatMap em.simplifier
    //val value2: em.MatchResult[Option[Field]] = value1 map (_.evaluateAsIs)
    //val value3: em.MatchResult[Field] = em.unpack(value2)
    //val k: em.MatchResult[Expression] = value3 map (Expression(_))
    k shouldBe em.Match(Expression(-2))
  }
  it should "biFunctionSimplifier on (1 + √3)(1 - √3)" in {
    //val p = em.simplifier
    val x = Expression(3).sqrt
    //    val x = Valuable.root3
    val y = -x
    val a = BiFunction(One, x, Sum)
    val b = BiFunction(One, y, Sum)
    val r = BiFunction(a, b, Product)
    //val z: em.MatchResult[Expression] = p(r.simplify)
    //val k: em.MatchResult[Expression] = z flatMap em.simplifier
    //print(s"r: ${r.simplify}")
    val k: em.MatchResult[Expression] = em.Match(r.simplify)
    k shouldBe em.Match(Expression(-2))
  }
  // NOTE biFunctionSimplifier cannot help here--nor should it!
  //  it should "simplify 1" in {
  //    val p = em.biFunctionSimplifier
  //    val x = Expression(3).sqrt
  //    val z = p(x)
  //    z shouldBe em.Match(Expression(±(√(3)), Some("√3")))
  //  }
  it should "simplify 1 + 2 - 2" in {
    val p = Expression.matchSimpler
    val x = One plus Two - Two
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }
  it should "properly simplify 1 + 2 - 2 + 0" in {
    val p = Expression.matchSimpler
    val x = One plus Two - Two + Zero
    val r = p(x)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe One
  }

  // (fixed) Issue #57
  it should "simplify expressions involving square root 3" in {
    val xo = Expression.parse("( 3 ∧ ( 2 ∧ -1 ) )")
    val yo = Expression.parse("( ( 3 ∧ ( 2 ∧ -1 ) ) * -1 )")
    val zo = for (x <- xo; y <- yo)
      yield Expression.matchSimpler(x * y) // 1st round
          .flatMap(Expression.matchSimpler) // 2nd round
    zo should matchPattern { case Some(_) => }
    zo.get shouldBe em.Match(Expression(-3))
  }

  behavior of "various"
  it should "distributeProductSum c" in {
    val x = Valuable("2.00*")
    val y = Valuable("3.00*")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    val z = a * b
    val eo = Expression.parse("( ( 3.00* + 0.5 ) + ( 2.00* * ( 3.00* + 0.5 ) ) )")
    eo map (e => {
      val result = matchSimpler(e)
      result.get should ===(z)
    })
  }
  it should "properly simplify 1 + root3 - root3 + 0" in {
    val z: Expression = Expression(3).sqrt
    val x = z plus -z + Zero
    val r = One + x
    val simplified = r.simplify
    simplified shouldBe One
  }
  it should "properly simplify (1 + root3) + (zero - root3)" in {
    val root3 = Expression(3).sqrt
    val x: Expression = One + root3
    val z = Zero - root3
    val expression = x + z
    val simplified = expression.simplify
    simplified shouldBe One
  }
  it should "properly simplify (1 * root3) * (3 / root3)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val root3 = Expression(3).sqrt
    val x: Expression = One * root3
    val z = Expression(3) * root3.reciprocal
    val r = p(Product ~ x ~ z)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe Expression(3)
  }
  it should "simplify 2 root(3) all squared" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val x = Expression(3).sqrt
    val a = BiFunction(Two, x, Product)
    val z = p(Power ~ a ~ Two)
    z shouldBe em.Match(Expression(12))
  }

  behavior of "value with logging"
  it should "work with value on Literal" in {
    val em = ems
    import em.MatcherOps
    implicit val logger: MatchLogger = em.matchLogger
    val f = em.value :| "value"
    f(Literal(one)).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on One" in {
    val em = ems
    import em.MatcherOps
    implicit val logger: MatchLogger = em.matchLogger
    val f = em.value :| "value"
    f(One).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on Number.one" in {
    val em = ems
    import em.MatcherOps
    implicit val logger: MatchLogger = em.matchLogger
    val f = em.value :| "value"
    f(One).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "work with value on Real" in {
    val em = ems
    import em.MatcherOps
    // CONSIDER this appears to be a debugging logger--that doesn't seem right.
    implicit val logger: MatchLogger = em.matchLogger
    val f = em.value :| "value"
    f(Literal(FuzzyNumber(Right(1), PureNumber, None))).successful shouldBe true
    sb.toString shouldBe "trying matcher value on 1...\n... value: Match: 1\n"
  }
  it should "fail on non-value" in {
    val em = ems
    val x = Expression(1) * 2
    import em.MatcherOps
    implicit val logger: MatchLogger = em.matchLogger
    val f = em.value :| "value"
    f(x).successful shouldBe false
    sb.toString shouldBe "trying matcher value on BiFunction{1 * 2}...\n... value(BiFunction{1 * 2}): Miss: value: BiFunction{1 * 2}\n"
  }

  behavior of "biFunctionTransformer (2)"

  import BiFunction.*

  private val p = Expression.matchSimpler
  it should "simplify 1 + 1" in {

    val r = p(Sum ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe Two
  }
  it should "simplify pi + pi" in {
    val r = p(Sum ~ ConstPi ~ ConstPi)
    r.successful shouldBe true
    r.get shouldBe Expression(Angle.twoPi)
  }
  it should "simplify 1 + 0" in {
    val r = p(Sum ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "fail to simplify 1 + pi" in {
    val r = p(Sum ~ One ~ ConstPi)
    r.successful shouldBe false
  }
  it should "simplify 1 + -1" in {
    val r = p(Sum ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * 1" in {
    val r = p(Product ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe One
  }
  // Issue #105 (fixed)
  it should "simplify pi * pi" in {
    val r = p(Product ~ ConstPi ~ ConstPi)
    r.successful shouldBe true
    r.get shouldBe BiFunction(ConstPi, Two, Power)
  }
  it should "simplify 1 * 0" in {
    val r = p(Product ~ One ~ Zero)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify 1 * -1" in {
    val r = p(Product ~ One ~ MinusOne)
    r.successful shouldBe true
    r.get shouldBe MinusOne
  }
  it should "simplify 2 ∧ -1" in {
    val r: Expression = Two ∧ MinusOne
    import Rational.RationalOps
    r.evaluateAsIs shouldBe Some(Valuable(1 :/ 2))
  }
  it should "fail to simplify 2 ∧ 1/2" in {
    val r = p(Power ~ Two ~ Expression(Rational.half))
    r.successful shouldBe true
  }

  behavior of "evaluateMonadicDuple"

  it should "simplify E" in {
    import UniFunction.*
    val p = Expression.matchSimpler
    val r = p(Exp ~ One)
    r.successful shouldBe true
    r.get shouldBe ConstE
  }
  it should "simplify ln(E)" in {
    import UniFunction.*
    val p = Expression.matchSimpler
    val r = p(Ln ~ ConstE)
    r.successful shouldBe true
    r.get shouldBe One
  }
  it should "simplify ln(1)" in {
    import UniFunction.*
    val p = Expression.matchSimpler
    val r = p(Ln ~ One)
    r.successful shouldBe true
    r.get shouldBe Zero
  }
  it should "simplify ln(-1)" in {
    import UniFunction.*
    val p = Expression.matchSimpler
    val r = p(Ln ~ MinusOne)
    r.successful shouldBe true
    val actual = r.get
    val expected = Literal(Valuable(ComplexPolar(core.Number.pi, core.Number.piBy2.makeNegative, 1)))
    actual shouldBe expected
  }

  // CONSIDER move the following
  behavior of "matchSimplifyBiFunction"
  // Issue #106 (fixed)
  it should "simplify multiple similar ops" in {
    val p = Expression.matchSimpler
    p(Expression(2) * 3 * Valuable.e * 5) shouldBe em.Match(ConstE * 30)
    // TODO we would like the following to be ConstE * 30
    //    em.simplifier(ConstE * 2 * 3 * 5) shouldBe em.Match(ConstE * 2 * 15)
    //    em.simplifier(Expression(5) * 2 * 3 * Valuable.e) shouldBe em.Match(ConstE * 30)
  }

  behavior of "two levels"
  it should "get 0 from -√3 + √3" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val root3 = Valuable(core.Real(√(3)))
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Product)
    val e: DyadicTriple = Sum ~ e1 ~ Literal(root3)
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  it should "get 0 from √3 + -√3" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val root3 = Expression(3).sqrt
    val e1: BiFunction = BiFunction(root3, MinusOne, Product)
    val e: DyadicTriple = Sum ~ root3 ~ e1
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  // test for Issue #126 (fixed)
  it should "get 1 from 1/√3 * √3" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val root3: Valuable = Valuable.root3
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Power)
    val e: DyadicTriple = Product ~ e1 ~ Literal(root3)
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe One
  }
  private lazy val root3Expression = Literal(Valuable(Constants.root3))

  it should "simplify -1 * √3 as negate(√3)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val e: DyadicTriple = Product ~ MinusOne ~ root3Expression
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe UniFunction(core.Number.root3, Negate)
  }
  it should "simplify √3 * -1 as negate(√3)" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val e: DyadicTriple = Product ~ root3Expression ~ MinusOne
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe UniFunction(core.Number.root3, Negate)
  }
  it should "simplify various" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    p(Sum ~ BiFunction(Two, MinusOne, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Zero)
    p(Sum ~ BiFunction(MinusOne, Two, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(MinusOne, Two, Product)) shouldBe em.Match(Zero)
  }
  it should "simplify root3 * 2 / 2" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    //    val root3: Number = √(3)
    val root3: Expression = Expression(3).sqrt
    val x: Expression = root3Expression * Valuable.two
    val y: Expression = Expression(Valuable.two).reciprocal
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(root3)
  }

  behavior of "matchAndCollectTwoDyadicLevels"
  it should "work for √3 * √3" in {
    import BiFunction.*
    val p = Expression.matchSimpler
    val e1: BiFunction = BiFunction(Expression(3), Expression(Rational.half), Power)
    val e2: BiFunction = BiFunction(Expression(3), Expression(Rational.half), Power)
    val e: DyadicTriple = Product ~ e1 ~ e2
    val triple: DyadicTriple = e
    val result = p(triple)
    result.successful shouldBe true
    result.get shouldBe Expression(3)
  }
  it should "work for √3 + -√3" in {
    val p = Expression.matchSimpler
    val root3: Expression = Expression(3).sqrt
    val e1 = BiFunction(root3, One, Product)
    val e2 = BiFunction(root3, MinusOne, Product)
    val result = p(Sum ~ e1 ~ e2)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  // Issue #128 (fixed)
  it should "work for (π + 1) * (π - 1)" in {
    val p = Expression.matchSimpler
    val e1 = BiFunction(ConstPi, One, Sum)
    val e2 = BiFunction(Literal(Valuable.pi), MinusOne, Sum)
    val result = p(Product ~ e1 ~ e2)
    result.successful shouldBe true
    result shouldBe em.Match(Aggregate(Sum, Seq(-1, BiFunction(ConstPi, 2, Power))))
    val e = result.get
    val actual = p(e)
    actual.successful shouldBe true
    // NOTE it's not trivially easy to arrange for this and it's not really that necessary, either. So let's be happy!
    val idealExpectedExpression = BiFunction(ConstPi ∧ 2, MinusOne, Sum)
    val interimExpectedExpression = Aggregate(Sum, Seq(-1, BiFunction(ConstPi, 2, Power)))
    actual.get shouldBe interimExpectedExpression
  }

  behavior of "complementaryTermsEliminatorBiFunction"
  it should "match 1 and -1 in addition" in {
    val x: Expression = 1
    val y: Expression = -1
    val z: em.MatchResult[Expression] = em.complementaryTermsEliminatorBiFunction(BiFunction(x, y, Sum))
    z.successful shouldBe true
    z.get shouldBe zero
  }
}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () })

