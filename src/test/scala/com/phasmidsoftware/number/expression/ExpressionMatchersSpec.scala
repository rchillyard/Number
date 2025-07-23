/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression

import com.phasmidsoftware.matchers._
import com.phasmidsoftware.number
import com.phasmidsoftware.number.core.Constants.root3
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.core.Number.{piBy2, root2, zeroR, √}
import com.phasmidsoftware.number.core.inner.Rational.infinity
import com.phasmidsoftware.number.core.inner.{PureNumber, Radian, Rational}
import com.phasmidsoftware.number.core.{Constants, ExactNumber, Field, FuzzyEquality, FuzzyNumber, Number, Real}
import com.phasmidsoftware.number.expression
import com.phasmidsoftware.number.expression.Expression.em.DyadicTriple
import com.phasmidsoftware.number.expression.Expression.{ExpressionOps, matchSimpler}
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.languageFeature.implicitConversions._

class ExpressionMatchersSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: Number => new ExpressionOps(a).compare(Literal(n)) == 0
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
  val ems: ExpressionMatchers = new ExpressionMatchers()(sbLogger) {}
  private val two: Number = 2
  private val one: Number = Number.one
  private val half: Number = convertToNumber(Number.two.invert)

  behavior of "value"
  it should "work with value on Literal" in {
    val f: em.ExpressionMatcher[Field] = em.value
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
  it should "work with value on FuzzyNumber" in {
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
    val f: em.ExpressionMatcher[Field] = em.matchValue(Constants.one)
    val e: Expression = one
    f(e).successful shouldBe true
  }

  behavior of "|"
  it should "work with | 1 or 2" in {
    val f = em.matchValue(Constants.one)
    val g = f | em.matchValue(Constants.pi)
    f(Literal(one)).successful shouldBe true
    g(Literal(Constants.pi, Some("π"))).successful shouldBe true
    g(Literal(Constants.e, Some("e"))).successful shouldBe false
  }

  import Expression.ExpressionOps

  behavior of "simplifyIdentityDyadic"

  import Matchers._

  it should "matchSimpler 1" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val x: Expression = Number.pi
    p(Sum ~ x ~ Literal(Number.zero)) shouldBe em.Match(x)
    p(Sum ~ Literal(Number.zero) ~ x) shouldBe em.Match(x)
    p(Product ~ x ~ Literal(Number.one)) shouldBe em.Match(x)
    p(Product ~ Literal(Number.one) ~ x) shouldBe em.Match(x)
    p(Power ~ x ~ Literal(Number.one)) shouldBe em.Match(x)
  }

  //  behavior of "evaluateExactDyadicTriple"
  //  ignore should "handle Sum" in {
  //    val p = em.evaluateExactDyadicTriple
  //    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
  //    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
  //    p(Sum ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //    p(Sum ~ One ~ Two) shouldBe em.Match(Literal(3))
  //    p(Sum ~ One ~ Literal(root2)) should matchPattern { case em.Miss(_, _) => }
  //  }
  //  ignore should "handle Product" in {
  //    val p = em.evaluateExactDyadicTriple
  //    p(Product ~ One ~ Zero) shouldBe em.Match(Zero)
  //    p(Product ~ Zero ~ One) shouldBe em.Match(Zero)
  //    p(Product ~ Two ~ One) shouldBe em.Match(Two)
  //    p(Product ~ One ~ Two) shouldBe em.Match(Two)
  //    p(Product ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //    p(Product ~ Two ~ Literal(3)) shouldBe em.Match(Literal(6))
  //  }
  //  ignore should "handle Power" in {
  //    val p = em.evaluateExactDyadicTriple
  //    p(Power ~ Two ~ Zero) shouldBe em.Match(One)
  //    p(Power ~ Two ~ One) shouldBe em.Match(Two)
  //    p(Power ~ One ~ Two) shouldBe em.Match(One)
  //    p(Power ~ Two ~ Two) shouldBe em.Match(Literal(4))
  //  }
  //  ignore should "cancel -1 and - 1" in {
  //    sb.append("cancel -1 and - 1:\n")
  //    val p = em.evaluateExactDyadicTriple
  //    val x: Expression = Expression.one
  //    val y = Expression.minusOne
  //    val result = p(Sum ~ y ~ x)
  //    result should matchPattern { case em.Match(Zero) => }
  //  }
  //  it should "cancel multiplication and division" in {
  //    val p = em.evaluateExactDyadicTriple
  //    val x = Literal(Number.pi) * 2
  //    val y = One / 2
  //    val result = p(Product ~ x ~ y)
  //    result shouldBe em.Match(ConstPi)
  //  }

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
    simplified.evaluateAsIs shouldBe Some(Constants.pi)
  }
  it should "use multiply instead of addition" in {
    val x = ConstPi + ConstPi
    val simplified = x.simplify
    val expected = Expression(2 * Constants.pi)
    simplified shouldBe expected
  }
  it should "work for Negate" in {
    val x = expression.Function(One, Negate)
    x.simplify shouldBe MinusOne
  }
  it should "work for Negate Negate" in {
    val x = expression.Function(expression.Function(One, Negate), Negate)
    x.simplify shouldBe One
  }
  it should "work for Reciprocal" in {
    val x = expression.Function(Two, Reciprocal)
    x.simplify shouldBe Literal(half)
  }
  it should "work for Reciprocal Reciprocal" in {
    val x = expression.Function(expression.Function(ConstPi, Reciprocal), Reciprocal)
    x.simplify shouldBe ConstPi
  }
  it should "work for Negate Zero" in {
    val x = expression.Function(Zero, Negate).simplify
    x shouldBe Zero
  }
  it should "work for Negate MinusOne" in {
    val x = expression.Function(MinusOne, Negate).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Zero" in {
    val x = expression.Function(Zero, Reciprocal).simplify
    x shouldBe Expression(infinity)
  }
  it should "work for Reciprocal One" in {
    val x = expression.Function(One, Reciprocal).simplify
    x shouldBe One
  }
  it should "work for Reciprocal Two" in {
    val x = expression.Function(Two, Reciprocal).simplify
    x shouldBe Literal(half)
  }
  it should "work for Exp neg Infinity" in {
    val x = expression.Function(Expression(infinity.negate), Exp).simplify
    x shouldBe Zero
  }
  it should "work for Exp Zero" in {
    val x = expression.Function(Zero, Exp)
    x.simplify shouldBe One
  }
  it should "work for Exp One" in {
    val x = expression.Function(One, Exp)
    x.simplify shouldBe ConstE
  }
  it should "work for Ln Zero" in {
    val x = expression.Function(Zero, Ln)
    x.simplify shouldBe Expression(infinity.negate)
  }
  it should "work for Ln One" in {
    val x = expression.Function(One, Ln)
    x.simplify shouldBe Zero
  }
  it should "work for Ln e" in {
    val x = expression.Function(Constants.e, Ln)
    x.simplify shouldBe One
  }
  it should "work for Sine 0" in {
    val x = expression.Function(Zero, Sine)
    x.simplify shouldBe Zero
  }
  it should "work for Sine pi/2" in {
    val x = expression.Function(Literal(piBy2), Sine)
    x.simplify shouldBe One
  }
  it should "work for Sine pi" in {
    val x = expression.Function(ConstPi, Sine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine 0" in {
    val x = expression.Function(Zero, Cosine)
    x.simplify shouldBe One
  }
  it should "work for Cosine pi/2" in {
    val x = expression.Function(Literal(piBy2), Cosine)
    x.simplify shouldBe Zero
  }
  it should "work for Cosine pi" in {
    val x = expression.Function(ConstPi, Cosine)
    x.simplify shouldBe MinusOne
  }

  behavior of "complementaryTermsEliminatorAggregate"
  it should "eliminate * 2 / 2 in Aggregate" in {
    val x = Aggregate.product(ConstPi, 2, Reciprocal(Constants.two))
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.product(ConstPi))
  }
  it should "eliminate 2 + -2 in Aggregate" in {
    val x = Aggregate.total(ConstPi, 2, Negate(Constants.two))
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.total(ConstPi))
  }
  it should "eliminate √3 and -√3 in Aggregate" in {
    val x = Aggregate.total(3, root3, expression.Function(root3, Negate), -1)
    val p: em.Matcher[Aggregate, Expression] = em.complementaryTermsEliminatorAggregate
    val result: em.MatchResult[Expression] = p(x)
    result shouldBe em.Match(Aggregate.total(-1, 3))
  }
  it should "eliminate square root ^ 2 in Aggregate" in {
    val x = Aggregate.total(ConstPi, 2, Negate(Constants.two))
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
    val x: Expression = Literal(Constants.one)
    val result = p(x)
    result shouldBe em.Match(One)
  }
  it should "simplify literal expression pi" in {
    val p = matchSimpler
    val x: Expression = Literal(Constants.pi)
    val result = p(x)
    result shouldBe em.Match(ConstPi)
  }

  behavior of "matchSimpler"
  it should "simplify sqrt(7)^2" in {
    val x: Expression = Expression(7)
    val y = x.sqrt
    val z = y ^ 2
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

  it should "cancel -1 and - 1" in {
    sb.append("cancel -1 and - 1:\n")
    val x: Expression = Expression.one
    val y = -x
    val z = y + x
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
  it should "cancel -1 and - 1 b" in {
    sb.append("cancel -1 and - 1 b:\n")
    val x: Expression = Expression.one
    val y = MinusOne * x
    val z = y + x
    val p = matchSimpler(z)
    p should matchPattern { case em.Match(Zero) => }
  }
  it should "cancel 2 * 1/2 (a)" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = x * y
    val p = matchSimpler(z)
    p should matchPattern { case em.Match(One) => }
  }
  it should "cancel 2 * 1/2 (b)" in {
    val x = Expression(2) * Expression.one
    val y = x.reciprocal
    val z = y * x
    val p = matchSimpler(z)
    p should matchPattern { case em.Match(One) => }
  }
  it should "cancel ^2 and sqrt for 7" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    val p = matchSimpler(y)
    p shouldBe em.Match(seven)
  }
  it should "show that lazy evaluation sometimes works even when you don't use it (a)" in {
    val seven = Number(7)
    val x: Number = seven.sqrt
    val y = x doPower two
    y.isExact shouldBe true
    y shouldBe seven
  }
  it should "cancel multiplication and division" in {
    val x = Literal(Number.pi) * 2 / 2
    matchSimpler(x).get shouldBe ConstPi
  }
  it should "cancel multiplication and division backwards" in {
    val x = Literal(Number.pi) / 2 * 2
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
    matchSimpler(z).get shouldBe One
  }
  it should "cancel 2 * 1/2" in {
    val x = Expression.one * 2
    val y = x.reciprocal
    val z = y * x
    matchSimpler(z).get shouldBe One
  }
  it should "cancel ^2 and sqrt" in {
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
    matchSimpler(y).get shouldBe Expression(7)
  }
  it should "cancel addition and subtraction of 3" in {
    val x = One + 3 - 3
    matchSimpler(x).get shouldBe One
  }
  it should "cancel addition and subtraction of e" in {
    val y: Expression = One + ConstE
    val z = y + expression.Function(ConstE, Negate)
    matchSimpler(z).get shouldBe One
  }
  it should "work for multi-levels 1" in {
    val x = (One + 3 - 3) * (Two / 4)
    matchSimpler(x).get shouldBe Half
  }
  it should "work for multi-levels 2" in {
    val x = (One + ConstE - ConstE) * (ConstPi / 4)
    matchSimpler(x).get shouldBe Expression(Constants.piBy4)
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
    em.factorsMatch(Product, ConstPi, MinusOne) shouldBe true
    em.factorsMatch(Product, MinusOne, ConstPi) shouldBe true
    em.factorsMatch(Product, Literal(root3), Literal(root2)) shouldBe true
    em.factorsMatch(Product, Literal(root2), MinusOne) shouldBe false
    em.factorsMatch(Product, MinusOne, Literal(root2)) shouldBe false
  }
  it should "match expression Power" in {
//        em.factorsMatch(Power, ConstPi, MinusOne) shouldBe true
    em.factorsMatch(Power, MinusOne, ConstPi) shouldBe true
    em.factorsMatch(Power, Literal(root3), Two) shouldBe true
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
    val result: Field = x.simplify.materialize
    result should matchPattern { case Constants.one => }
  }

  // This appears to be fixed Issue #87
  it should "simplify aggregate 1a" in {
    val target: Expression = Aggregate(Sum, Seq(ConstPi, -ConstPi))
    val expected = ExactNumber(0, Radian)
    val result = target.simplify.materialize
    convertToNumber(result) shouldBe expected
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
    val result = simplify.materialize
    result.asNumber shouldBe Some(zeroR)
    val number = convertToNumber(result)
    number.isZero shouldBe true
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
//    result match {
//      case em.Match(x: Field) =>
//        convertToNumber(x).isZero shouldBe true
//      case _ => fail("expected a Field")
//    }
  }

  it should "simplify aggregate 2b" in {
    // NOTE: this does not create a Aggregate but instead creates a BiFunction.
    val target: Expression = CompositeExpression(ConstPi, -ConstPi)
    //val expected = ExactNumber(0, Radian) // Ideally, the result should equal this but for now, we only test isZero.
    //val result: em.MatchResult[Field] = em.simplifier(target.simplify) map (_.materialize)
    val result = target.simplify.materialize
    convertToNumber(result).isZero shouldBe true
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
    convertToNumber(result) shouldBe Number.zero
//    val result = value1 map (_.materialize)
//    result match {
//      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
//      case x => fail(s"expected a Match(Field) but got $x")
//    }
  }

  it should "simplify aggregate 3a" in {
    val target: Expression = Aggregate(Sum, Seq(Literal(root2), Literal(root2) * Constants.minusOne))
    //val value1 = em.simplifier(target)
    //val result = value1 map (_.materialize)
    val result = target.simplify.materialize
    convertToNumber(result) shouldBe Number.zero
//    result match {
//      case em.Match(x: Field) => convertToNumber(x) shouldBe Number.zero
//      case x => fail(s"expected a Match(Field) but got $x")
//    }
  }

//  ignore should "simplify aggregate 3b" in {
//    CompositeExpression(Literal(root2), -Literal(root2)) match {
//      case x: BiFunction =>
//        val target = x.asAggregate
//        //        val simplified: Expression = aggregateSimplifier(target)
//        val simplified = em.simplifier(target)
//        simplified map (_.materialize) match {
//          case em.Match(x: Field) =>
//            val value = convertToNumber(x)
//            value shouldBe Number.zero
//          case x => fail(s"expected a Field but got $x")
//        }
//    }
//  }

  it should "simplify aggregate 4a" in {
    val target: Expression = Aggregate(Sum, Seq(One, ConstE, expression.Function(ConstE, Negate)))
    //val result: em.MatchResult[Expression] = em.simplifier(target)
    val result = target.simplify
    result shouldBe One
  }

  // Was Issue #88 but that was fixed a while ago.
  it should "simplify aggregate 4b" in {
    val root3 = Expression(3).sqrt
    val root3PlusOne = root3 plus Expression.one
    val root3MinusOne = root3 plus Expression(Constants.minusOne)
    val expression = root3PlusOne * root3MinusOne
    val result = expression.simplify
    //val result = em.simplifier(expression)
    result shouldBe Two
  }

  behavior of "simplifyAndEvaluate (a)"
  it should "show ^2 and sqrt for illustrative purposes (a)" in {
    val seven = Number(7)
    val x = seven.sqrt
    val y: Expression = Literal(x) ^ Constants.two
    val result = convertToNumber(y.materialize)
    result.isExact shouldBe true
    result shouldEqual Number(7)
  }
  it should "evaluate E * 2" in {
    (Literal(Number.e) * 2).materialize.toString shouldBe "5.436563656918091[15]"
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
  it should "work for Function" in {
    val target: CompositeExpression = expression.Function(Two * Two, Negate)
    //val result: Expression = em.simplifyTerms(target)
    val result: Expression = Expression.simplifyComponents(target).getOrElse(target)
    result shouldBe expression.Function(4, Negate)
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
  it should "work for Aggregate product 2" in {
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
      case _             => em.Miss("not a BiFunction chain", x)
    }
    result shouldBe em.Match(Aggregate.total(7, Two, expression.Function(Expression(3), Negate)))
  }
  it should "work for 7 * 2 * -3" in {
    val x: Expression = Expression(7) * 2 * -3
    //val p = em.matchBiFunction & em.biFunctionAggregator
    //val result = p(x)
    // CONSIDER why is this behavior different than that for 7 + 2 - 3?
    val result = x match {
      case b: BiFunction => em.matchBiFunctionAsAggregate(b)
      case _             => em.Miss("not a BiFunction chain", x)
    }
    result shouldBe em.Match(Aggregate.product(7, Two, -3))
  }

  behavior of "biFunctionSimplifier"
  it should "work for square of square root" in {
    val p = Expression.matchSimpler
    val seven = Expression(7)
    val x: Expression = seven.sqrt
    val y = x ^ 2
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
    z.get.materialize shouldBe Real(42)
  }
  it should "work for sums" in {
    val p = Expression.matchSimpler
    val seven = Expression(7)
    val x: Expression = seven + 2
    val y = x + 3
    val z: ExpressionMatchers#MatchResult[Expression] = p(y)
    z.successful shouldBe true
    z.get.materialize shouldBe Real(12)
  }
  it should "gather powers of 2 and * 1/2" in {
    val x: Expression = 7
    val y = x.sqrt
    val z = y ^ 2
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
    val x = Literal(Number.pi)
    val y = -x
    val result = p(BiFunction(x, y, Sum))
    result should matchPattern { case em.Match(Literal(Real(Number.zeroR), _)) => }
  }

  behavior of "matchDyadicTrivial"
  it should "handle Sum" in {
    import BiFunction._
    val p = Expression.matchSimpler
    p(Sum ~ Two ~ Zero) shouldBe em.Match(Two)
    p(Sum ~ Zero ~ Two) shouldBe em.Match(Two)
    p(Sum ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(Sum ~ One ~ Two) shouldBe em.Match(Literal(Constants.three))
  }
  it should "handle Product" in {
    import BiFunction._
    val p = Expression.matchSimpler
    p(Product ~ One ~ Zero) shouldBe em.Match(Zero)
    p(Product ~ Zero ~ One) shouldBe em.Match(Zero)
    p(Product ~ Two ~ One) shouldBe em.Match(Two)
    p(Product ~ One ~ Two) shouldBe em.Match(Two)
    p(Product ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(BiFunction(Two, 3, Product)) shouldBe em.Match(Expression(6))
  }
  it should "handle Power" in {
    import BiFunction._
    val p = Expression.matchSimpler
    p(Power ~ Two ~ Zero) shouldBe em.Match(One)
    p(Power ~ Two ~ One) shouldBe em.Match(Two)
    p(Power ~ One ~ Two) shouldBe em.Match(One)
    p(Power ~ Two ~ Two) shouldBe em.Match(Expression(4))
    p(Power ~ Literal(root2) ~ Two) shouldBe em.Match(Two)
  }
  it should "cancel multiplication and division" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val x = Literal(Number.pi) * 2
    val y = One / 2
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(ConstPi)
  }
  it should "cancel value and reciprocal 1" in {
    val p = em.matchComplementaryExpressions
    val x = Literal(Number.pi)
    val y = expression.Function(x, Reciprocal)
    val result = p(Product ~ x ~ y)
    result should matchPattern { case em.Match(One) => }
  }
  it should "cancel value and reciprocal 2" in {
    val p = em.matchComplementaryExpressions
    val x = Literal(Number.pi)
    val y = expression.Function(x, Reciprocal)
    val result = p(Product ~ y ~ x)
    result should matchPattern { case em.Match(One) => }
  }

  behavior of "matchComplementaryExpressions"
  it should "work for plus and minus" in {
    em.matchComplementaryExpressions(Sum ~ One ~ expression.Function(One, Negate)) shouldBe em.Match(Zero)
    em.matchComplementaryExpressions(Sum ~ expression.Function(One, Negate) ~ One) shouldBe em.Match(Zero)
    em.matchComplementaryExpressions(Sum ~ BiFunction(Two, One, Sum) ~ expression.Function(One, Negate)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ expression.Function(One, Negate) ~ BiFunction(Two, One, Sum)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ BiFunction(One, Two, Sum) ~ expression.Function(One, Negate)) shouldBe em.Match(Two)
    em.matchComplementaryExpressions(Sum ~ expression.Function(One, Negate) ~ BiFunction(One, Two, Sum)) shouldBe em.Match(Two)
  }
  it should "work for reciprocals" in {
    em.matchComplementaryExpressions(Product ~ Two ~ expression.Function(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expression.Function(Two, Reciprocal) ~ Two) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ BiFunction(Two, One, Product) ~ expression.Function(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expression.Function(Two, Reciprocal) ~ BiFunction(Two, One, Product)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ BiFunction(One, Two, Product) ~ expression.Function(Two, Reciprocal)) shouldBe em.Match(One)
    em.matchComplementaryExpressions(Product ~ expression.Function(Two, Reciprocal) ~ BiFunction(One, Two, Product)) shouldBe em.Match(One)
  }

  behavior of "matchSimplifyDyadicTermsTwoLevels"

  it should "match 1" in {
    import BiFunction._
    val p = Expression.matchSimpler
    // CONSIDER these shouldn't be handled by matchSimplifyDyadicTermsTwoLevels since they are handled by matchComplementary
    p(Sum ~ One ~ expression.Function(One, Negate)) shouldBe em.Match(Zero)
    p(Sum ~ expression.Function(One, Negate) ~ One) shouldBe em.Match(Zero)
  }
  it should "match 2" in {
    import BiFunction._
    val p = Expression.matchSimpler
    p(Sum ~ One ~ expression.Function(ConstPi, Cosine)) shouldBe em.Match(Zero)
    p(Sum ~ expression.Function(ConstPi, Cosine) ~ One) shouldBe em.Match(Zero)
    p(Sum ~ expression.Function(Zero, Cosine) ~ expression.Function(ConstPi, Cosine)) shouldBe em.Match(Zero)
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
    val expression = BiFunction(Expression(root3), MinusOne, Product)
    expression.simplify shouldBe number.expression.Function(Expression(root3), Negate)
  }
  it should "simplify √3 * 1 as √3" in {
    val expression = BiFunction(Expression(root3), One, Product)
    expression.simplify shouldBe Expression(root3)
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
    // biFunctionTransformer: Match: 2
    // biFunctionSimplifier: Match: 2
    val root3: Expression = Constants.root3
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
    //    val x = Constants.root3
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

  // Issue #57
  it should "work" in {
    val xo = Expression.parse("( 3 ^ ( 2 ^ -1 ) )")
    val yo = Expression.parse("( ( 3 ^ ( 2 ^ -1 ) ) * -1 )")
    //val zo = for (x <- xo; y <- yo) yield em.biFunctionSimplifier(x * y)
    val zo = for (x <- xo; y <- yo)
    yield Expression.matchSimpler(x * y)        // 1st round
      .flatMap(Expression.matchSimpler)     // 2nd round

    zo should matchPattern { case Some(_) => }
    zo.get shouldBe em.Match(Expression(-3))
  }

  //  behavior of "matchAndCollectTwoDyadicLevels"
  //  it should "simplify e * 2 / 2 direct" in {
  //    val p = em.matchTwoDyadicTripleLevels & em.matchAndCollectTwoDyadicLevels
  //    import em.TildeOps
  //    val e: Number = Number.e
  //    val x: Expression = Literal(e) * Constants.two
  //    val y: Expression = Expression(Constants.two).reciprocal
  //    val result = p(Product ~ x ~ y)
  //    result.successful shouldBe true
  //    result.get shouldBe ConstE
  //  }

  behavior of "matchSimpler 2"
  it should "simplify (1+2)*(2+1)" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Expression(9))
  }
  it should "properly simplify 1 * (root3 / root3 * 3)" in {
    val z: Expression = Expression(3).sqrt
    val x = z * z.reciprocal * Real(3)
    val simplified = x.simplify
    simplified.evaluateAsIs shouldBe Some(Real(3))
  }
  it should "simplify e * 2 / 2" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val e: Field = Constants.e
    val x: Expression = Literal(e, Some("e")) * Constants.two
    val y: Expression = Expression(Constants.two).reciprocal
    val z = p(Product ~ x ~ y)
    z shouldBe em.Match(Expression(e))
  }
  it should "simplify root3 * 2 / 2" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root3: Number = √(3)
    val x: Expression = Literal(root3) * Constants.two
    val y: Expression = Expression(Constants.two).reciprocal
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(Literal(root3))
  }
  it should "simplify root4 * 2 / 2" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root4: Number = √(4)
    val x = Literal(root4) * Constants.two
    val y: Expression = Expression(Constants.two).reciprocal
    p(Product ~ x ~ y) shouldBe em.Match(Literal(root4))
  }
  it should "distribute" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Expression(9))
  }
  it should "distributeProductSum a" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val a = BiFunction(One, Two, Sum)
    val b = BiFunction(Two, One, Sum)
    val z = p(Product ~ a ~ b)
    z shouldBe em.Match(Expression(9))
  }
  it should "distributeProductSum b" in {
    import Rational.RationalHelper
    val x = Number("2.00")
    val y = Number("3.00")
    val a = BiFunction(One, Literal(x), Sum)
    val b = BiFunction(Literal(half), Literal(y), Sum)
    val z = a * b
    z.evaluateAsIs shouldBe Some(Real(r"21/2"))
  }
  it should "distributeProductPower on root(3) * root(3)" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val x = Expression(3).sqrt
    val q = p(Product ~ x ~ x)
    q should matchPattern { case em.Match(_) => }
    q.get.materialize shouldBe Real(3)
  }

  // The following behaviors are all problematic (or were problematic)
  behavior of "various"

  it should "distributeProductSum c" in {
    val x = Number("2.00*")
    val y = Number("3.00*")
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
    val root3: Real = Real(Number(3).sqrt)
    val x: Expression = One + root3
    val z = Zero - root3
    val expression = x + z
    val simplified = expression.simplify
    simplified shouldBe One
  }

  it should "properly simplify (1 * root3) * (3 / root3)" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root3 = Expression(3).sqrt
    val x: Expression = One * root3
    val z = Expression(3) * root3.reciprocal
    val r = p(Product ~ x ~ z)
    r should matchPattern { case em.Match(_) => }
    r.get shouldBe Expression(3)
  }

  it should "simplify 2 root(3) all squared" in {
    import BiFunction._
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
  it should "work with value on FuzzyNumber" in {
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

  import BiFunction._

  val p = Expression.matchSimpler

  it should "simplify 1 + 1" in {

    val r = p(Sum ~ One ~ One)
    r.successful shouldBe true
    r.get shouldBe Two
  }
  it should "simplify pi + pi" in {
    val r = p(Sum ~ ConstPi ~ ConstPi)
    r.successful shouldBe true
    r.get shouldBe Expression(Constants.twoPi)
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
  // Issue #105
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
  it should "simplify 2 ^ -1" in {
    val r: Expression = Two ^ MinusOne
    import Rational.RationalOps
    r.evaluateAsIs shouldBe Some(Real(1 :/ 2))
  }
  it should "fail to simplify 2 ^ 1/2" in {
    val r = p(Power ~ Two ~ Expression(Rational.half))
    r.successful shouldBe true
  }

  behavior of "evaluateMonadicDuple"

  it should "simplify E" in {
    import Function._
    val p = Expression.matchSimpler
    val r = p(Exp ~ One)
    r.successful shouldBe true
    r.get shouldBe ConstE
  }

  it should "simplify ln(E)" in {
    import Function._
    val p = Expression.matchSimpler
    val r = p(Ln ~ ConstE)
    r.successful shouldBe true
    r.get shouldBe One
  }

  it should "simplify ln(1)" in {
    import Function._
    val p = Expression.matchSimpler
    val r = p(Ln ~ One)
    r.successful shouldBe true
    r.get shouldBe Zero
  }

  // CONSIDER move the following
  behavior of "matchSimplifyBiFunction"

  // Issue #106
  it should "simplify multiple similar ops" in {
    val p = Expression.matchSimpler
    p(Expression(2) * 3 * Constants.e * 5) shouldBe em.Match(ConstE * 30)
    // TODO we would like the following to be ConstE * 30
    //    em.simplifier(ConstE * 2 * 3 * 5) shouldBe em.Match(ConstE * 2 * 15)
//    em.simplifier(Expression(5) * 2 * 3 * Constants.e) shouldBe em.Match(ConstE * 30)
  }

  behavior of "two levels"
  it should "get 0 from -√3 + √3" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root3: Number = √(3)
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Product)
    val e: DyadicTriple = Sum ~ e1 ~ Literal(root3)
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  it should "get 0 from √3 + -√3" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Product)
    val e: DyadicTriple = Sum ~ Literal(root3) ~ e1
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Zero
  }
  // test for Issue #126
  it should "get 1 from 1/√3 * √3" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root3: Number = √(3)
    val e1: BiFunction = BiFunction(Literal(root3), MinusOne, Power)
    val e: DyadicTriple = Product ~ e1 ~ Literal(root3)
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe One
  }
  it should "simplify -1 * √3 as negate(√3)" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val e: DyadicTriple = Product ~ MinusOne ~ Literal(Number.root3)
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Function(Number.root3, Negate)
  }
  it should "simplify √3 * -1 as negate(√3)" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val e: DyadicTriple = Product ~ Literal(Number.root3) ~ MinusOne
    val result = p(e)
    result.successful shouldBe true
    result.get shouldBe Function(Number.root3, Negate)
  }
  it should "simplify various" in {
    import BiFunction._
    val p = Expression.matchSimpler
    p(Sum ~ BiFunction(Two, MinusOne, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(Two, MinusOne, Product)) shouldBe em.Match(Zero)
    p(Sum ~ BiFunction(MinusOne, Two, Product) ~ Two) shouldBe em.Match(Zero)
    p(Sum ~ Two ~ BiFunction(MinusOne, Two, Product)) shouldBe em.Match(Zero)
  }
  it should "simplify root3 * 2 / 2" in {
    import BiFunction._
    val p = Expression.matchSimpler
    val root3: Number = √(3)
    val x: Expression = Literal(root3) * Constants.two
    val y: Expression = Expression(Constants.two).reciprocal
    val result = p(Product ~ x ~ y)
    result shouldBe em.Match(Literal(root3))
  }

  behavior of "matchAndCollectTwoDyadicLevels"
  it should "work for √3 * √3" in {
    import BiFunction._
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
  // Issue #128
  it should "work for (π + 1) * (π - 1)" in {
    val p = Expression.matchSimpler
    val e1 = BiFunction(ConstPi, One, Sum)
    val e2 = BiFunction(Literal(Constants.pi), MinusOne, Sum)
    val result = p(Product ~ e1 ~ e2)
    result.successful shouldBe true
    result shouldBe em.Match(Aggregate(Sum, Seq(-1, BiFunction(ConstPi, 2, Power))))
    val e = result.get
    val actual = p(e)
    actual.successful shouldBe true
    val idealExpectedExpression = BiFunction(ConstPi ^ 2, MinusOne, Sum)
    val interimExpectedExpression = Aggregate(Sum, Seq(-1, BiFunction(ConstPi, 2, Power)))
    actual.get shouldBe interimExpectedExpression
  }
}

case class SBLogger(override val logLevel: LogLevel, sb: StringBuilder) extends MatchLogger(logLevel, { w => sb.append(s"$w\n"); () })

