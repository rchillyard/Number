package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.ComplexPolar.±
import com.phasmidsoftware.number.core.Expression.{ExpressionOps, pi}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.parse.ShuntingYardParser
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter with FuzzyEquality {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: GeneralNumber => new ExpressionOps(a).compare(Literal(n)) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "parse"
  private val syp = ShuntingYardParser
  it should "parse 1" in {
    syp.parseInfix("1") should matchPattern { case Success(_) => }
    syp.parseInfix("(1)") should matchPattern { case Failure(_) => } // tokens must currently be separated by white space
    syp.parseInfix("( 1 )") should matchPattern { case Success(_) => }
    syp.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
    syp.parseInfix("( ( 0.5 + 3.00* ) + ( 2.00* * ( 0.5 + 3.00* ) ) )") should matchPattern { case Success(_) => }
  }

  it should "parse and evaluate sqrt(3)" in {
    val eo: Option[Expression] = Expression.parse("3 ^ ( 2 ^ -1 )")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(Literal(3), BiFunction(Literal(2), Literal(-1), Power), Power)
  }
  it should "parse and evaluate half" in {
    val eo: Option[Expression] = Expression.parse("2 ^ -1")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(Literal(2), Literal(-1), Power)
  }

  // NOTE these are the new tests (Aug 6th) which should be good
  behavior of "Function"
  it should "work for Exp(1)" in {
    val x = Function(One, Exp)
    val result = x.materialize
    result shouldBe Constants.e
  }
  it should "work for Negate" in {
    Function(MinusOne, Negate).materialize shouldBe Constants.one
    Function(Zero, Negate).materialize shouldBe Constants.zero
    Function(One, Negate).materialize shouldBe Constants.minusOne
    Function(Two, Negate).materialize shouldBe Real(-2)
    Function(Function(Two, Negate), Negate).materialize shouldBe Constants.two
  }
  it should "work for Exp(Log(2))" in {
    val x = Function(Function(Two, Log), Exp)
    val result = x.materialize
    result shouldBe Constants.two
  }

  behavior of "Expression"

  it should "materialize" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    val result = e.materialize
    convertToNumber(result) shouldEqual Number(Math.PI + 1)
  }

  it should "render" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(Literal(x1), Literal(x2), Sum)
    e.render shouldBe "{1 + 𝛑}"
    e.materialize.render shouldBe "4.1415926535897930(41)"
  }

  it should "evaluate 3 5 + 7 2 – *" in {
    val expression = (Literal(3) + 5) * (Literal(7) - 2)
    println(expression)
    val result = expression.materialize
    result shouldEqual Real(40)
  }

  behavior of "ExpressionOps"

  it should "evaluate +" in {
    val x = Literal(1) + 2
    val y: Number = x
    y shouldEqual Number(3)
  }
  it should "evaluate -" in {
    val x = Literal(1) - 2
    val result = x.materialize.asNumber
    result shouldEqual Some(Number(-1))
  }
  it should "evaluate *" in {
    val x = Literal(3) * 2
    x shouldEqual Number(6)
  }
  it should "evaluate /" in {
    val x = Literal(6) / 2
    x shouldEqual Number(3)
  }
  it should "evaluate ^ 2" in {
    val x = Literal(6) ^ 2
    x shouldEqual Number(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = Literal(36).sqrt
    x.materialize.normalize shouldEqual ±(6)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = ConstPi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Constants.one
  }
  it should "evaluate atan" in {
    Zero.atan(One).materialize.asNumber shouldBe Some(Number.piBy2)
    Zero.atan(1).materialize.asNumber shouldBe Some(Number.piBy2)
    One.atan(0).materialize.asNumber shouldBe Some(Number(0, Radian))
    Number.one.atan(Number.zero) shouldBe Number(0, Radian)
  }
  it should "evaluate ln E" in {
    val x: Expression = ConstE
    val y: Expression = x.log
    y.materialize shouldBe Constants.one
  }
  it should "evaluate ln 2E" in {
    val x: Expression = ConstE * 2
    val y: Expression = x.log
    val result = y.materialize
    val expected = Real("1.693147180559945(13)")
    result shouldEqual expected
  }

  behavior of "toString"
  it should "work for (sqrt 7)^2" in {
    val seven: Expression = Literal(7)
    val result: Expression = seven.sqrt ^ 2
    result.toString shouldBe "{{7 ^ {2 ^ -1}} ^ 2}"
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
    (ConstPi + Constants.pi).isExactInContext(Some(Radian)) shouldBe true
  }
  it should "be false for any product of exact Numbers and a NatLog factor (except for one)" in {
    (Literal(2) * Constants.e).isExact shouldBe false
  }
  it should "be true for product of one exact Numbers and a NatLog factor" in {
    val expression = Literal(1) * Constants.e
    expression.isExact shouldBe true
  }
  it should "be true for product of zero exact Numbers and a NatLog factor" in {
    (Literal(0) * Constants.e).isExact shouldBe true
  }

  behavior of "depth"
  it should "be 1 for any atomic expression" in {
    Expression(1).depth shouldBe 1
    Expression.one.depth shouldBe 1
    Literal(1).depth shouldBe 1
    pi.depth shouldBe 1
  }
  it should "be more than 1 for other expression" in {
    (ConstE * 2).depth shouldBe 2
    (ConstE * 2 / 2).depth shouldBe 3
    val expression = Expression(7).sqrt ^ 2
    expression.depth shouldBe 4
  }

  behavior of "Euler"
  it should "work" in {
    val iPi = ComplexCartesian(0, Number.pi)
    val euler: Expression = Expression(Constants.e) ^ iPi
    euler.materialize shouldBe Constants.minusOne
  }

  behavior of "ReducedQuadraticRoot"
  it should "evaluate Phi correctly" in {
    (((Phi ^ 2) - 1).materialize - Constants.phi).isZero shouldBe true
    Phi.materialize should ===(Constants.phi)
  }
  it should "evaluate Phi^2 correctly" in {
    val em: ExpressionMatchers = Expression.em
    import em.TildeOps
    val p = em.biFunctionTransformer
    p(Power ~ Phi ~ 2) shouldBe em.Match(BiFunction(Phi, One, Sum))
  }
}
