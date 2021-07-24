package com.phasmidsoftware.number.core

import com.phasmidsoftware.number.core.Expression.{ExpressionOps, pi}
import com.phasmidsoftware.number.core.Field.convertToNumber
import com.phasmidsoftware.number.parse.ShuntingYardParser
import org.scalactic.Equality
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class ExpressionSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  implicit object ExpressionEquality extends Equality[Expression] {
    def areEqual(a: Expression, b: Any): Boolean = b match {
      case n: GeneralNumber => new ExpressionOps(a).compare(n) == 0
      case n: Expression => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object FieldEquality extends Equality[Field] {
    def areEqual(a: Field, b: Any): Boolean = b match {
      case n: Field => a.compare(n) == 0
      case _ => false
    }
  }

  implicit object NumberEquality extends Equality[GeneralNumber] {
    def areEqual(a: GeneralNumber, b: Any): Boolean = b match {
      case n: GeneralNumber => a.compare(n) == 0
      case _ => false
    }
  }

  behavior of "parse"
  val syp = new ShuntingYardParser()
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
    eo.get shouldBe BiFunction(GeneralNumber(3), BiFunction(GeneralNumber(2), GeneralNumber(-1), Power), Power)
  }
  it should "parse and evaluate half" in {
    val eo: Option[Expression] = Expression.parse("2 ^ -1")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(GeneralNumber(2), GeneralNumber(-1), Power)
  }


  behavior of "Expression"

  it should "materialize" in {
    val x1 = GeneralNumber.one
    val x2 = GeneralNumber.pi
    val e = BiFunction(x1, x2, Sum)
    val result = e.materialize
    convertToNumber(result) shouldEqual GeneralNumber(Math.PI + 1)
  }

  it should "render" in {
    val x1 = GeneralNumber.one
    val x2 = GeneralNumber.pi
    val e = BiFunction(x1, x2, Sum)
    e.render shouldBe "4.1415926535897930(29)"
  }

  behavior of "ExpressionOps"

  it should "evaluate +" in {
    val x = GeneralNumber(1) + 2
    x shouldEqual GeneralNumber(3)
  }
  it should "evaluate -" in {
    val x = GeneralNumber(1) - 2
    x shouldEqual GeneralNumber(-1)
  }
  it should "evaluate *" in {
    val x = GeneralNumber(3) * 2
    x shouldEqual GeneralNumber(6)
  }
  it should "evaluate /" in {
    val x = GeneralNumber(6) / 2
    x shouldEqual GeneralNumber(3)
  }
  it should "evaluate ^ 2" in {
    val x = GeneralNumber(6) ^ 2
    x shouldEqual GeneralNumber(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = GeneralNumber(36).sqrt
    x shouldEqual GeneralNumber(6)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = GeneralNumber.pi / 2
    val y: Expression = x.sin
    y.materialize shouldBe GeneralNumber.one
  }
  it should "evaluate atan" in {
    Expression(GeneralNumber.zero).atan(GeneralNumber.one).materialize shouldBe (GeneralNumber.pi / 2).materialize
    Expression(GeneralNumber.one).atan(GeneralNumber.zero).materialize shouldBe (GeneralNumber.pi * 0).materialize
    GeneralNumber.one.atan(GeneralNumber.zero).materialize shouldBe (GeneralNumber.pi * 0).materialize
  }
  it should "evaluate ln E" in {
    val x: Expression = GeneralNumber.e
    val y: Expression = x.log
    y.materialize shouldBe GeneralNumber.one
  }
  it should "evaluate ln 2E" in {
    val x: Expression = GeneralNumber.e * 2
    val y: Expression = x.log
    val result = y.materialize
    val expected = GeneralNumber("1.693147180559945(13)")
    result shouldEqual expected
  }

  behavior of "toString"
  it should "work for (sqrt 7)^2" in {
    val seven: Expression = GeneralNumber(7)
    val result: Expression = seven.sqrt ^ 2
    result.toString shouldBe "{{7 ^ (2 ^ -1)} ^ 2}"
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (GeneralNumber.e * 2).materialize.toString shouldBe "5.43656365691809(3)"
  }

  behavior of "isExact"
  it should "be true for any constant GeneralNumber" in {
    GeneralNumber.one.isExact shouldBe true
    GeneralNumber.pi.isExact shouldBe true
  }
  it should "be true for any sum of exact Numbers of the same factor (not e)" in {
    (GeneralNumber.one plus GeneralNumber.two).isExact shouldBe true
    (GeneralNumber.pi plus GeneralNumber.pi).isExact shouldBe true
  }
  it should "be true for any product of exact Numbers of factor e" in {
    (GeneralNumber.e multiply GeneralNumber.e).isExact shouldBe true
  }

  behavior of "depth"
  it should "be 1 for any atomic expression" in {
    Expression(1).depth shouldBe 1
    Expression.one.depth shouldBe 1
    Literal(1).depth shouldBe 1
    pi.depth shouldBe 1
  }
  it should "be more than 1 for other expression" in {
    (GeneralNumber.e * 2).depth shouldBe 2
    (GeneralNumber.e * 2 / 2).depth shouldBe 3
    val expression = Expression(7).sqrt ^ 2
    expression.depth shouldBe 4
  }
}
