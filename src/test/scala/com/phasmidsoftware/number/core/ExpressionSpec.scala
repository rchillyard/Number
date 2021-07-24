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
      case n: Number => new ExpressionOps(a).compare(n) == 0
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

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
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
    eo.get shouldBe BiFunction(Number(3), BiFunction(Number(2), Number(-1), Power), Power)
  }
  it should "parse and evaluate half" in {
    val eo: Option[Expression] = Expression.parse("2 ^ -1")
    eo should matchPattern { case Some(_) => }
    eo.get shouldBe BiFunction(Number(2), Number(-1), Power)
  }


  behavior of "Expression"

  it should "materialize" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(x1, x2, Sum)
    val result = e.materialize
    convertToNumber(result) shouldEqual Number(Math.PI + 1)
  }

  it should "render" in {
    val x1 = Number.one
    val x2 = Number.pi
    val e = BiFunction(x1, x2, Sum)
    e.render shouldBe "4.1415926535897930(29)"
  }

  behavior of "ExpressionOps"

  it should "evaluate +" in {
    val x = Number(1) + 2
    x shouldEqual Number(3)
  }
  it should "evaluate -" in {
    val x = Number(1) - 2
    x shouldEqual Number(-1)
  }
  it should "evaluate *" in {
    val x = Number(3) * 2
    x shouldEqual Number(6)
  }
  it should "evaluate /" in {
    val x = Number(6) / 2
    x shouldEqual Number(3)
  }
  it should "evaluate ^ 2" in {
    val x = Number(6) ^ 2
    x shouldEqual Number(36)
  }
  it should "evaluate sqrt 36" in {
    val x: Expression = Number(36).sqrt
    x shouldEqual Number(6)
  }
  it should "evaluate sin pi/2" in {
    val x: Expression = Number.pi / 2
    val y: Expression = x.sin
    y.materialize shouldBe Number.one
  }
  it should "evaluate atan" in {
    Expression(Number.zero).atan(Number.one).materialize shouldBe (Number.pi / 2).materialize
    Expression(Number.one).atan(Number.zero).materialize shouldBe (Number.pi * 0).materialize
    Number.one.atan(Number.zero).materialize shouldBe (Number.pi * 0).materialize
  }
  it should "evaluate ln E" in {
    val x: Expression = Number.e
    val y: Expression = x.log
    y.materialize shouldBe Number.one
  }
  it should "evaluate ln 2E" in {
    val x: Expression = Number.e * 2
    val y: Expression = x.log
    val result = y.materialize
    val expected = Number("1.693147180559945(13)")
    result shouldEqual expected
  }

  behavior of "toString"
  it should "work for (sqrt 7)^2" in {
    val seven: Expression = Number(7)
    val result: Expression = seven.sqrt ^ 2
    result.toString shouldBe "{{7 ^ (2 ^ -1)} ^ 2}"
  }

  behavior of "various operations"
  it should "evaluate E * 2" in {
    (Number.e * 2).materialize.toString shouldBe "5.43656365691809(3)"
  }

  behavior of "isExact"
  it should "be true for any constant Number" in {
    Number.one.isExact shouldBe true
    Number.pi.isExact shouldBe true
  }
  it should "be true for any sum of exact Numbers of the same factor (not e)" in {
    (Number.one plus Number.two).isExact shouldBe true
    (Number.pi plus Number.pi).isExact shouldBe true
  }
  it should "be true for any product of exact Numbers of factor e" in {
    (Number.e multiply Number.e).isExact shouldBe true
  }

  behavior of "depth"
  it should "be 1 for any atomic expression" in {
    Expression(1).depth shouldBe 1
    Expression.one.depth shouldBe 1
    Literal(1).depth shouldBe 1
    pi.depth shouldBe 1
  }
  it should "be more than 1 for other expression" in {
    (Number.e * 2).depth shouldBe 2
    (Number.e * 2 / 2).depth shouldBe 3
    val expression = Expression(7).sqrt ^ 2
    expression.depth shouldBe 4
  }
}
