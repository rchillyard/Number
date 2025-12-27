package com.phasmidsoftware.number.expression.parse

import com.phasmidsoftware.number.core.numerical.Number
import com.phasmidsoftware.number.expression.mill.{Add, DyadicExpression, Expr, Expression, Item, Mill, Multiply, Sin, Stack, Subtract, TerminalExpression}
import com.phasmidsoftware.number.expression.parse.ShuntingYardParser.{InfixToken, ShuntingYard, shuntingYard}
import org.scalactic.Equality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class ShuntingYardParserSpec extends AnyFlatSpec with should.Matchers {

  implicit object NumberEquality extends Equality[Number] {
    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number => a.compare(n) == 0
      case _ => false
    }
  }

  private val p = ShuntingYardParser

  behavior of "ShuntingYardParser"
  it should "parse the easy expressions" in {
    ShuntingYardParser.stringParser(shuntingYard, "1") shouldBe Success(ShuntingYard(List(Item("1")), List()))
    ShuntingYardParser.stringParser(shuntingYard, "( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )") shouldBe Success(ShuntingYard(List(Item("1"), Item("2"), Item("3"), Add, Item("4"), Item("5"), Multiply, Multiply, Add), List()))
    ShuntingYardParser.stringParser(shuntingYard, "(sin(𝛑) * -1)") shouldBe Success(ShuntingYard(List(Item("𝛑"), Sin, Item("-1"), Multiply), List()))
    ShuntingYardParser.stringParser(shuntingYard, "sin(𝛑) - 1") shouldBe Success(ShuntingYard(List(Item("𝛑"), Sin, Item("1"), Subtract), List()))
  }
  // FIXME Issue #143
  it should "parse the tricky expressions" in {
//    ShuntingYardParser.stringParser(shuntingYard, "(sin(𝛑)-1)") shouldBe Success(ShuntingYard(List(Item("𝛑"), Sin, Item("1"), Subtract), List()))
    pending
  }

  behavior of "Mill"
  it should "parseInfix 1" in {
    val sy = p.parseInfix("1")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    val expected = Number.one
    xo.get.value shouldBe expected
  }
  it should "parseInfix 1 + 2" in {
    val sy = p.parseInfix("1 + 2")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get.value shouldBe Number(3)
  }
  it should "parseInfix ( 1 + 2 ) * 3" in {
    val sy = p.parseInfix("( 1 + 2 ) * 3")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get shouldBe DyadicExpression(DyadicExpression(1, 2, "+"), 3, "*")
  }
  // From https://www.hpmuseum.org/rpn.htm
  it should "parseInfix ( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ∧ 9" in {
    val sy = p.parseInfix("( ( ( 4 + 5 ) * ( 2 + 3 ) + 6 ) / ( 8 + 7 ) ) ∧ 9")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get.value shouldBe Number("60716.992766464000")
  }
  it should "parseInfix ( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ∧ 2 ) ∧ 3 )" in {
    val sy = p.parseInfix("( ( ( ( 4 + ( 4 × ( 2 / ( 1 − 5 ) ) ) ) ∧ 2 ) ∧ 3 )")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get.value shouldBe Number(64)
  }
  it should "parseInfix 3 + 4 × 2 / ( 1 − 5 ) ∧ 2 ∧ 3" in {
    val sy = p.parseInfix("3 + 4 × 2 / ( 1 - 5 ) ∧ 2 ∧ 3")
    sy should matchPattern { case Success(_) => }
    // 3 4 2 × 1 5 − 2 3 ∧ ∧ ÷ +
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get.value shouldEqual Number(3.0001220703125)
  }
  it should "parseInfix ( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )" in {
    val sy = p.parseInfix("( 1 + ( ( 2 + 3 ) * ( 4 * 5 ) ) )")
    sy should matchPattern { case Success(_) => }
    val xo: Option[Expression] = for (s <- sy.toOption; e <- s.evaluate) yield e
    xo should matchPattern { case Some(_) => }
    xo.get.value shouldBe Number(101)
  }
  it should "parse Infix and evaluate:  9" in {
    val value: Try[Mill] = p.parseInfix("3 ∧ 2")
    value should matchPattern { case Success(_) => }
    value.toOption flatMap (_.evaluate) map (_.value) shouldBe Some(Number(9))
  }
  it should "parse Infix and evaluate:  0.5" in {
    val value: Try[Mill] = p.parseInfix("2 ∧ -1")
    value should matchPattern { case Success(_) => }
    value.toOption flatMap (_.evaluate) map (_.value) shouldBe Some(Number.half)
  }
  it should "parse Infix and evaluate: sqrt(3)" in {
    val value: Option[Mill] = p.parseInfix("3 ∧ ( 2 ∧ -1 )").toOption
    value.isDefined shouldBe true
    val z: Option[Expression] = value.flatMap(_.evaluate)
    val q = z map (_.value)
    q shouldBe Some(Number.root3)
    // TESTME the result.
  }
  it should "parse Infix and evaluate: sin(𝛑)" in {
    val value: Option[Mill] = p.parseInfix("sin(𝛑)").toOption
    value.isDefined shouldBe true
    val stack: List[Item] = List(Sin, Expr(TerminalExpression(Number.pi)))
    value.get shouldBe Stack(stack)
    val z: Option[Expression] = value.flatMap(_.evaluate)
    val q = z map (_.value)
    q shouldBe Some(Number.zero)
  }
  it should "parse Infix and evaluate: sin(𝛑/2)" in {
    val value: Option[Mill] = p.parseInfix("sin(𝛑/2)").toOption
    value.isDefined shouldBe true
    val z: Option[Expression] = value.flatMap(_.evaluate)
    val q = z map (_.value)
    q shouldBe Some(Number.one)
  }
  it should "parse Infix and evaluate: sin(𝛑) - 1" in {
    val value: Option[Mill] = p.parseInfix("sin(𝛑) - 1").toOption
    value.isDefined shouldBe true
    val stack: List[Item] = List(Subtract, Expr(Number.one), Sin, Expr(Number.pi))
    value.get shouldBe Stack(stack)
    val z: Option[Expression] = value.flatMap(_.evaluate)
    val q = z map (_.value)
    q shouldBe Some(Number.negOne)
  }
  it should "shuntingYard" in {
    p.parseInfix("( 1 + 3 ) + ( 2 * 3 )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 1 + 3 ) + ( 2 * 3 ) )") should matchPattern { case Success(_) => }
    p.parseInfix("( ( 0.5 + 3 ) + ( 2 * ( 0.5 + 3 ) ) )") should matchPattern { case Success(_) => }
  }
  // Test for (fixed) Issue #42
  it should "shuntingYard w/o spaces" in {
    p.parseInfix("(1+3)+(2*3)") should matchPattern { case Success(_) => }
    p.parseInfix("((1+3)+(2*3))") should matchPattern { case Success(_) => }
    p.parseInfix("((0.5+3)+(2*(0.5+3)))") should matchPattern { case Success(_) => }
  }
  it should "operator" in {

  }
  // NOTE this test was really just to understand instance-method eta expansion a little better.
  it should "essentially do nothing" in {
    val s = new ShuntingYardParser.ShuntingYard(Nil, Nil)
    val f: ShuntingYardParser.InfixToken => ShuntingYardParser.ShuntingYard = s.:+
    f(InfixToken(None, false)) shouldBe s
  }
}
