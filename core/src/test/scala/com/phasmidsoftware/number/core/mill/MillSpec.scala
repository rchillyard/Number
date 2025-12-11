/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.mill

import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.expression.Expression.convertMillExpressionToExpression
import com.phasmidsoftware.number.core.inner.{NatLog, Rational}
import com.phasmidsoftware.number.core.mill.{Add, CoreMill, CoreMillItem, Empty, Expr, MillException, Stack, TerminalExpression}
import com.phasmidsoftware.number.core.numerical.Field.convertToNumber
import com.phasmidsoftware.number.core.numerical.{Field, FuzzyEquality, Number, Real}
import com.phasmidsoftware.number.core.mill.CoreMill.parseInfix
import com.phasmidsoftware.number.parse.MillParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.{Assertion, Succeeded}
import scala.util.{Success, Try}

class MillSpec extends AnyFlatSpec with should.Matchers with FuzzyEquality {

  behavior of "Mill"

  it should "pop" in {
    val mill = CoreMill.empty
    mill.pop should matchPattern { case (None, Empty) => }
  }
  it should "push" in {
    val mill = CoreMill.empty.push(Expr(Number.one))
    mill.pop match {
      case (None, _) => fail("logic error")
      case (Some(x), _) => x shouldBe Expr(Number.one)
    }
  }
  private val one: CoreMillItem = CoreMillItem("1")

  it should "iterate" in {
    val mill = CoreMill(one)
    val i = mill.iterator
    i.hasNext shouldBe true
    i.next() shouldBe one
    i.hasNext shouldBe false
  }
  it should "empty" in {
    val mill = CoreMill.empty
    mill.isEmpty shouldBe true
  }
  it should "apply()" in {
    val mill = CoreMill()
    mill.isEmpty shouldBe true
  }
  it should "apply(1)" in {
    val mill = CoreMill(one)
    mill.isEmpty shouldBe false
    mill.evaluate shouldBe Some(TerminalExpression(1))
  }
  it should "process empty list of Items" in {
    val mill = CoreMill()
    mill.evaluate shouldBe None
  }
  it should "create a Mill from list of Items: 42, 37, +" in {
    val target = CoreMill.create(List("42", "37", "+").map(CoreMillItem(_)))
    val items: List[CoreMillItem] = List(Add, Expr(Number(37)), Expr(Number(42)))
    target shouldBe Stack(items)
  }
  it should "accept list of Items: 42, 37, +" in {
    val target = CoreMill.apply(List("42", "37", "+").map(CoreMillItem(_)): _*)
    val items: List[CoreMillItem] = List(Add, Expr(Number(37)), Expr(Number(42)))
    target shouldBe Stack(items)
  }
  it should "process list of Items: 42, 37, +" in {
    checkMill(Real(79), List("42", "37", "+")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items: 42, 37, -" in {
    checkMill(Real(5), List("42", "37", "-")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items with Swap: 42, 37, Swap, -" in {
    checkMill(Real(-5), List("42", "37", "<>", "-")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items with Noop: 42, 37, +, Noop" in {
    checkMill(Real(79), List("42", "37", "+", "")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items with Clr: 42, 37, +, c" in {
    val x: CoreMill = create(List("42", "37", "+", "c"))
    x.evaluate shouldBe None
  }
  it should "process Noop" in {
    val x: CoreMill = create(List("+", ""))
    an[MillException] shouldBe thrownBy(x.evaluate)
  }
  it should "process list of Items: 3, 2, ∧" in {
    checkMill(Real(9), List("3", "2", "∧")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items: 7, chs" in {
    checkMill(Real(-7), List("7", "CHS")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items: 42, 37, +, 2, *" in {
    checkMill(Real(158), List("42", "37", "+", "2", "*")) should matchPattern { case Succeeded => }
  }
  it should "process list of Items: 2, inv" in {
    checkMill(Real(Rational.half), List("2", "inv")) should matchPattern { case Succeeded => }
  }
  it should "process a String: 42 37 + 2 *" in {
    val value: Option[Expression] = CoreMill.parse("42 37 + 2 *").toOption.flatMap(_.evaluate).map(convertMillExpressionToExpression)
    value map (_.materialize) shouldBe Some(Real(158))
  }

  behavior of "parse and evaluate"
  private val p = MillParser

  it should "parse and evaluate: 2 37 + 2 *" in {
    val value: Try[CoreMill] = p.parseMill("42 37 + 2 *")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(158), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate: 2 3 ∧" in {
    val value: Try[CoreMill] = p.parseMill("2 3 ∧")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(8), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate: 2 3 /" in {
    val value: Try[CoreMill] = p.parseMill("2 3 /")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real("2/3"), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate: 2 v" in {
    val value: Try[CoreMill] = p.parseMill("2 v")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(2).sqrt, _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate: 73 24 <> -" in {
    val my: Try[CoreMill] = p.parseMill("73 24 <> -")
    my should matchPattern { case Success(_) => }
    val eo = for (z <- my.toOption; q <- z.evaluate) yield convertMillExpressionToExpression(q).materialize
    eo shouldBe Some(Real(-49))
  }
  it should "parse and evaluate: \uD835\uDED1 cos" in {
    val my: Try[CoreMill] = p.parseMill("\uD835\uDED1 cos")
    my should matchPattern { case Success(_) => }
    val eo = for (z <- my.toOption; q <- z.evaluate) yield convertMillExpressionToExpression(q).materialize
    eo shouldBe Some(Real(-1))
  }
  it should "parse and evaluate: \uD835\uDED1 sin" in {
    val my: Try[CoreMill] = p.parseMill("\uD835\uDED1 sin")
    my should matchPattern { case Success(_) => }
    val eo = for (z <- my.toOption; q <- z.evaluate) yield convertMillExpressionToExpression(q).materialize
    eo shouldBe Some(Real(0))
  }
  it should "parse and evaluate: \uD835\uDF00 ln" in {
    val my: Try[CoreMill] = p.parseMill("\uD835\uDF00 ln")
    my should matchPattern { case Success(_) => }
    val eo = for (z <- my.toOption; q <- z.evaluate) yield convertMillExpressionToExpression(q).materialize
    eo shouldBe Some(Real(1))
  }
  it should "parse and evaluate: 2 exp" in {
    val my: Try[CoreMill] = p.parseMill("2 exp")
    my should matchPattern { case Success(_) => }
    val eo = for (z <- my.toOption; q <- z.evaluate) yield convertMillExpressionToExpression(q).materialize
    eo shouldBe Some(Real(Number(2, NatLog)))
  }

  // See https://hansklav.home.xs4all.nl/rpn/
  it should "parse and evaluate:  12  34  +  56  +  78  -  90  +  12  -  " in {
    val value: Try[CoreMill] = p.parseMill("12  34  +  56  +  78  -  90  +  12  -  ")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(102), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate:  3696" in {
    val w =
      """12 34  *
        |56 78  * +
        |90  12  * - """.stripMargin
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(3696), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate:  207" in {
    val w = "6  7  +  5  *  4  +  3  *"
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(207), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate:  207 with leading space" in {
    val w = " 6  7  +  5  *  4  +  3  *"
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(207), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate:  207 with trailing space" in {
    val w = "6  7  +  5  *  4  +  3  * "
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(207), _)) should matchPattern { case Success(_) => }
  }
  it should "parse and evaluate:  9" in {
    val w = "3 2 ∧"
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(9), _)) should matchPattern { case Success(_) => }
  }
  it should "parse 3 5 + 7 2 – *" in {
    val w = "3 5 + 7 2 – *"
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(40), _)) should matchPattern { case Success(_) => }
  }
  // this is from https://en.wikipedia.org/wiki/Shunting-yard_algorithm
  it should "parse 3 4 2 × 1 5 − 2 3 ∧ ∧ ÷ +" in {
    val w = "3 4 2 × 1 5 − 2 3 ∧ ∧ ÷ +"
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    val q: Option[Expression] = value.toOption.flatMap(_.evaluate).map(convertMillExpressionToExpression)
    val z = q map (_.materialize)
    z should matchPattern { case Some(_) => }
    convertToNumber(z.get) shouldEqual Number("3.000*")
  }

  it should "parse and evaluate:  220xxxx with trailing space" in {
    val w =
      """ 6    5  ∧   7    4  ∧ +
        | 8    3  ∧   9    2  ∧ + ×
        | 2    9  ∧   3    8  ∧ +
        | 4    7  ∧   5    6  ∧ + × –""".stripMargin
    val value: Try[CoreMill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value map (checkMill(Real(-220364696), _)) should matchPattern { case Success(_) => }
  }

  it should "parse infix" in {
    val result: Option[Field] = parseInfix("12 + 34  +  56  -  78  +  90  -  12").toOption.flatMap(_.evaluate).map(convertMillExpressionToExpression).map(_.materialize)
    result.isDefined shouldBe true
    result.get shouldBe Real(102)
  }

  private def checkMill(expected: Real, list: List[String]): Assertion = {
    checkMill(expected, create(list))
  }

  private def create(list: List[String]): CoreMill = {
    val mill = CoreMill.create(list map (CoreMillItem(_)))
    mill
  }

  private def checkMill(expected: Field, mill: CoreMill): Assertion = {
    val q: Option[Field] = mill.evaluate.map(convertMillExpressionToExpression).map(_.materialize)
    q should matchPattern { case Some(_) => }
    q.get should ===(expected)
  }
}