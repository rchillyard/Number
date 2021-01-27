package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.{Expression, Number, Rational}
import com.phasmidsoftware.number.parse.MillParser
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class MillSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Mill"

  it should "pop" in {
    val mill = Mill.empty
    mill.pop should matchPattern { case (None, Empty) => }
  }
  it should "push" in {
    val mill = Mill.empty.push(Expr(Number.one))
    mill.pop match {
      case (None, _) => fail("logic error")
      case (Some(x), _) => x shouldBe Expr(Number.one)
    }
  }
  it should "empty" in {
    val mill = Mill.empty
    mill.isEmpty shouldBe true
  }
  it should "apply()" in {
    val mill = Mill()
    mill.isEmpty shouldBe true
  }
  it should "apply(1)" in {
    val mill = Mill(Item("1"))
    mill.isEmpty shouldBe false
    mill.evaluate shouldBe Some(Number.one)
  }
  it should "process empty list of Items" in {
    val mill = Mill()
    mill.evaluate shouldBe None
  }
  it should "create a Mill from list of Items: 42, 37, +" in {
    val target = Mill.create(List("42", "37", "+").map(Item(_)))
    val items: List[Item] = List(Add, Expr(Number(37)), Expr(Number(42)))
    target shouldBe Stack(items)
  }
  it should "accept list of Items: 42, 37, +" in {
    val target = Mill.apply(List("42", "37", "+").map(Item(_)): _*)
    val items: List[Item] = List(Add, Expr(Number(37)), Expr(Number(42)))
    target shouldBe Stack(items)
  }
  it should "process list of Items: 42, 37, +" in {
    checkMill(Number(79), List("42", "37", "+"))
  }
  it should "process list of Items: 3, 2, ^" in {
    checkMill(Number(9), List("3", "2", "^"))
  }
  it should "process list of Items: 7, chs" in {
    checkMill(Number(-7), List("7", "chs"))
  }
  it should "process list of Items: 42, 37, +, 2, *" in {
    checkMill(Number(158), List("42", "37", "+", "2", "*"))
  }
  it should "process list of Items: 2, inv" in {
    checkMill(Number(Rational.half), List("2", "inv"))
  }
  it should "process a String: 42 37 + 2 *" in {
    val value: Option[Expression] = Mill.parse("42 37 + 2 *").toOption.flatMap(_.evaluate)
    value map (_.materialize) shouldBe Some(Number(158))
  }

  behavior of "parse and evaluate"
  val p = new MillParser

  it should "parse and evaluate: 2 37 + 2 *" in {
    val value: Try[Mill] = p.parseMill("42 37 + 2 *")
    value should matchPattern { case Success(_) => }
    value foreach (m => println(m.evaluate))
    value map (checkMill(Number(158), _))
  }
  // See https://hansklav.home.xs4all.nl/rpn/
  it should "parse and evaluate:  12  34  +  56  +  78  -  90  +  12  -  " in {
    val value: Try[Mill] = p.parseMill("12  34  +  56  +  78  -  90  +  12  -  ")
    value should matchPattern { case Success(_) => }
    value map (checkMill(Number(102), _))
  }
  // FIXME this should work.
  it should "parse and evaluate:  3696" in {
    val w =
      """12 34  * 56 78  * + 90  12  * - """
    val value: Try[Mill] = p.parseMill(w)
    value should matchPattern { case Success(_) => }
    value foreach (m => println(m.evaluate))
    value map (checkMill(Number(3696), _))
  }


  private def checkMill(expected: Number, list: List[String]): Any = {
    val items = list map (Item(_))
    val mill = Mill.create(items)
    checkMill(expected, mill)
  }


  private def checkMill(expected: Number, mill: Mill) = {
    val z = mill.evaluate
    z map (_.materialize) shouldBe Some(expected)
  }
}