package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.{Expression, Number, Rational}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

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
  it should "process list of Items: 42, 37, +" in {
    checkMill(List("42", "37", "+"), Number(79))
  }
  it should "process list of Items: 3, 2, ^" in {
    checkMill(List("3", "2", "^"), Number(9))
  }
  it should "process list of Items: 7, -" in {
    checkMill(List("7", "-"), Number(-7))
  }
  it should "process list of Items: 42, 37, +, 2, *" in {
    checkMill(List("42", "37", "+", "2", "*"), Number(158))
  }
  it should "process list of Items: 2, inv" in {
    checkMill(List("2", "inv"), Number(Rational.half))
  }
  it should "process a String: 42 37 + 2 *" in {
    val value: Option[Expression] = Mill.parse("42 37 + 2 *").evaluate
    value map (_.materialize) shouldBe Some(Number(158))
  }

  private def checkMill(list: List[String], expected: Number) = {
    val items = list map (Item(_))
    val mill = items.foldLeft(Mill.empty)((m, x) => m.push(x))
    val z = mill.evaluate
    z map (_.materialize) shouldBe Some(expected)
  }
}