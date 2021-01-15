package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MillSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Mill"

  it should "pop" in {
    val mill = Mill.empty
    a[MillException] shouldBe thrownBy(mill.pop)
  }

  it should "push" in {
    val mill = Mill.empty.push(Constant(Number.one))
    mill.pop match {
      case (None, _) => fail("logic error")
      case (Some(x), _) => x shouldBe Constant(Number.one)
    }
  }

  it should "apply" in {
    val mill = Mill.empty
    mill.isEmpty shouldBe true
  }

  it should "process list of Items: 42, 37, *" in {
    checkMill(List("42", "37", "+"), Number(79))
  }

  it should "process list of Items: 7, -" in {
    checkMill(List("7", "-"), Number(-7))
  }

  private def checkMill(list: List[String], expected: Number) = {
    val items = list map (Item(_))
    val mill = items.foldLeft(Mill.empty)((m, x) => m.push(x))
    val (zo, m) = mill.evaluate
    zo should matchPattern { case None => }
    m.isEmpty shouldBe false
    val result = m.pop
    result should matchPattern { case (Some(_), Empty) => }
    val (z, _) = result
    val y = z map { case x: Constant => x.x.materialize }
    y shouldBe Some(expected)
  }
}