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

  it should "process list of Items" in {
    val list = List("42", "37", "+")
    val items = list map (Item(_))
    val mill = items.foldLeft(Mill.empty)((m, x) => m.push(x))
    val (zo, m) = mill.evaluate
    zo should matchPattern { case None => }
    m.isEmpty shouldBe false
    val (result, emptyMill) = m.pop
    m.pop should matchPattern { case (Some(_), `emptyMill`) => }
    emptyMill.isEmpty shouldBe true
    result.get match {
      case Constant(e) => e.materialize shouldBe Number(79)
      case x => fail(s"logic error: $x")
    }
  }

}