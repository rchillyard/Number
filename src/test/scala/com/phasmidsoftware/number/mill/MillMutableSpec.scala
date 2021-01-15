package com.phasmidsoftware.number.mill

import com.phasmidsoftware.number.core.Number
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MillMutableSpec extends AnyFlatSpec with should.Matchers {

  behavior of "MillMutable"

  it should "pop" in {
    val mill = MillMutable()
    mill.pop should matchPattern { case None => }
  }

  it should "push" in {
    val mill = MillMutable()
    mill.push(Number.one)
    mill.pop() match {
      case None => fail("logic error")
      case Some(x) => x shouldBe Number.one
    }
  }

  it should "apply" in {
    val mill = MillMutable()
    mill.isEmpty shouldBe true
  }

}
