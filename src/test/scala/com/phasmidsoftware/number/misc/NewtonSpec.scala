package com.phasmidsoftware.number.misc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class NewtonSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Newton"

  it should "solve x = cos(x)" in {
    val newton = Newton({ x => math.cos(x) - x }, { x => -math.sin(x) - 1 })
    newton.solve(10, 1E-10, 1.0) match {
      case Success(x) => x shouldBe 0.73908513322 +- 1E-11
      case Failure(t) => fail(t.getLocalizedMessage)
    }
  }

  it should "solve x * x = 2" in {
    val expected = 1.41421356237309504880168872420969807856967187537694
    val newton = Newton({ x => x * x - 2 }, { x => 2 * x })
    newton.solve(100, 1E-12, 99.0 / 70) match {
      case Success(x) => x shouldBe expected +- 1E-11
      case Failure(t) => fail(t.getLocalizedMessage)
    }
  }

}
