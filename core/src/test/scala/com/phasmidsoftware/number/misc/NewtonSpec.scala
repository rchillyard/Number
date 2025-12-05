package com.phasmidsoftware.number.misc

import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.Constants
import com.phasmidsoftware.number.core.Number.piBy2
import com.phasmidsoftware.number.core.expression.Expression
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
    val expected = 1.41421356237309504
    val newton = Newton({ x => x * x - 2 }, { x => 2 * x })
    val initialGuess = ContinuedFraction.root2.coefficients.take(4).last.toRational.toDouble
    newton.solve(100, 1E-15, initialGuess) match {
      case Success(x) => x shouldBe expected +- 1E-15
      case Failure(t) => fail(t.getLocalizedMessage)
    }
  }

  behavior of "Newton Worksheet"
  it should "worksheet" in {

    /**
      * Some trigonometric identities.
      */
    val iPi = Expression(Constants.i) * Constants.pi
    val euler = Expression(Constants.e) ∧ iPi
    println(euler.asNumber)

    val a: core.Number = piBy2.cos
    println(a)
    val b: core.Number = piBy2.sin
    println(b)
    println(piBy2.tan)
  }
}
