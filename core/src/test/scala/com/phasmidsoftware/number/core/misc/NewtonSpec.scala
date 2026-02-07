/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.misc.{ContinuedFraction, Newton}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.Constants
import com.phasmidsoftware.number.core.numerical.Number.piBy2
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
//  it should "worksheet" in {
//
//    /**
//      * Some trigonometric identities.
//      */
//    val iPi = Expression(Constants.i) * Constants.pi
//    val euler = Expression(Constants.e) ∧ iPi
//    println(euler.asNumber)
//
//    val a: numerical.Number = piBy2.cos
//    println(a)
//    val b: numerical.Number = piBy2.sin
//    println(b)
//    println(piBy2.tan)
//  }
}
