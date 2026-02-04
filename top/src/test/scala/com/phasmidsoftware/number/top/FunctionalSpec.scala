package com.phasmidsoftware.number.top

import cats.implicits.toShow
import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, NaturalExponential, WholeNumber}
import com.phasmidsoftware.number.expression.expr.*
import com.phasmidsoftware.number.parse.ExpressionParser.puremath
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FunctionalSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Functional.sc"

  // FIXME Issue #163
  it should "get root2 right" in {
    puremath"\sqrt{2}".materialize shouldBe Eager.root2
  }

  // FIXME Issue #163
  it should "perform the worksheet operations" in {

    // Euler's constant (as an expression)
    val e = E
    e shouldBe a[NamedConstant]

    // Other ways to express e
    Eager.e shouldBe NaturalExponential(WholeNumber(1))
    val e1 = puremath"\e"
    e1.toDouble shouldBe 2.718281828459045

    // What about e^2?
    val e2 = puremath"\e^2"
    e2 shouldBe BiFunction(E, 2, Power)
    e2.toDouble shouldBe 7.3890560989306495

    // Pi as an expression
    val pi = Pi
    pi shouldBe a[NamedConstant]

    // Other ways to express pi
    val pi1 = Angle(1)
    pi1.show shouldBe "\uD835\uDED1"
    Pi.materialize shouldBe Angle(WholeNumber(1))

    val deg180 = Angle.degrees(WholeNumber(180))
    deg180.show shouldBe "180°"

    // √2 as an expression
    val actual = Root.rootTwo.materialize
    val expected = Eager.root2
    actual shouldBe expected

    // Other ways to express √2
    val root2 = puremath"\sqrt{2}"
    val materialize = root2.materialize
    materialize shouldBe Eager.root2
    val string = root2.show
    string shouldBe "√2"
    materialize shouldBe Eager.root2
    root2.toDouble shouldBe 1.4142135623730951


  }
}
