package com.phasmidsoftware.number.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ComplexSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Complex"

  it should "imag" in {

  }

  it should "compare" in {

  }

  it should "multiply" in {

  }

  it should "unary_$minus" in {

  }

  it should "asNumber" in {

  }

  it should "divide" in {

  }

  it should "isAtomic" in {

  }

  it should "simplify" in {

  }

  it should "real" in {

  }

  it should "isExact" in {

  }

  it should "power" in {

  }

  it should "add" in {
    val c1 = ComplexCartesian(Number.one, Number.two)
    val c2 = ComplexCartesian(Number.two, Number.zero)
    val c3 = c1 add c2
    c3 should matchPattern { case ComplexCartesian(ExactNumber(Right(3), Scalar), ExactNumber(Right(2), Scalar)) => }
  }

  it should "depth" in {

  }

  it should "materialize" in {

  }

  it should "maybeFactor" in {

  }

  it should "addComplex" in {

  }

  it should "showImaginary" in {

  }

  it should "doAdd" in {

  }

  it should "invert" in {

  }

  it should "apply" in {

  }

  it should "convertToPolar" in {

  }

  it should "unapply" in {

  }

  it should "asComplex" in {

  }

  it should "convertToCartesian" in {

  }

}
