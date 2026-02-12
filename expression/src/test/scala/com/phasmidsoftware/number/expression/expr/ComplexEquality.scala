/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager.Complex
import com.phasmidsoftware.number.core.inner.Value
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar}
import org.scalactic.Equality
import org.scalatest.matchers.should.Matchers

/**
  * Test suite for generating and validating complex solutions to Root objects.
  *
  * Complex roots occur when a quadratic equation x² + px + q = 0 has a negative discriminant:
  * discriminant = (p/2)² - q < 0
  *
  * The solutions are of the form: -p/2 ± i√(|discriminant|/4)
  */
trait ComplexEquality extends Matchers {

  implicit object ComplexCartesianEquality extends Equality[ComplexCartesian] {
    def areEqual(a: ComplexCartesian, b: Any): Boolean = b match {
      case n: numerical.Complex => (a - n).isZero
      case _ => false
    }
  }

  implicit object ComplexPolarEquality extends Equality[ComplexPolar] {
    def areEqual(a: ComplexPolar, b: Any): Boolean = b match {
      case n: ComplexPolar => (a - n).isZero
      case _ => false
    }
  }

  implicit object ComplexEquality extends Equality[Complex] {
    def areEqual(a: Complex, b: Any): Boolean = (a, b) match {
      case (Complex(x: ComplexCartesian), Complex(y: ComplexCartesian)) => x === y
      case (Complex(x: ComplexPolar), Complex(y: ComplexPolar)) => x === y
      // TODO add case for cross-types
      case _ => false
    }
  }

  def checkModulus(modulus: numerical.Number, expected: Double) = {
    val maybeDouble = Value.maybeDouble(modulus.nominalValue)
    maybeDouble shouldBe defined
    maybeDouble.get shouldBe expected +- 1e-10
  }

}