package com.phasmidsoftware.number

import scala.util.{Either, Try}

package object core {

  /**
    * This is the type of the value parameter of a Number.
    */
  type Value = Either[Either[Option[Double], Rational], Int]

  type DyadicFunctions = ((Int, Int) => Try[Int], (Rational, Rational) => Try[Rational], (Double, Double) => Try[Double])

  type MonadicFunctions = (Int => Try[Int], Rational => Try[Rational], Double => Try[Double])

  type QueryFunctions = (Int => Try[Boolean], Rational => Try[Boolean], Double => Try[Boolean])

  val DoublePrecisionTolerance: Double = 1E-15
}
