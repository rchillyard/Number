package com.phasmidsoftware.number

import scala.math.BigInt
import scala.util.{Either, Try}

package object core {

  /**
    * This is the type of the value parameter of a Number.
    *
    * CONSIDER removing the BigInt option: a BigInt is just a Rational with unit denominator.
    */
  type Value = Either[Either[Either[Option[Double], Rational], BigInt], Int]

  type DyadicFunctions = ((Int, Int) => Try[Int], (BigInt, BigInt) => Try[BigInt], (Rational, Rational) => Try[Rational], (Double, Double) => Try[Double])

  type MonadicFunctions = (Int => Try[Int], BigInt => Try[BigInt], Rational => Try[Rational], Double => Try[Double])

  //  type MonadicTransformations = (Int => Try[Double], BigInt => Try[Double], Rational => Try[Double], Double => Try[Double])

  type QueryFunctions = (Int => Try[Boolean], BigInt => Try[Boolean], Rational => Try[Boolean], Double => Try[Boolean])

  val DoublePrecisionTolerance: Double = 1E-15
}
