package com.phasmidsoftware.number

import scala.util.Either

package object core {

  /**
    * This is the type of the value parameter of a Number.
    */
  type Value = Either[Either[Either[Option[Double], Rational], BigInt], Int]
}
