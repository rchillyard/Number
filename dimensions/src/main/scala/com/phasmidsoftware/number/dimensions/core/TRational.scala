package com.phasmidsoftware.number.dimensions.core

import scala.compiletime.ops.int.*

/**
  * Type-level rational numbers for representing fractional dimension exponents.
  */
trait TRational

/**
  * A type-level rational number with numerator N and denominator D.
  * Rationals are automatically normalized on construction.
  */
case class TRat[N <: Int, D <: Int]() extends TRational

// Type-level absolute value
type Abs[X <: Int] <: Int = (X < 0) match {
  case true => 0 - X
  case false => X
}

// Type-level GCD using Euclidean algorithm
type GCD[A <: Int, B <: Int] <: Int = B match {
  case 0 => Abs[A]
  case _ => GCD[B, A % B]
}

/**
  * Normalize a rational by dividing both numerator and denominator by their GCD
  */
type Normalize[R <: TRational] <: TRational = R match {
  case TRat[n, d] => TRat[n / GCD[n, d], d / GCD[n, d]]
}

/**
  * Add two type-level rationals: n1/d1 + n2/d2 = (n1*d2 + n2*d1)/(d1*d2)
  */
type AddTRat[R1 <: TRational, R2 <: TRational] <: TRational = (R1, R2) match {
  case (TRat[n1, d1], TRat[n2, d2]) => Normalize[TRat[n1 * d2 + n2 * d1, d1 * d2]]
}

/**
  * Subtract two type-level rationals: n1/d1 - n2/d2 = (n1*d2 - n2*d1)/(d1*d2)
  */
type SubTRat[R1 <: TRational, R2 <: TRational] <: TRational = (R1, R2) match {
  case (TRat[n1, d1], TRat[n2, d2]) => Normalize[TRat[n1 * d2 - n2 * d1, d1 * d2]]
}

/**
  * Multiply two type-level rationals: (n1/d1) * (n2/d2) = (n1*n2)/(d1*d2)
  */
type MulTRat[R1 <: TRational, R2 <: TRational] <: TRational = (R1, R2) match {
  case (TRat[n1, d1], TRat[n2, d2]) => Normalize[TRat[n1 * n2, d1 * d2]]
}

/**
  * Divide two type-level rationals: (n1/d1) / (n2/d2) = (n1*d2)/(d1*n2)
  */
type DivTRat[R1 <: TRational, R2 <: TRational] <: TRational = (R1, R2) match {
  case (TRat[n1, d1], TRat[n2, d2]) => Normalize[TRat[n1 * d2, d1 * n2]]
}