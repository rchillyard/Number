package com.phasmidsoftware.number.cats

import cats.kernel.CommutativeMonoid

/**
  * Lightweight wrappers representing common error metrics so that we can endow them with
  * Cats algebra instances without dragging Number/Fuzziness context (factor, nominal value, etc.).
  *
  * Each wrapper models the magnitude of an uncertainty under the assumption of independent errors.
  * They serve as the core building blocks for folding or parallel aggregation of fuzzy quantities.
  */
object ErrorCommutativeMonoid {

  /** Absolute standard deviation (σ). Suitable for additive error propagation. */
  final case class AbsSigma(value: Double) extends AnyVal

  object AbsSigma {
    private def square(x: Double): Double = x * x

    val zero: AbsSigma = AbsSigma(0.0)

    implicit val commutativeMonoid: CommutativeMonoid[AbsSigma] = new CommutativeMonoid[AbsSigma] {
      override def empty: AbsSigma = zero

      override def combine(x: AbsSigma, y: AbsSigma): AbsSigma =
        AbsSigma(math.sqrt(square(x.value) + square(y.value)))
    }
  }

  /**
    * Relative standard deviation (σ / μ). Suitable for multiplicative error propagation.
    *
    * NOTE: uses the same rule as Gaussian.convolutionProduct for independent operands:
    *   `r_xy^2 = r_x^2 + r_y^2 + (r_x r_y).`
    */
  final case class RelSigma(value: Double) extends AnyVal

  object RelSigma {
    private def square(x: Double): Double = x * x

    val zero: RelSigma = RelSigma(0.0)
  }
}


