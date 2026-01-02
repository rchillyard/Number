package com.phasmidsoftware.number.cats

import cats.kernel.CommutativeMonoid
import com.phasmidsoftware.number.core.misc.Variance.rootSumSquares

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

    val zero: AbsSigma = AbsSigma(0.0)

    implicit val commutativeMonoid: CommutativeMonoid[AbsSigma] = new CommutativeMonoid[AbsSigma] {
      override def empty: AbsSigma = zero

      override def combine(x: AbsSigma, y: AbsSigma): AbsSigma =
        AbsSigma(rootSumSquares(x.value, y.value))
    }
  }

  /**
    * Relative standard deviation (σ / μ). Suitable for multiplicative error propagation.
    *
    * NOTE: uses the same rule as Gaussian.convolutionProduct for independent operands:
    *   `r_xy^2 = r_x^2 + r_y^2 + (r_x r_y).`
    *
    * This is abolished.
    */
  final case class RelSigma(value: Double) extends AnyVal

  object RelSigma {

    val zero: RelSigma = RelSigma(0.0)
  }
}


