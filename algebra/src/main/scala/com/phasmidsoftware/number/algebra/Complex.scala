/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.Real
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.*
import com.phasmidsoftware.number.{algebra, core}
import org.slf4j.{Logger, LoggerFactory}
import scala.util.{Failure, Success, Try}

/**
  * This is a placeholder for a Complex number to demonstrate where it should appear in the type hierarchy (it should extend Structure).
  *
  * @see com.phasmidsoftware.number.core.numerical.Complex
  */
case class Complex(complex: numerical.Complex) extends Eager {

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = complex.render

  /**
    * Determines whether this `Valuable` is exact, i.e., has no approximation.
    *
    * CONSIDER it may be possible that there are non-approximatable entities that are not exact either.
    *
    * The method returns `true` if there is no approximate representation
    * available (i.e., `approximation` is `None`), indicating that the
    * entity is exact. Otherwise, it returns `false`.
    *
    * @return a `Boolean` indicating whether the entity is exact (`true`)
    *         or has an approximation (`false`).
    */
  def isExact: Boolean = complex.isExact

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
//  def maybeDouble: Option[Double] =
//    complex.toRational.map(_.toDouble) // TESTME

  /**
    * Optionally retrieves a factor associated with this `Valuable` if one exists (this is a Scalar).
    *
    * Factors are components or divisors related to the numerical value represented 
    * by this `Valuable`. If no such factor exists or is applicable, the result will 
    * be `None`.
    *
    * @return an `Option` containing the `Factor` if available, otherwise `None`.
    */
  def maybeFactor(context: Context): Option[Factor] = complex.maybeFactor

  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (Complex(a), Complex(b)) =>
      Success(a == b)
    case _ =>
      Failure(AlgebraException(s"Complex.eqv: unexpected input: $this and $that"))
  }

  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (Complex(a), Complex(b)) => Success(a.isSame(b)) // TODO we lose the value of `p` here (it defaults to 0.5)
    case _ => Failure(AlgebraException(s"Complex.fuzzyEqv: unexpected input: $this, $that"))
  }

  /**
    * Attempts to compute an approximate representation of the current value.
    *
    * This method provides an optional approximation of the value represented by
    * the implementing class. The approximation may account for uncertainties or
    * computational limitations. By default, this method does not force computation
    * of the approximation unless explicitly requested.
    *
    * @param force a boolean flag indicating whether to force computation of
    *              the approximation. If `true`, the method will attempt to
    *              generate an approximation even if such computation
    *              is resource-intensive or not strictly necessary.
    * @return an `Option` containing the approximate value as a `Real` if available,
    *         or `None` if no approximation can be computed.
    */
  def approximation(force: Boolean): Option[Real] =
    FP.whenever(complex.isReal) {
      val modulus: Number = complex.modulus
      Scalar.createScalar(modulus.nominalValue, modulus.factor, modulus.fuzz).approximation(force)
    }
}

object Complex {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[Complex] = new DyadicOperator[Complex] {
    def op[B <: Complex, Z](f: (Complex, B) => Try[Z])(x: Complex, y: B): Try[Z] = f(x, y)
  }

  given Eq[Complex] = Eq.instance {
    (x, y) =>
      FP.toOptionWithLog(logger.warn("Eq[Complex]", _))(x.eqv(y)).getOrElse(false)
  }

  given FuzzyEq[Complex] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || FP.toOptionWithLog(logger.warn("FuzzyEq[Complex]", _))(x.fuzzyEqv(p)(y)).getOrElse(false)
  }

}
