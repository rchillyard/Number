/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
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
case class Complex(complex: numerical.Complex)(val maybeName: Option[String] = None) extends Solution {
  /**
    * Returns the number of branches in `this`.
    *
    * TODO implement this properly.
    *
    * `branches` corresponds to the number of possible solutions to some equation.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int = 1

  /**
    * Retrieves the branch at the specified index.
    *
    * @param index the index of the branch to retrieve, where the index starts from 0.
    * @return the element of type `T` at the specified branch index.
    *         If the index is out of range, the behavior is implementation-specific.
    */
  def branched(index: Int): Eager = ??? // TODO implement this properly.

  /**
    * Normalizes this `Valuable` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest `Valuable` representation of this value
    */
  def normalize: Complex = this // CONSIDER should this be a method on Complex?

  /**
    * Method to render this `Valuable` for presentation to the user.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse complex.render

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

  /**
    * Returns the negation of this `Complex` number.
    *
    * The negation operation inverts both the real and imaginary components
    * of the `Complex` instance. This is equivalent to rotating the complex
    * number by 180 degrees in the complex plane.
    *
    * @return a new `Solution` representing the negated value of this `Complex` instance
    */
  def negate: Solution = Complex(complex.rotate.rotate)()

  def +(other: Solution): Solution = other match {
    case Complex(c) =>
      Complex((complex + c).asInstanceOf[numerical.Complex])()
    case _ => throw new UnsupportedOperationException(s"Complex.+(Solution): unexpected input: $this and $other")
  }

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
    *
    * @return an `Option` containing the approximate value as a `Real` if available,
    *         or `None` if no approximation can be computed.
    */
  def approximation(force: Boolean): Option[eager.Real] =
    FP.whenever(complex.isReal) {
      val modulus: Number = complex.modulus
      Scalar.createScalar(modulus.nominalValue, modulus.factor, modulus.fuzz).approximation(force)
    }


}

/**
  * Trait `Solution` extends the `Eager` trait and represents an eagerly evaluated solution, one of several possible solutions to an equation.
  * It provides a mechanism to obtain the negation of the current solution.
  *
  * TODO once Can no longer extends Structure, let's extend CanNegate[Solution], CanAdd, etc. instead.
  */
trait Solution extends Eager with Negatable[Solution] {

  /**
    * Adds another `Solution` instance to the current instance, combining their effects or values
    * based on the implementation of the `+` operation in the `Solution` trait.
    *
    * @param other the `Solution` instance to add to the current instance
    * @return a new `Solution` instance representing the result of the addition
    */
  def +(other: Solution): Solution
}

/**
  * Companion object for the `Complex` class.
  *
  * Provides utility methods and typeclass instances related to `Complex`,
  * including factory methods, equality checks, and operator support.
  */
object Complex {
  /**
    * Constructs a new instance of `Complex` from a given `numerical.Complex`.
    *
    * @param x the input of type `numerical.Complex` to be converted into a `Complex` instance.
    * @return a new `Complex` instance created using the given `numerical.Complex`.
    */
  def apply(x: numerical.Complex): Complex = new Complex(x)()

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
