/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.{Factor, Rational}
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
    * CONSIDER implement this more generally.
    * For the roots of quadratic equations, the number of branches is 2, since there are two possible solutions.
    * But for nth roots, we would need to support more branches.
    *
    * `branches` corresponds to the number of possible solutions to some equation.
    *
    * @return the number of branches as an integer.
    */
  def branches: Int = 2

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
    * Computes and returns the conjugate of the current `Solution` instance.
    *
    * The conjugate represents a value or transformation that is mathematically
    * paired oppositely with the current instance based on the structure and
    * properties of the `Solution` implementation.
    *
    * Normally, the `branch` of the conjugate, added to the current `branch` should equal `branches`.
    *
    * @return a new `Solution` instance representing the conjugate of the current instance
    */
  def conjugate: Solution = copy(complex = complex.conjugate)(None)

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

  def +(other: Solution): Eager = other match {
    case Complex(c) =>
      Complex((complex + c).asInstanceOf[numerical.Complex])()
    case _ => throw new UnsupportedOperationException(s"Complex.+(Solution): unexpected input: $this and $other")
  }

  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (Complex(a), Complex(b)) =>
      Success(a.normalize == b.normalize)
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
  * Represents a `Solution` that is `Eager`, negatable, and branched with monotonic properties.
  * This trait combines the characteristics of eagerly evaluated values, negatable structures,
  * and entities with multiple branches (or solutions) represented by `Monotone`.
  */
trait Solution extends Eager with Negatable[Solution] with Branched[Rational] {

  /**
    * Computes and returns the conjugate of the current `Solution` instance.
    *
    * The conjugate represents a value or transformation that is mathematically
    * paired oppositely with the current instance based on the structure and
    * properties of the `Solution` implementation.
    *
    * Normally, the `branch` of the conjugate, added to the current `branch` should equal `branches`.
    *
    * @return a new `Solution` instance representing the conjugate of the current instance
    */
  def conjugate: Solution

  /**
    * Adds another `Solution` instance to the current instance, combining their effects or values
    * based on the implementation of the `+` operation in the `Solution` trait.
    *
    * CONSIDER this should be defined in `CanAdd[Solution]` rather than `Solution`.
    * Maybe we could achieve that by defining `Can` as a subtype of `Eager`, rather than of `Structure`.
    *
    * @param other the `Solution` instance to add to the current instance
    * @return a new `Solution` instance representing the result of the addition
    */
  def +(other: Solution): Eager

  /**
    * Computes a rational value based on a given index, `k`, and the number of branches
    * in a cyclic structure.
    * This method is applicable only to quadratic solutions.
    * If we ever are able to handle cubic, etc., solutions, we will need to define a more general method.
    *
    * @param k an integer representing the index in the cyclic sequence, where `k` must
    *          be a non-negative number within the bounds of the branches.
    *
    * @return a `Rational` value corresponding to the quadratic offset coefficient
    *         derived from the provided index and the cyclic properties of the structure.
    */
  def branched(k: Int): Rational = Solution.quadraticOffsetCoefficient(k, branches)

}

object Solution {

  /**
    * Computes the quadratic offset coefficient for a given index within a cyclic structure.
    *
    * The method maps the index `k` within the range {0, 1, ..., n-1} to an odd position
    * in a `2n`-cycle and calculates a corresponding rational coefficient.
    *
    * @param k the index to map, where `k` is an integer in the range {0, 1, ..., n-1}
    * @param n the total number of branches in the cycle, representing the size of the range
    * @return the quadratic offset coefficient as a `Rational` value
    */
  def quadraticOffsetCoefficient(k: Int, n: Int): Rational = if (k == 0) 1 else -1
  // NOTE that I wanted to use something like below but that doesn't actually give the right answers.
//  {
// Map k ∈ {0, 1, ..., n-1} to odd positions in 2n cycle
//    val position = 2 * k + 1  // gives {1, 3, 5, ..., 2n-1}
//    Rational(position, 2 * n) - 1 // gives {-1/2, +1/2, ...}
//  }

  /**
    * LatexRenderer for Solution (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val solutionLatexRenderer: LatexRenderer[Solution] = LatexRenderer.instance {
    case c: Complex => c.toLatex
    case a: Algebraic => a.toLatex
    case s => s.render // Fallback to render method
  }

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

  /**
    * LatexRenderer for Complex numbers.
    *
    * Renders as a + bi where:
    * - a is the real part
    * - b is the imaginary part
    * - Special cases for 0, purely real, purely imaginary handled
    */
  implicit val complexLatexRenderer: LatexRenderer[Complex] = LatexRenderer.instance { c =>
    val complex = c.complex.asCartesian
    val real = complex.real
    val imag = complex.imag

    // Helper to check if a number is effectively zero
    def isZero(n: com.phasmidsoftware.number.core.numerical.Number): Boolean =
      n.isZero

    // Helper to check if a number is effectively one
    def isOne(n: com.phasmidsoftware.number.core.numerical.Number): Boolean =
      n.isUnity

    // Helper to check if a number is effectively negative one
    def isNegOne(n: com.phasmidsoftware.number.core.numerical.Number): Boolean =
      n == Number.negOne

    // TODO do it with latex
    (isZero(real), isZero(imag)) match {
      case (true, true) => "0"
      case (true, false) =>
        if (isOne(imag)) "i"
        else if (isNegOne(imag)) "-i"
        else s"i$imag"
      case (false, true) =>
        real.render
      case (false, false) =>
        s"$real + i${imag.render}"
    }
  }
}
