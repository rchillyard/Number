/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager.RationalNumber.rationalLatexRenderer
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Value.fromRational
import com.phasmidsoftware.number.core.inner.{Composite, DyadicOperationPlus, Factor, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.ExactNumber
import org.slf4j.{Logger, LoggerFactory}

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.util.{Failure, Success, Try}

/**
  * Trait to model the behavior of a solution to an equation.
  * All such solutions are exact.
  * Typically, however, such solutions cannot be represented exactly as pure numbers.
  * The four parameters of a Algebraic are: base, offset, coefficient, and factor.
  * Solutions are subject to various operations such as addition, scaling, and number conversions.
  * Extends the `NumberLike` trait, inheriting behavior common to number-like entities.
  *
  * CONSIDER why is there so much dependence on the core module here? We shouldn't need that at all!
  */
sealed trait Algebraic extends Solution with Unitary with Scalable[Algebraic] {
  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Structure

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Structure

  /**
    * This is the coefficient of the offset (square root term) the solution of a quadratic equation.
    *
    * @return the coefficient.
    */
  def coefficient: Int

  /**
    * Determines whether the solution is a pure number.
    * A pure number is defined as one without any associated factors or offsets.
    *
    * @return true if the solution is a pure number, false otherwise
    */
  def isPureNumber: Boolean

  /**
    * Adds a `Rational` value to the current solution and returns a new `Algebraic` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Algebraic` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Algebraic

  /**
    * Determines the factor associated with this solution based on certain conditions.
    * If the solution is a pure number, it returns `Some(PureNumber)`.
    * If the base value of the solution is zero, it returns `Some(factor)`.
    * Otherwise, it returns `None`.
    *
    * CONSIDER invoke `normalize` and then apply `maybeFactor` to the result.
    *
    * @return an `Option` of type `Factor`, where `Some` may contain the factor
    *         depending on the conditions, or `None` if no factor is applicable
    */
  def maybeFactor(context: Context): Option[Factor] =
    (base, offset) match {
      case (x, y) if x.isZero && y.isZero =>
        Some(PureNumber)
      case (x, y) if y.isZero =>
        x.maybeFactor(context)
      case (x, y) if x.isZero =>
        y.maybeFactor(context)
      case (x, y) =>
        Some(Composite)
    }

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = true
//
//  /**
//    * Determines whether the current solution is equivalent to another solution.
//    * This method performs a structural comparison for specific solution types
//    * such as `LinearSolution` and `QuadraticSolution`. For other cases, it defers
//    * to the superclass implementation.
//    *
//    * @param that the `Eager` instance to compare with the current solution
//    * @return a `Try[Boolean]` indicating whether the two solutions are equivalent.
//    *         The result is wrapped in a `Try` to account for potential errors during comparison.
//    */
//  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
//    case (a: LinearSolution, b: LinearSolution) =>
//      a.value.eqv(b.value)
//    case (a: QuadraticSolution, b: QuadraticSolution) =>
//      for {
//        baseEq <- a.base.eqv(b.base)
//        offsetEq <- a.offset.eqv(b.offset)
//      } yield baseEq && offsetEq && a.coefficient == b.coefficient
//    case _ =>
//      super.eqv(that)
//  }

  /**
    * Performs a fuzzy equivalence comparison between the current solution and another solution.
    * The comparison allows for a degree of tolerance specified by the parameter `p`.
    *
    * @param p    the tolerance level for the fuzzy equivalence comparison; smaller values impose stricter equality conditions
    * @param that the other solution to compare against, which must be of type `Eager`
    * @return a `Try[Boolean]` indicating whether the two solutions are approximately equivalent
    *         within the specified tolerance, or an error if the comparison cannot be performed
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: LinearSolution, b: LinearSolution) =>
      a.value.fuzzyEqv(p)(b.value)
    case (a: QuadraticSolution, b: QuadraticSolution) =>
      if (a.coefficient != b.coefficient) {
        Success(false)
      } else {
        for {
          baseEq <- a.base.fuzzyEqv(p)(b.base)
          offsetEq <- a.offset.fuzzyEqv(p)(b.offset)
        } yield baseEq && offsetEq
      }
    case _ =>
      super.fuzzyEqv(p)(that)
  }
}

/**
  * Represents a mathematical solution, which can be either linear or quadratic in nature.
  * This object provides factory methods for creating instances of `Algebraic`.
  */
object Algebraic {
  /**
    * Creates an `Algebraic` from a given `Rational` value.
    *
    * @param r the rational value used to construct the solution
    * @return an `Algebraic` instance constructed from the given rational value
    */
  def apply(r: Rational): Algebraic =
    LinearSolution(RationalNumber(r))

  /**
    * Constructs an `Algebraic` based on the provided `base` and `offset` monotone values
    * and a specified `coefficient` for the offset. If the `offset` is zero, a `LinearSolution`
    * is returned; otherwise, a `QuadraticSolution` is returned.
    *
    * @param base   the base monotone component of the solution
    * @param offset the offset monotone component used to determine whether the solution
    *               is linear or quadratic
    *
    * @param coefficient an integer used in cases where a quadratic solution is required
    * @return an `Algebraic` instance, which is either a `LinearSolution` or a `QuadraticSolution`
    */
  def apply(base: Structure, offset: Structure, coefficient: Int): Algebraic =
    if (offset.isZero)
      LinearSolution(base)
    else
      QuadraticSolution(base, offset, coefficient, false)

  given DyadicOperator[Algebraic] = new DyadicOperator[Algebraic] {
    def op[B <: Algebraic, Z](f: (Algebraic, B) => Try[Z])(x: Algebraic, y: B): Try[Z] =
      f(x, y)
  }

  given Eq[Algebraic] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Algebraic]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Algebraic] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * LatexRenderer for Algebraic (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val algebraicLatexRenderer: LatexRenderer[Algebraic] = LatexRenderer.instance {
    case q: QuadraticSolution => q.toLatex
    case l: LinearSolution => l.toLatex
  }
}

/**
  * Represents a solution to a quadratic equation in the context of a specific Factor.
  *
  * @constructor Creates a new instance of QuadraticSolution.
  * @param base   The base Value component of the solution.
  * @param offset The offset Value component of the solution, usually derived from the discriminant.
  *
  *               QuadraticSolution models the structure of solutions to equations of the form:
  *               x&#94;2 + px + q = 0.
  *               It encapsulates components necessary for representing and interpreting the result
  *               in terms of its `base`, the `offset` typically arising from the quadratic formula's discriminant,
  *               and any contextual modifications by a `factor`.
  *
  *               Instances of this class are designed to interact with the `Factor` type in performing various
  *               operations such as evaluating, converting, or rendering the solution accurately in diverse contexts.
  */
case class QuadraticSolution(base: Structure, offset: Structure, coefficient: Int, imaginary: Boolean = false)(val maybeName: Option[String] = None) extends Algebraic {

  /**
    * Normalizes this `Valuable` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest `Valuable` representation of this value
    */
  def normalize: Eager = (base.normalize, offset.normalize) match {
    case (x: eager.Number, y: eager.Number) =>
      x + y.scale(coefficient).asInstanceOf[eager.Number]
    case (x: Structure, y: Scalar) if x.isZero =>
      y.scale(coefficient)
    case (x, y: Structure) if y.isZero =>
      x
    case (x: Structure, y) if x.isZero =>
      y
    case _ =>
      this
  }

  /**
    * If this `Solution` can be represented as a `Structure`, return it wrapped in `Some`, otherwise return `None`.
    *
    * @return an `Option[Structure]`.
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the conversion is not supported.
    */
  def toMonotone: Option[Structure] = (base, offset) match {
    case (b, o) if o.isZero => Some(b)
    case (b, o) if b.isZero => Some(o)
    case _ if isPureNumber => throw AlgebraException("toMonotone: this case not yet implemented")
    case _ => None
  }

  /**
    * Method to render this NumberLike in a presentable manner.
    * CONSIDER improve this in the event that offset itself is negative (as opposed to imaginary).
    * However, that scenario is unlikely, I believe.
    *
    * @return a String
    */
  def render: String = maybeName getOrElse (this match {
    case q@QuadraticSolution(base, offset, coefficient, false) if offset.isZero || base.isZero =>
      q.normalize.render
    case QuadraticSolution(base, offset, coefficient, false) =>
      val offStr = if (offset.isZero || coefficient == 0) "" else s"${offset.render}"
      s"(${base.render} ${if (coefficient == 1) "+ " else "- "}$offStr)"
    case _ =>
      // TODO this is not correct in the cases where coefficient is not 1 or -1
      s"Complex quadratic solution: ${base.render} ${if (coefficient == -1) "- " else "+ "}${offset.render}"
  })

  /**
    * Computes the conjugate of the current quadratic solution.
    *
    * The conjugate is calculated by inverting the coefficient of the solution,
    * effectively toggling between the two branches of the quadratic equation.
    *
    * @return a new QuadraticSolution instance representing the conjugate of the current solution
    */
  def conjugate: QuadraticSolution =
    copy(coefficient = -coefficient)(None)

  /**
    * Negates the current quadratic solution by applying a negation
    * operation to its `offset` component and then computing the conjugate.
    *
    * The negation is accomplished by using the `Value.negate` function on
    * the solution's `offset`, and the resulting solution's conjugate
    * is returned as a new instance of `QuadraticSolution`.
    *
    * @return a new QuadraticSolution instance that represents the negation and conjugation
    *         of the current quadratic solution
    */
  def negate: QuadraticSolution =
    copy(base = base.negate)(None).conjugate

  /**
    * Checks if the solution represented by this instance is equivalent to zero.
    *
    * A solution is considered zero if both its `base` and `offset` components are zero values.
    *
    * @return true if both the `base` and `offset` are zero, otherwise false.
    */
  def isZero: Boolean =
    base.isZero && isPureNumber

  /**
    * Determines if the current quadratic solution represents a pure number.
    *
    * A solution is considered a pure number if its offset component is zero.
    * This method relies on the `Value.isZero` utility to check whether the
    * offset is zero.
    *
    * @return true if the offset of the solution is zero, otherwise false.
    */
  def isPureNumber: Boolean =
    coefficient == 0 || offset.isZero || offset.maybeFactor(RestrictedContext(PureNumber)).isDefined

  /**
    * Determines if the current QuadraticSolution instance represents unity (1).
    *
    * The solution is considered unity if its `base` component is equivalent to one
    * (as verified by the conversion to a Rational) and its `offset` is zero.
    *
    * @return true if the solution represents unity, otherwise false.
    */
  def isUnity: Boolean =
    isPureNumber && base.isUnity

  /**
    * Determines the "sign" of the current quadratic solution.
    *
    * This method utilizes the `signum` method from the `asField` representation
    * of the solution to evaluate its sign. The sign can indicate whether the
    * solution is positive, negative, or zero based on its position relative to
    * the origin in the Field domain.
    *
    * @return +1 if the solution is positive, -1 if negative, or 0 if it is at the origin.
    */
  def signum: Int = toDouble.sign.toInt // CHECK

  import scala.language.implicitConversions

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
  def approximation(force: Boolean): Option[Real] =
    FP.whenever(!isExact || !offset.isZero || force)(
      for {
        x <- base.convert(Real.one)
        y <- offset.convert(Real.one)
        z <- y.scale(coefficient).convert(Real.one)
      } yield (x + z).normalize
    )

  /**
    * Adds a `Rational` value to the current solution and returns a new `Algebraic` as the result.
    * NOTE this only affects the `base` of this `Algebraic`.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Algebraic` instance representing the sum of the current solution and the given `addend`
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the addition with the specified solution is unsupported or leads to an error
    */
  def add(addend: Rational): Algebraic = {
    val zo: Option[Structure] = for {
      case numerical.Real(n) <- Valuable.valuableToMaybeField(base) if n.factor == PureNumber
      value <- doComposeValueDyadic(n.nominalValue, fromRational(addend))(DyadicOperationPlus.functions)
      field = numerical.Real(ExactNumber(value, PureNumber))
      case m: Structure <- Some(Eager(field))
    } yield m
    FP.recover(zo.map(z => copy(base = z)(None)))(AlgebraException(s"QuadraticSolution: add($addend) not supported"))
  }

  /**
    * Adds the specified solution to this quadratic solution, producing a new solution as a result.
    *
    * This method supports the addition of solutions of type `Algebraic`. If the provided solution
    * is of an unsupported type, an `AlgebraException` is thrown. When the solution is an instance
    * of `Algebraic`, the addition is attempted using the `add` method, and any potential errors
    * are recovered into an exception.
    *
    * @param other the solution to be added to this quadratic solution
    * @return a new `Solution` representing the sum of this quadratic solution and the specified solution
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the addition with the specified solution is unsupported or leads to an error
    */
  def +(other: Solution): Solution = other match {
    case algebraic: Algebraic =>
      add(algebraic).getOrElse(throw AlgebraException(s"QuadraticSolution: +($other) not supported"))
    case _ =>
      throw AlgebraException(s"QuadraticSolution: +($other) not supported")
  }

  /**
    * Adds the specified solution to the current solution and returns an optional new solution
    * that represents the result of the addition. The addition occurs only when
    * the `offset`, `factor`, and `negative` properties of both solutions match the
    * specified conditions.
    *
    * CONSIDER rewriting this method in the style of its cousins.
    *
    * @param solution the solution to be added to the current solution
    * @return an Option containing the resulting solution after the addition if the conditions are met, or None otherwise
    */
  override def add(solution: Eager): Try[Algebraic] = (this, solution) match {
    // NOTE special case where we add two different branches of the same pair of solutions (Not sure we really need this special case).
    case (x@QuadraticSolution(bx: eager.Number, ox, cx, ix), y@QuadraticSolution(by: eager.Number, oy, cy, iy)) if ox === oy && ix == iy =>
      Success(QuadraticSolution((bx + by).asMonotone, Eager.zero.asMonotone, cx + cy, ix)(None))
    case (x@QuadraticSolution(bx: eager.Number, ox: Scalar, nx, ix), y@QuadraticSolution(by: eager.Number, oy: Scalar, ny, iy)) if ix == iy =>
      val basePart: Eager = bx + by
      val qx = ox.scale(nx)
      val qy = oy.scale(ny)
      (qx, qy) match {
        case (q: RationalNumber, r: RationalNumber) if q.isZero && r.isZero =>
          Success(QuadraticSolution(basePart.asMonotone, (q + r).asMonotone, 1, ix)(None))
        case _ =>
          Failure(AlgebraException(s"QuadraticSolution.add($solution)"))
      }
    case (x@QuadraticSolution(bx: eager.Number, ox: InversePower, nx, ix), y@QuadraticSolution(by: eager.Number, oy: InversePower, ny, iy)) if ox == oy && ix == iy =>
      val basePart: Eager = bx + by
      val offsetPart: Eager = ox.*(nx + ny)
      Success(QuadraticSolution(basePart.asMonotone, offsetPart.asMonotone, 0, ix)(None))
    case (x@QuadraticSolution(bx: eager.Number, ox: Scalable[eager.Number] @unchecked, nx, ix), y: eager.Number) =>
      Success(QuadraticSolution((bx + y).asMonotone, ox, nx, ix)(None))
    // TODO add more cases (see commented code below)
      // Issue #147
    case _ =>
      Failure(AlgebraException(s"QuadraticSolution.add($solution)"))
  }

  /**
    * Computes the square of the current instance by multiplying it with itself.
    *
    * @return The result of squaring the current instance.
    */
  def square: Eager = this * this

  def *(other: QuadraticSolution): Eager = (this, other) match {
    // XXX Special case where both ox terms are SquareRoots
    case (x@QuadraticSolution(bx: eager.Number, ox@InversePower(2, px), nx, false), y@QuadraticSolution(by: eager.Number, oy@InversePower(2, py), ny, false)) if px == py =>
      val rx = RationalNumber(nx)
      val ry = RationalNumber(ny)
      val bb = bx * ry + by * rx
      // TODO cast
      val o: Structure = if (bb.isZero) eager.Number.zero.asInstanceOf[Structure] else InversePower(2, bb * bb * px)
      val pr = px * rx * ry
      QuadraticSolution(bx * by + pr, o, bb.signum, false).normalize
    case (x@QuadraticSolution(bx: eager.Number, ox: eager.Number, nx, false), y@QuadraticSolution(by: eager.Number, oy: eager.Number, ny, false)) =>
      val rx = RationalNumber(nx)
      val ry = RationalNumber(ny)
      val t1: eager.Number = bx * by
      val t2: eager.Number = bx * ry * oy + by * rx * ox
      val t3: eager.Number = rx * ry * ox * oy
      QuadraticSolution((t1 + t2 + t3).asMonotone, eager.Number.zero.asMonotone, 1, false).normalize
    case _ =>
      throw AlgebraException(s"QuadraticSolution: $this*$other not supported")
  }

  // TODO this needs more work and testing.
  def multiply(solution: Algebraic): Option[Eager] = (this, solution) match {
    // XXX Special case where both ox terms are SquareRoots
    case (x@QuadraticSolution(_, ox@InversePower(2, px), _, _), y@QuadraticSolution(_, oy@InversePower(2, py), _, _)) if px == py =>
      Some(x * y)
    case (x: QuadraticSolution, y: QuadraticSolution) =>
      Some(x * y)
    // CONSIDER add more cases
    case _ =>
      None
  }

  /**
    * Scales the quadratic solution using a given rational factor.
    *
    * This method computes a new quadratic solution by scaling the current
    * solution's components (`base` and `offset`) with a specified rational
    * multiplier. If any intermediate calculation fails, it returns `None`.
    *
    * @param r the scaling factor as a Rational value
    * @return an Option containing the scaled quadratic solution if calculations succeed, otherwise None
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the scaling operation is not supported.
    */
  def *(r: Rational): Algebraic = (base, offset) match {
    case (b: Scalar, o: Scalar) =>
      QuadraticSolution(b.scale(r), o.scale(r), coefficient, imaginary)
    case (b: Scalar, o@InversePower(2, x)) =>
      val number = RationalNumber(r)
      val br: Int = if (number.signum < 0) 1 - coefficient else coefficient
      val offsetValue: Structure = if (number.isZero) WholeNumber.zero else InversePower(2, x * number * number)
      val value = QuadraticSolution(b.scale(r), offsetValue, br, imaginary)
      value
    case _ =>
      throw AlgebraException(s"QuadraticSolution: *($r) not supported for $base, $offset")
  }

  /**
    * Scales the current solution by a given rational number.
    * TODO do we really need both `*` and `scale`?
    *
    * @param r The rational number by which to scale the current solution.
    * @return A new scaled solution.
    */
  def scale(r: Rational): Solution = *(r)

  /**
    * Determines equivalence between the current `Eager` instance and another `Eager` instance.
    *
    * This method checks if the current instance and the provided instance represent equivalent
    * quadratic solutions. It performs a specific comparison for instances of `QuadraticSolution`,
    * verifying equality of their `base`, `offset`, and `coefficient` components. For other cases,
    * it delegates the equivalence check to the superclass implementation.
    *
    * @param that the other `Eager` instance to be compared for equivalence
    * @return `Try[Boolean]` indicating either the result of the equivalence comparison or an
    *         error if the comparison cannot be performed
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: QuadraticSolution, b: QuadraticSolution) =>
      Success(a.base === b.base && a.offset === b.offset && a.coefficient == b.coefficient)
    case (a: QuadraticSolution, b: eager.ExactNumber) =>
      Success(a.base === b && a.offset.isZero)
    case (a: QuadraticSolution, b: eager.InversePower) =>
      Success(a.offset === b && a.base.isZero)
    // TODO we need the inverses of these cases implemented.
    case _ =>
      super.eqv(that)
  }

  /**
    * Performs a fuzzy equivalence comparison between the current solution and another solution.
    * The comparison allows for a degree of tolerance specified by the parameter `p`.
    *
    * @param p    the tolerance level for the fuzzy equivalence comparison; smaller values impose stricter equality conditions
    * @param that the other solution to compare against, which must be of type `Eager`
    * @return a `Try[Boolean]` indicating whether the two solutions are approximately equivalent
    *         within the specified tolerance, or an error if the comparison cannot be performed
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] =
    (this, that) match {
      case (a: QuadraticSolution, b: QuadraticSolution) =>
        for {
          baseEqv <- a.base.fuzzyEqv(p)(b.base)
          offsetEqv <- a.offset.fuzzyEqv(p)(b.offset)
        } yield baseEqv && offsetEqv && a.coefficient == b.coefficient
      case _ =>
        super.fuzzyEqv(p)(that)
    }
}

/**
  * Object containing predefined solutions to a quadratic equation and utilities.
  *
  * The object includes two specific solutions (`phi` and `psi`) that are parametrized
  * using the `QuadraticSolution` class, demonstrating the representation of roots
  * of a quadratic equation. These roots are computed using predefined rational
  * coefficients and specified root indices.
  */
object QuadraticSolution {

  /**
    * Creates a new instance of `QuadraticSolution` using the provided base monotone,
    * offset monotone, and coefficient index.
    *
    * @param base   the base component of the quadratic solution, represented as a `Structure`
    * @param offset the offset component of the quadratic solution, represented as a `Structure`
    * @param coefficient an integer specifying the coefficient of the solution
    * @return a new `QuadraticSolution` instance constructed from the provided parameters
    */
  def apply(base: Structure, offset: Structure, coefficient: Int, imaginary: Boolean): QuadraticSolution =
    (base.normalize, offset.normalize) match {
      case (b: Scalar, o: Scalar) =>
        new QuadraticSolution(b, o, coefficient, imaginary)(None)
      case _ =>
        new QuadraticSolution(base, offset, coefficient, imaginary)(None)
    }

  /**
    * Represents the value `phi`, which is a solution of a quadratic equation
    * derived using `QuadraticSolution`. The solution is constructed from:
    * - The first coefficient provided by converting `1/2` into a `Value` using `Value.fromRational`.
    * - The second coefficient provided by converting `5/4` into a `Value` using `Value.fromRational`.
    * - The root type specified as `SquareRoot`.
    * - The root index specified as `0`, which identifies this as one of the possible roots.
    */
  val phi: QuadraticSolution = new QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(5, 4)), 1)(Some("𝛗"))
  /**
    * Represents a specific quadratic solution characterized by its parameters.
    *
    * @see QuadraticSolution
    * @see Value.fromRational
    */
  val psi: QuadraticSolution = new QuadraticSolution(RationalNumber.half, InversePower(2, RationalNumber(5, 4)), -1)(Some("𝛙"))

  /**
    * Calculates the square root of a given rational number as a monotone function.
    *
    * @param r the rational number for which the square root is to be computed.
    * @return a monotone representation of the square root of the given rational number.
    */
  def squareRoot(r: RationalNumber): Structure =
    InversePower(2, r).normalize.asMonotone

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[QuadraticSolution] = new DyadicOperator[QuadraticSolution] {
    def op[B <: QuadraticSolution, Z](f: (QuadraticSolution, B) => Try[Z])(x: QuadraticSolution, y: B): Try[Z] =
      f(x, y)
  }

  given Eq[QuadraticSolution] = Eq.instance {
    (x, y) =>
      FP.toOptionWithLog(logger.warn("Eq[QuadraticSolution]", _))(x.eqv(y)).getOrElse(false)
  }

  given FuzzyEq[QuadraticSolution] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || FP.toOptionWithLog(logger.warn("FuzzyEq[QuadraticSolution]", _))(x.fuzzyEqv(p)(y)).getOrElse(false)
  }

  /**
    * LatexRenderer for QuadraticSolution.
    *
    * Renders as:
    * - Named constants (φ, ψ) use their LaTeX symbols
    * - Real roots: base ± offset
    * - Complex roots: base ± i·offset
    * - Normalizes when appropriate (zero offset or zero base)
    */
  implicit val quadraticSolutionLatexRenderer: LatexRenderer[QuadraticSolution] = LatexRenderer.instance {
    case QuadraticSolution.phi =>
      "\\varphi"  // or "\\phi" depending on style preference

    case QuadraticSolution.psi =>
      "\\psi"

    case QuadraticSolution(base, offset, _, false) if offset.isZero =>
      base.toLatex

    case q@QuadraticSolution(base, offset, coefficient, false) if base.isZero =>
      if (coefficient == 1) {
        offset.toLatex
      } else if (coefficient == -1) {
        s"-${offset.toLatex}"
      } else {
        s"${Rational(coefficient).toLatex} \\cdot ${offset.toLatex}"
      }

    case QuadraticSolution(base, offset, coefficient, false) =>
      val signStr = LatexRenderer.sign(coefficient == 1)
      s"${base.toLatex} $signStr ${offset.toLatex}"

    case QuadraticSolution(base, offset, coefficient, true) =>
      val signStr = LatexRenderer.sign(coefficient == 1)
      s"${base.toLatex} $signStr i ${offset.toLatex}"
  }

}

/**
  * Represents a linear solution characterized by a single `value`.
  * Implements the `Algebraic` trait where the `base` is equal to the `value`,
  * the `offset` is `Value.zero`, and the `factor` is `PureNumber`.
  * CONSIDER we should not have to deal with `offset, factor, negative` attributes here.
  *
  * @param value the base value of the solution
  */
case class LinearSolution(value: Structure)(val maybeName: Option[String] = None) extends Algebraic {
  /**
    * Returns the number of branches in `this`.
    *
    * @return 1.
    */
  def branches: Int = 1

  /**
    * Computes and returns the conjugate of the current `Solution` instance.
    *
    * The conjugate represents a value or transformation that is mathematically
    * paired oppositely with the current instance based on the structure and
    * properties of the `Solution` implementation.
    *
    * Normally, the `coefficient` of the conjugate, added to the current `coefficient` should equal `branches`.
    *
    * @return a new `Solution` instance representing the conjugate of the current instance
    */
  def conjugate: Solution = this // CONSIDER throwing an exception instead.

  /**
    * Normalizes the current instance of `LinearSolution` to its simplest equivalent form.
    * The normalization process reduces the representation while maintaining the same value.
    *
    * This may simplify complex expressions or adjust the type to a more precise representation.
    *
    * @return the simplest `Valuable` representation of this value.
    */
  def normalize: Eager = value.normalize

  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Structure = normalize.asMonotone

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Structure = eager.Number.zero

  /**
    * If this `Solution` can be represented as a `Structure`, return it wrapped in `Some`, otherwise return `None`.
    *
    * @return an `Option[Structure]`.
    */
  def toMonotone: Option[Structure] = Some(value)

  /**
    * Determines whether the solution is a pure number.
    * A pure number is defined as one without any associated factors or offsets.
    *
    * @return true if the solution is a pure number, false otherwise
    */
  def isPureNumber: Boolean = true

  /**
    * Determines if the offset is negative.
    * A negative value may not be expressible with just `Value` and `Factor`, for example, if `factor` is `SquareRoot`.
    *
    * @return true if the offset is negative, false otherwise
    */
  def coefficient: Int = 0

  /**
    * Scales the current `Solution` instance by a given `Rational` factor.
    * CONSIDER this could be merged with the method whose name is the asterisk.
    *
    * The scaling operation adjusts the `Solution` based on the value of the provided
    * `Rational` parameter, resulting in a new `Solution` instance that reflects
    * the modification.
    *
    * @param r the `Rational` value by which to scale the current `Solution`
    * @return a new `Solution` instance representing the result of the scaling operation
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the scaling operation is not supported for the current `Solution` implementation.
    */
  def scale(r: Rational): Solution = value match {
    case scalar: Scalar =>
      LinearSolution(scalar.scale(r))
    case _ =>
      throw AlgebraException(s"LinearSolution: scale($r) not supported")
  }

  /**
    * Determines whether the solution is zero.
    *
    * @return true if the solution is zero, false otherwise.
    */
  def isZero: Boolean = value.isZero

  /**
    * Determines whether the solution represents unity.
    *
    * @return true if the solution represents unity, false otherwise
    */
  override def isUnity: Boolean = value.isUnity

  /**
    * Determines the sign of the solution.
    *
    * @return an integer representing the sign of the solution: 0 if the solution is zero,
    *         a positive value if the solution is positive, or a negative value if the solution is negative.
    */
  def signum: Int = value.signum

  /**
    * Negates the current instance of the solution by inverting its base and offset values.
    *
    * @return a new instance of `QuadraticSolution` representing the negated version of the current solution
    */
  def negate: Algebraic = LinearSolution(value.negate)

  /**
    * Adds the specified `Solution` to the current `LinearSolution` and returns a new `Solution`
    * representing the result of the addition. The operation is only supported for instances of
    * `LinearSolution`. If the addition is unsupported for the given `Solution`, an exception may
    * be thrown.
    *
    * @param other the `Solution` to add to the current `LinearSolution`
    * @return the resulting `Solution` of the addition if supported
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the operation is not valid for the given `Solution`
    */
  def +(other: Solution): Solution = other match {
    case linear: LinearSolution =>
      add(linear).getOrElse(throw AlgebraException(s"LinearSolution: +($other) not supported"))
    case _ =>
      throw AlgebraException(s"LinearSolution: +($other) not supported")
  }

  /**
    * Adds the specified solution to the current solution and returns
    * a new solution that represents the result of the addition.
    *
    * TODO implement me
    *
    * @param addend the Eager to be added to the current solution
    * @return an optional solution representing the sum of the current solution
    *         and the given solution, or None if the operation is not valid
    */
  override def add(addend: Eager): Try[Algebraic] = (value, addend) match {
    case (v: CanAdd[eager.Number, eager.Number] @unchecked, y@LinearSolution(x: eager.Number)) =>
      Success(LinearSolution(v + x))
    case (v: CanAdd[eager.Number, eager.Number], y: eager.Number) =>
      Success(LinearSolution(v + y))
    case _ =>
      Failure(AlgebraException(s"LinearSolution.add($addend)"))
  }

  /**
    * Adds a `Rational` value to the current solution and returns a new `Algebraic` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Algebraic` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Algebraic =
    add(RationalNumber(addend)) match {
      case Success(a) => a
      case Failure(e) => throw e
    }

  /**
    * Scales the given rational value using the current value to produce an optional solution.
    *
    * @param r the rational value used as a multiplier during scaling
    * @return an optional `Algebraic` as the result of scaling, or `None` if the operation is not valid
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the operation is not valid for the given `Solution`
    */
  def *(r: Rational): Algebraic = value match {
    case b: Scalar =>
      LinearSolution(b.scale(r))
    case _ =>
      throw AlgebraException(s"LinearSolution: *($r) not supported")
  }

  /**
    * Renders the solution as a string representation.
    * It attempts to use a rational representation of the value if available;
    * otherwise, it falls back to the default string representation of the solution.
    *
    * @return a string representation of the solution
    */
  def render: String =
    maybeName.getOrElse(value.render)

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
    * @return      an `Option` containing the approximate value as a `Real` if available,
    *              or `None` if no approximation can be computed.
    */
  def approximation(force: Boolean): Option[Real] =
    Some(Real(value.toDouble))

  /**
    * Determines if the current instance of `LinearSolution` is equivalent to another `Eager` instance.
    * The equivalence check is specific to the type of the `Eager` instance.
    *
    * @param that the `Eager` instance to compare for equivalence
    * @return a `Try[Boolean]` indicating the result of the equivalence check
    *         - `Success(true)` if the instances are considered equivalent
    *         - `Success(false)` if the instances are not equivalent
    *         - `Failure` if the operation is not supported or if an error occurs during the comparison
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: LinearSolution, b: LinearSolution) =>
      a.value.eqv(b.value)
    case _ =>
      Failure(AlgebraException(s"LinearSolution: eqv($that) not supported"))
  }
}

/**
  * The `LinearSolution` object provides utility functions for creating and manipulating instances
  * of `LinearSolution`. It includes methods and typeclass instances for operations such as equality
  * comparison and fuzzy equality checks.
  *
  * The object also initializes the logger and defines givens for specific typeclass operations
  * such as `DyadicOperator`, `Eq`, and `FuzzyEq`, allowing more advanced handling of `LinearSolution` instances.
  */
object LinearSolution {

  /**
    * Constructs a new instance of `LinearSolution` using the provided `Structure` value.
    *
    * @param value the `Structure` instance used to initialize the `LinearSolution`
    * @return a new `LinearSolution` instance corresponding to the given `Structure`
    */
  def apply(value: Structure): LinearSolution = new LinearSolution(value)()

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  // CONSIDER do we need this?
  given DyadicOperator[LinearSolution] = new DyadicOperator[LinearSolution] {
    def op[B <: LinearSolution, Z](f: (LinearSolution, B) => Try[Z])(x: LinearSolution, y: B): Try[Z] =
      f(x, y)
  }

  given Eq[LinearSolution] = Eq.instance {
    (x, y) =>
      FP.toOptionWithLog(logger.warn("Eq[LinearSolution]", _))(x.eqv(y)).getOrElse(false)
  }

  given FuzzyEq[LinearSolution] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || FP.toOptionWithLog(logger.warn("FuzzyEq[LinearSolution]", _))(x.fuzzyEqv(p)(y)).getOrElse(false)
  }

  /**
    * LatexRenderer for LinearSolution.
    *
    * Simply renders the value.
    */
  implicit val linearSolutionLatexRenderer: LatexRenderer[LinearSolution] = LatexRenderer.instance { ls =>
    ls.value.toLatex
  }
}