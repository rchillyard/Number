/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.misc.{AlgebraException, DyadicOperator, FP, FuzzyEq}
import com.phasmidsoftware.number.core.algebraic.Algebraic
import com.phasmidsoftware.number.core.inner.Operations.doComposeValueDyadic
import com.phasmidsoftware.number.core.inner.Value.fromRational
import com.phasmidsoftware.number.core.inner.{DyadicOperationPlus, Factor, PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ExactNumber, Field}
import org.slf4j.{Logger, LoggerFactory}
import scala.util.{Success, Try}

/**
  * Trait to model the behavior of a solution to an equation.
  * All such solutions are exact.
  * Typically, however, such solutions cannot be represented exactly as pure numbers.
  * The four parameters of a Solution are: base, offset, branch, and factor.
  * Solutions are subject to various operations such as addition, scaling, and number conversions.
  * Extends the `NumberLike` trait, inheriting behavior common to number-like entities.
  *
  * CONSIDER it is inappropriate to include branch in `Solution`. A `Solution` should be a unique solution.
  */
trait Solution extends Eager {
  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Monotone

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Monotone

  /**
    * Branch number runs from `0` to `n-1` where `n` is the number of branches (or roots).
    * For a polynomial of degree `n`, there will, in general be `n` branches or roots.
    * Branch 0 is always the branch with the smallest positive offset.
    *
    * @return the branch number.
    */
  def branch: Int

  /**
    * Determines the factor associated with this solution based on certain conditions.
    * If the solution is a pure number, it returns `Some(PureNumber)`.
    * If the base value of the solution is zero, it returns `Some(factor)`.
    * Otherwise, it returns `None`.
    *
    * @return an `Option` of type `Factor`, where `Some` may contain the factor
    *         depending on the conditions, or `None` if no factor is applicable
    */
  def maybeFactor(context: Context): Option[Factor] =
    if (isPureNumber) {
      Option.when(context.factorQualifies(PureNumber))(PureNumber)
    }
    else if (base.isZero)
      offset.maybeFactor(context)
    else
      None

  /**
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = true

  /**
    * Determines whether the solution is a pure number.
    * A pure number is defined as one without any associated factors or offsets.
    *
    * @return true if the solution is a pure number, false otherwise
    */
  def isPureNumber: Boolean

  /**
    * Determines whether the solution is zero.
    *
    * @return true if the solution is zero, false otherwise.
    */
  def isZero: Boolean

  /**
    * Determines whether the solution represents unity.
    *
    * @return true if the solution represents unity, false otherwise
    */
  def isUnity: Boolean

  /**
    * Determines the sign of the solution.
    *
    * @return an integer representing the sign of the solution: 0 if the solution is zero,
    *         a positive value if the solution is positive, or a negative value if the solution is negative.
    */
  def signum: Int

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution

  /**
    * Adds the given solution to the current solution and returns an optional result.
    * The addition may fail under certain conditions, in which case `None` is returned.
    *
    * @param solution the `Solution` to be added to the current solution
    * @return an `Option` containing the resulting `Solution` if the addition is successful,
    *         or `None` if the addition cannot be performed
    */
  def add(solution: Solution): Option[Solution]

  /**
    * Negates the current solution by applying a unary negation operation.
    *
    * @return a new `Solution` instance representing the negated value of the current solution
    */
  def negate: Solution

  /**
    * Adds the given solution to the current solution and returns an optional result.
    * The addition may fail under certain conditions, in which case `None` is returned.
    *
    * @param solution the `Solution` to be added to the current solution
    * @return an `Option` containing the resulting `Solution` if the addition is successful,
    *         or `None` if the addition cannot be performed
    */
  def subtract(solution: Solution): Option[Solution] =
    add(solution.negate)

  /**
    * Scales the current solution by multiplying its base and offset values by the specified `Rational` factor.
    *
    * @param r the `Rational` value used as the scaling factor
    * @return an optional `Solution` instance, where `Some` contains the scaled solution if the operation is valid,
    *         or `None` if scaling cannot be performed
    */
  def scale(r: Rational): Option[Solution]

  /**
    * Determines whether the current solution is equivalent to another solution.
    * This method performs a structural comparison for specific solution types
    * such as `LinearSolution` and `QuadraticSolution`. For other cases, it defers
    * to the superclass implementation.
    *
    * @param that the `Eager` instance to compare with the current solution
    * @return a `Try[Boolean]` indicating whether the two solutions are equivalent.
    *         The result is wrapped in a `Try` to account for potential errors during comparison.
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: LinearSolution, b: LinearSolution) =>
      a.value.eqv(b.value)
    case (a: QuadraticSolution, b: QuadraticSolution) =>
      for {
        baseEq <- a.base.eqv(b.base)
        offsetEq <- a.offset.eqv(b.offset)
      } yield baseEq && offsetEq && a.branch == b.branch
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
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: LinearSolution, b: LinearSolution) =>
      a.value.fuzzyEqv(p)(b.value)
    case (a: QuadraticSolution, b: QuadraticSolution) =>
      if (a.branch != b.branch) {
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
  * This object provides factory methods for creating instances of `Solution`.
  */
object Solution {
  /**
    * Creates a `Solution` from a given `Rational` value.
    *
    * @param r the rational value used to construct the solution
    * @return a `Solution` instance constructed from the given rational value
    */
  def apply(r: Rational): Solution =
    LinearSolution(RationalNumber(r))

  /**
    * Constructs a `Solution` based on the provided `base` and `offset` monotone values
    * and a specified `branch` identifier. If the `offset` is zero, a `LinearSolution`
    * is returned; otherwise, a `QuadraticSolution` is returned.
    *
    * @param base   the base monotone component of the solution
    * @param offset the offset monotone component used to determine whether the solution
    *               is linear or quadratic
    *
    * @param branch an integer identifier used in cases where a quadratic solution is required
    * @return a `Solution` instance, which is either a `LinearSolution` or a `QuadraticSolution`
    */
  def apply(base: Monotone, offset: Monotone, branch: Int): Solution =
    if (offset.isZero)
      LinearSolution(base)
    else
      QuadraticSolution(base, offset, branch)

  given DyadicOperator[Solution] = new DyadicOperator[Solution] {
    def op[B <: Solution, Z](f: (Solution, B) => Try[Z])(x: Solution, y: B): Try[Z] =
      f(x, y)
  }

  given Eq[Solution] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[Solution]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  given FuzzyEq[Solution] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }
}

/**
  * Represents a solution to a quadratic equation in the context of a specific Factor.
  *
  * @constructor Creates a new instance of QuadraticSolution.
  * @param base   The base Value component of the solution.
  * @param offset The offset Value component of the solution, usually derived from the discriminant.
  * @param factor The Factor associated with the solution, influencing conversions and evaluations.
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
case class QuadraticSolution(base: Monotone, offset: Monotone, branch: Int) extends Solution {

  /**
    * Method to render this NumberLike in a presentable manner.
    * CONSIDER improve this in the event that offset itself is negative (as opposed to imaginary).
    * However, that scenario is unlikely, I believe.
    *
    * @return a String
    */
  def render: String = this match {
    case QuadraticSolution.phi =>
      Algebraic.phi.render
    case QuadraticSolution.psi =>
      Algebraic.psi.render
    case q@QuadraticSolution(base, offset, branch) if offset.isZero || base.isZero =>
      q.render
    case QuadraticSolution(base, offset, branch) =>
      s"Solution: $base + ${if (branch == 1) "- " else "+ "}"
  }

  override def toString: String = render

  /**
    * Computes the conjugate of the current quadratic solution.
    *
    * The conjugate is calculated by inverting the branch of the solution,
    * effectively toggling between the two branches of the quadratic equation.
    *
    * @return a new QuadraticSolution instance representing the conjugate of the current solution
    */
  def conjugate: QuadraticSolution =
    copy(branch = 1 - branch)

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
    copy(offset = offset.negate).conjugate

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
    offset.isZero

  /**
    * Determines if the current QuadraticSolution instance represents unity (1).
    *
    * The solution is considered unity if its `base` component is equivalent to one
    * (as verified by the conversion to a Rational) and its `offset` is zero.
    *
    * @return true if the solution represents unity, otherwise false.
    */
  def isUnity: Boolean =
    isPureNumber && base == Eager.one

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
//
//  /**
//    * Converts a given `Number` into a `Field`.
//    *
//    * TODO change this method to be non-implicit. Implicit conversions are evil.
//    * Don't forget to remove the implicitConversions flag from other sources.
//    *
//    * @param x the `Number` to be converted into a `Field`.
//    * @return a `Field` representation of the provided `Number`.
//    */
//  implicit def convertFromNumber(x: Number): Field =
//    Real(x)

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
      for {x <- base.convert(Real.one); y <- offset.convert(Real.one)} yield x + y
    )
//  /**
//    * Converts the current QuadraticSolution instance into a Field representation.
//    *
//    * TODO refactor this code using radicalTerm
//    *
//    * The method computes a Field value based on the `base`, `offset`, and `factor`.
//    * If the `offset` component is zero, the result is the `base` number as a Field.
//    * Otherwise, it incorporates the `offset` and possibly an imaginary component
//    * to produce either a ComplexCartesian number or a regular Field representation,
//    * depending on the properties of the given solution.
//    *
//    * @return a Field representation of the current QuadraticSolution instance
//    */
//  def asField: Field =
//    if ((offset.isZero))
//      ExactNumber(base, PureNumber)
//    else {
//      val offsetNumber: ExactNumber = ExactNumber(offset, factor)
//
//      def offsetToNumber = {
//        val maybeValue = factor.convert(offset, PureNumber) map (v => ExactNumber(v, PureNumber))
//        val maybeApproximation = offsetNumber.asNumber
//        FP.toTry(maybeValue orElse maybeApproximation, Failure(new Exception("QuadraticSolution.asField: factor.convert failed")))
//      }
//
//      if (offsetNumber.isImaginary) { // XXX offset is negative and factor is SquareRoot
//        val imaginaryPart = {
//          if (branch == 0) // TESTME
//            ExactNumber(Value.negate(offset), factor)
//          else
//            offsetToNumber.get // TODO convert to a proper exception
//        } // CONSIDER handling this unlikely but possible exception properly
//        ComplexCartesian(ExactNumber(base, PureNumber), imaginaryPart)
//      }
//      else {
//        val variablePart = offsetToNumber.get // CONSIDER handling this unlikely but possible exception properly
//        ExactNumber(base, PureNumber) + (if (branch == 1) variablePart.makeNegative else variablePart)
//      }
//    }

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    * NOTE this only affects the `base` of this `Solution`.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = {
    val zo: Option[Monotone] = for {
      case numerical.Real(n) <- Valuable.valuableToMaybeField(base) if n.factor == PureNumber
      value <- doComposeValueDyadic(n.nominalValue, fromRational(addend))(DyadicOperationPlus.functions)
      field = numerical.Real(ExactNumber(value, PureNumber))
      case m: Monotone <- Some(Eager(field))
    } yield m
    FP.recover(zo.map(z => copy(base = z)))(AlgebraException(s"QuadraticSolution: add($addend) not supported"))
  }

  private def sumBases(m1: Monotone, m2: Monotone): Monotone = ???

  /**
    * Adds the specified solution to the current solution and returns an optional new solution
    * that represents the result of the addition. The addition occurs only when
    * the `offset`, `factor`, and `negative` properties of both solutions match the
    * specified conditions.
    *
    * @param solution the solution to be added to the current solution
    * @return an Option containing the resulting solution after the addition if the conditions are met, or None otherwise
    */
  def add(solution: Solution): Option[Solution] = {
    (isPureNumber, solution.isPureNumber) match {
      case (true, true) => copy(base = sumBases(base, solution.base))
    }

    solution match {
      case q: QuadraticSolution =>
//        if (maybeFactor(AnyContext.asInstanceOf[Context]) == q.factor) {
//          // CONSIDER should we be using z here?
//          val (a, b, z) = (branch, q.branch) match {
//            case (0, _) => (this, q, branch)
//            case (_, 0) => (q, this, q.branch)
//            case _ => (this.conjugate, q.conjugate, 1 - branch)
//          }
//          for {
//            x <- doComposeValueDyadic(a.base, b.base)(DyadicOperationPlus.functions)
//            (v, f, z) <- factor.add(a.offset, b.offset, factor, b.branch == 1)
//            if f == factor && z.isEmpty
//          } yield Solution(x, branch = a.branch)
//        }
//        else
        None  // TESTME
    }
  }

  // TODO this needs more work and testing. For example, factors don't have to be the same.
  private def multiply(solution: Solution): Option[Solution] =
    solution match {
      case q: QuadraticSolution =>
//        if (factor == q.factor) {
//          // CONSIDER should we be using z here?
//          val (a, b, z) = (branch, q.branch) match {
//            case (0, _) => (this, q, branch)
//            case (_, 0) => (q, this, q.branch)
//            case _ => (this.conjugate, q.conjugate, 1 - branch)
//          }
//          for {
//            x <- doComposeValueDyadic(a.base, b.base)(DyadicOperationTimes.functions)
//            (v, f, z) <- factor.multiply(a.offset, b.offset, factor)
//            if f == factor && z.isEmpty
//          } yield Solution(x, branch = a.branch)
//        }
//        else
        None  // TESTME
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
    */
  def scale(r: Rational): Option[Solution] = (base, offset) match {
    case (b: Scalar, o: Scalar) =>
      Some(QuadraticSolution(b.scale(r), o.scale(r), branch))
  }

  /**
    * Determines equivalence between the current `Eager` instance and another `Eager` instance.
    *
    * This method checks if the current instance and the provided instance represent equivalent
    * quadratic solutions. It performs a specific comparison for instances of `QuadraticSolution`,
    * verifying equality of their `base`, `offset`, and `branch` components. For other cases,
    * it delegates the equivalence check to the superclass implementation.
    *
    * @param that the other `Eager` instance to be compared for equivalence
    * @return a `Try[Boolean]` indicating either the result of the equivalence comparison or an
    *         error if the comparison cannot be performed
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (a: QuadraticSolution, b: QuadraticSolution) =>
      Success(a.base === b.base && a.offset === b.offset && a.branch == b.branch)
    case _ =>
      super.eqv(that)
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
    * Represents the value `phi`, which is a solution of a quadratic equation
    * derived using `QuadraticSolution`. The solution is constructed from:
    * - The first coefficient provided by converting `1/2` into a `Value` using `Value.fromRational`.
    * - The second coefficient provided by converting `5/4` into a `Value` using `Value.fromRational`.
    * - The root type specified as `SquareRoot`.
    * - The root index specified as `0`, which identifies this as one of the possible roots.
    */
  val phi = QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 0)
  /**
    * Represents a specific quadratic solution characterized by its parameters.
    *
    * @see QuadraticSolution
    * @see Value.fromRational
    */
  val psi = QuadraticSolution(RationalNumber.half, RationalNumber(5, 4), 1)

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
}

/**
  * Represents a linear solution characterized by a single `value`.
  * Implements the `Solution` trait where the `base` is equal to the `value`,
  * the `offset` is `Value.zero`, and the `factor` is `PureNumber`.
  * CONSIDER we should not have to deal with `offset, factor, negative` attributes here.
  *
  * @param value the base value of the solution
  */
case class LinearSolution(value: Monotone) extends Solution {
  /**
    * Retrieves the base value of the solution.
    *
    * @return the base value of type Value
    */
  def base: Monotone = value

  /**
    * Retrieves the offset value of the solution.
    *
    * @return the offset value of type Value
    */
  def offset: Monotone = Number.zero

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
  def branch: Int = 0

  /**
    * Converts the solution to a representation in the Field domain.
    *
    * @return an instance of Field representing the solution.
    */
  def asField: Field = ???

  /**
    * Determines whether the solution is zero.
    *
    * @return true if the solution is zero, false otherwise.
    */
  def isZero: Boolean = ???

  /**
    * Determines whether the solution represents unity.
    *
    * @return true if the solution represents unity, false otherwise
    */
  def isUnity: Boolean = ???

  /**
    * Determines the sign of the solution.
    *
    * @return an integer representing the sign of the solution: 0 if the solution is zero,
    *         a positive value if the solution is positive, or a negative value if the solution is negative.
    */
  def signum: Int = ???

  /**
    * Negates the current instance of the solution by inverting its base and offset values.
    *
    * @return a new instance of `QuadraticSolution` representing the negated version of the current solution
    */
  def negate: Solution = ???

  /**
    * Adds the specified solution to the current solution and returns
    * a new solution that represents the result of the addition.
    *
    * TODO implement me
    *
    * @param solution the solution to be added to the current solution
    * @return an optional solution representing the sum of the current solution
    *         and the given solution, or None if the operation is not valid
    */
  def add(solution: Solution): Option[Solution] = ???

  /**
    * Adds a `Rational` value to the current solution and returns a new `Solution` as the result.
    *
    * @param addend the `Rational` value to be added to the current solution
    * @return a new `Solution` instance representing the sum of the current solution and the given `addend`
    */
  def add(addend: Rational): Solution = ???
//  {
//    val po = PureNumber.add(value, fromRational(addend), PureNumber)
//    po match {
//      case Some((v, PureNumber, None)) =>
//        LinearSolution(v)
//      case _ =>
//        throw new Exception("LinearSolution.add: PureNumber.add failed")
//    }
//  }

  /**
    * Scales the given rational value using the current value to produce an optional solution.
    *
    * @param r the rational value used as a multiplier during scaling
    * @return an optional `Solution` as the result of scaling, or `None` if the operation is not valid
    */
  def scale(r: Rational): Option[Solution] = value match {
    case b: Scalar => Some(LinearSolution(b.scale(r)))
    case _ => None
  }

  /**
    * Renders the solution as a string representation.
    * It attempts to use a rational representation of the value if available;
    * otherwise, it falls back to the default string representation of the solution.
    *
    * @return a string representation of the solution
    */
  def render: String =
    value.render

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
    Some(Real(value.toDouble))
}

object LinearSolution {

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

}