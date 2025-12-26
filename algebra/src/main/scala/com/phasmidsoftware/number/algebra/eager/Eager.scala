/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP}
import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.CoreExceptionWithCause
import com.phasmidsoftware.number.core.parse.NumberParser
import com.phasmidsoftware.number.core.{algebraic, inner, numerical}
import com.phasmidsoftware.number.{algebra, core}
import scala.Option.when
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Trait `Eager` extends `Valuable` and is used to represent entities that evaluate their values eagerly.
  * That's to say, `Valuable` objects that do not extend `Expression`.
  * At present, `Eager` is extended by `Structure, Complex`, and `Nat`.
  * CONSIDER splitting this off into its own file.
  *
  * Unlike lazy evaluation, eager evaluation computes and stores the value immediately when the entity is created
  * or instantiated. This behavior can be useful in scenarios where prompt computation is essential, and
  * deferred or lazy evaluation may introduce undesired complexities or delays.
  *
  * `Eager` does not introduce additional properties or methods but serves as a marker trait
  * that confirms the eager nature of an extending type.
  */
trait Eager extends Valuable with Approximate with DyadicOps {

  /**
    * Retrieves an optional name associated with this instance.
    *
    * @return an `Option[String]` containing the name if present, otherwise `None`
    */
  def maybeName: Option[String]

  /**
    * Normalizes this `Eager` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * @return the simplest representation of this value that is a subtype of `Eager`.
    */
  def normalize: Eager

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] =
    when(isExact)(toDouble)

  /**
    * Compares this `Eager` instance with another `Eager` instance for equality.
    * This method is unimplemented and currently returns a failure with an `AlgebraException`.
    *
    * @param that the `Eager` instance to compare against
    * @return a `Try[Boolean]` indicating whether the two instances are equal.
    *         As the method is unimplemented, it always returns a failure containing an `AlgebraException`.
    */
  def eqv(that: Eager): Try[Boolean] =
    Failure(AlgebraException(s"Eager.eqv: unimplemented compare $this and $that"))

  /**
    * Performs a fuzzy equality comparison between the current `Eager` instance and another `Eager` instance.
    * The comparison is based on a specified tolerance level.
    * This method is unimplemented and currently returns a failure containing an `AlgebraException`.
    *
    * @param p    the tolerance level (as a `Double`) to be used for the fuzzy equality comparison
    * @param that the `Eager` instance to compare against this instance
    * @return a `Try[Boolean]` indicating whether the two instances are fuzzy equal.
    *         As the method is unimplemented, it always returns a failure containing an `AlgebraException`.
    */
  def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] =
    Failure(AlgebraException(s"Eager.fuzzyEqv: unimplemented compare $this and $that"))

  /**
    * Compares two instances of `Eager` and determines their relative ordering.
    * This method is not yet implemented and will raise an `AlgebraException` if called.
    *
    * @param x the first `Eager` instance to compare
    * @param y the second `Eager` instance to compare
    * @return a `Try[Int]` that contains the comparison result:
    *         - `0` if `x` and `y` are equal,
    *         - a negative result if `x` is less than `y`,
    *         - and a positive result if `x` is greater than `y`.
    *         As the method is unimplemented, it will always return a failure with an `AlgebraException`.
    */
  def compare(x: Eager, y: Eager): Try[Int] =
    Failure(AlgebraException(s"Eager.compare: unimplemented compare $x and $y"))

  /**
    * Performs a fuzzy comparison between two `Eager` instances.
    * The comparison is based on a specified tolerance and determines their relative ordering.
    * If the comparison cannot be performed, an `AlgebraException` is returned as a failure.
    *
    * @param p the tolerance (as a `Double`) to be used for the comparison
    * @param x the first `Eager` instance to be compared
    * @param y the second `Eager` instance to be compared
    * @return a `Try[Int]` representing the result of the comparison:
    *         - A negative integer if `x` is less than `y`
    *         - Zero if `x` is approximately equal to `y` within the tolerance `p`
    *         - A positive integer if `x` is greater than `y`
    *         - A `Failure(AlgebraException)` if the comparison cannot be performed
    */
  def fuzzyCompare(p: Double)(x: Eager, y: Eager): Try[Int] =
    Failure(AlgebraException(s"Eager.fuzzyCompare: unimplemented compare $x and $y"))
}

/**
  * The `Eager` object provides factory methods to create instances of `Valuable` entities
  * that are evaluated eagerly. These entities can represent numerical values parsed from
  * strings, long integers, or specific types of mathematical fields.
  */
object Eager {
  lazy val zero: Eager = Number.zero
  lazy val one: Eager = Number.one
  lazy val minusOne: Eager = Number.minusOne
  lazy val two: Eager = Scalar(2)
  lazy val half: Eager = RationalNumber(Rational.half)
  lazy val ten: Eager = Scalar(10)
  lazy val pi: Eager = Angle.pi
  lazy val piBy2: Eager = Angle.piBy2
  lazy val piBy4: Eager = Angle.piBy4
  lazy val e: Eager = NatLog.e
  lazy val infinity: Eager = RationalNumber(Rational.infinity)
  lazy val negInfinity: Eager = RationalNumber(Rational.negInfinity)
  lazy val root2: Eager = new InversePower(2, 2)(Some("√2"))
  lazy val root3: Eager = new InversePower(2, 3)(Some("√3"))

  /**
    * Parses the given string into a `Valuable` representation. If the string cannot be parsed
    * into a valid `Number`, an exception is thrown.
    *
    * @param str the input string representing a numerical value.
    * @return a `Valuable` representation of the parsed `Number`.
    * @throws CoreExceptionWithCause if parsing the string fails.
    */
  def apply(str: String): Eager =
    NumberParser.parseNumber(str) match {
      case Success(number) =>
        Scalar(number)
      case Failure(exception) =>
        throw CoreExceptionWithCause("Valuable.apply", exception)
    }

  /**
    * Creates a `Valuable` instance representing the given long value.
    *
    * @param x the input value of type `Long` to be wrapped in a `Valuable` representation.
    * @return a `Valuable` object corresponding to the input value.
    */
  def apply(x: Long): Eager = WholeNumber(x)

  /**
    * Creates a `Valuable` instance based on the given `Field`.
    * If the `Field` is a `Real` object, it converts it into a `Scalar` representation.
    * Otherwise, it throws an `IllegalArgumentException`.
    *
    * @param field the input field to be converted into a `Valuable`. It is expected
    *              to be of type `com.phasmidsoftware.number.core.Real`.
    *
    * @return a `Valuable` representation of the input `Field` as a `Scalar`.
    * @throws IllegalArgumentException if the provided `Field` is not of type `Real`.
    */
  def apply(field: numerical.Field): Eager =
    field match {
      case numerical.Real(n) =>
        Scalar(n)
      case c: numerical.Complex =>
        Complex(c)()
      case a@Algebraic_Quadratic(_, com.phasmidsoftware.number.core.algebraic.Quadratic(p, q), pos) =>
        val q: algebraic.Solution = a.solve
        val m1: Monotone = convertToMonotone(q.base, PureNumber)
        val m2: Monotone = convertToMonotone(q.offset, q.factor)
        QuadraticSolution(m1, m2, q.branch)
      case _ =>
        throw AlgebraException(s"Valuable.apply: Algebraic not yet implemented: $field")
    }

  /**
    * Converts the given value and factor into a `Monotone` representation.
    * Throws an `AlgebraException` if the conversion does not result in a valid `Monotone` instance.
    *
    * @param value  the input value of type `inner.Value` to be converted into a `Monotone`.
    * @param factor the input factor of type `inner.Factor` used for the conversion process.
    * @return a `Monotone` instance representing the input value and factor.
    * @throws AlgebraException if the conversion yields an unexpected result.
    */
  private def convertToMonotone(value: inner.Value, factor: inner.Factor): Monotone =
    Eager(numerical.Real(numerical.Number.one.make(value, factor))) match {
      case m: Monotone => m
      case _ => throw AlgebraException(s"convertToMonotone: unexpected value: $value")
    }

  /**
    * Implicitly converts an integer value into an `Eager` representation.
    * This conversion wraps the integer as a `WholeNumber` within the `Eager` type.
    *
    * @param x the integer value to be converted into an `Eager` instance.
    * @return an `Eager` instance representing the input integer as a `WholeNumber`.
    */
  implicit def convIntEager(x: Int): Eager = WholeNumber(x)

  /**
    * Implicitly converts a `Rational` value into an `Eager` representation.
    * This conversion wraps the `Rational` as a `RationalNumber` within the `Eager` type.
    *
    * @param x the `Rational` value to be converted into an `Eager` instance.
    * @return an `Eager` instance representing the input `Rational` as a `RationalNumber`.
    */
  implicit def convRationalEager(x: Rational): Eager = RationalNumber(x)

  import org.slf4j.{Logger, LoggerFactory}
  import scala.util.Try

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Provides an implementation of the `DyadicOperator` trait for the `Eager` type,
    * enabling dyadic operations to be performed on `Eager` operands or their subtypes.
    *
    * The implementation supports both same-type and cross-type operations.
    * It delegates processing to specialized `DyadicOperator` instances where applicable,
    * or falls back to a more generic operation using the given function `f`.
    *
    * NOTE currently, this is not used because the `Eq` and `FuzzyEq` instances go directly to the instance methods of `Eager`.
    *
    * @return An instance of `DyadicOperator[Eager]` that defines the behavior
    *         for performing dyadic operations on `Eager` values.
    */
  given DyadicOperator[Eager] = new DyadicOperator[Eager] {
    @tailrec
    def op[B <: Eager, Z](f: (Eager, B) => Try[Z])(x: Eager, y: B): Try[Z] = (x, y) match {
      // Same-type operations:
      case (a: Structure, b: Structure) =>
        implicitly[DyadicOperator[Structure]].op(f)(a, b)
      case (a: Complex, b: Complex) =>
        implicitly[DyadicOperator[Complex]].op(f)(a, b)
      case (a: Algebraic, b: Algebraic) =>
        implicitly[DyadicOperator[Algebraic]].op(f)(a, b)
      case (a: Nat, b: Nat) =>
        implicitly[DyadicOperator[Nat]].op(f)(a, b)

      // Cross-type operations:
      case (a: Structure, b: Complex) =>
        tryConvertAndOp(f)(a, b)
      case (a: Complex, b: Structure) =>
        op(f)(b, a.asInstanceOf[B])
      case (a: Structure, b: Nat) =>
        tryConvertAndOp(f)(a, b)
      case (a: Nat, b: Structure) =>
        op(f)(b, a.asInstanceOf[B])

      case _ =>
        f(x, y)
    }

    private def convertToMonotone(value: inner.Value, factor: inner.Factor): Monotone =
      Eager(numerical.Real(numerical.Number.one.make(value, factor))) match {
        case m: Monotone => m
        case _ => throw AlgebraException(s"convertToMonotone: unexpected value: $value")
      }
  }

  /**
    * Provides an instance of `Eq` for the `Eager` type, which defines equality comparison behavior
    * for `Eager` values. The comparison is performed using the `eqv` method of `Eager`.
    * If the `eqv` method returns `Some(true)`, the values are considered equal.
    * Otherwise, they are considered not equal, defaulting to `false` if no result is specified.
    *
    * @return An instance of `Eq[Eager]` for equality comparison.
    */
  given Eq[Eager] = Eq.instance {
    (x, y) => x.eqv(y).getOrElse(false)
  }

  /**
    * Provides an instance of the `FuzzyEq` type class for the `Eager` type.
    *
    * This implementation determines whether two `Eager` instances are approximately
    * equal based on one of the following conditions:
    * 1. The two instances are strictly equal (`===`).
    * 2. They are approximately equal according to the fuzzy equality comparison defined
    *    by the `fuzzyEqv` method of the `Eager` type, using the specified probability `p`.
    *
    * @return An instance of `FuzzyEq[Eager]` that evaluates fuzzy equality between two `Eager` values.
    */
  given FuzzyEq[Eager] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  private def tryConvertAndOp[T <: Eager, Z](f: (Eager, T) => Try[Z])(s: Structure, e: T): Try[Z] = e match {
    case c: Complex =>
      complexToEager(c) match {
        case Some(struct: Structure) =>
          f(struct, s.asInstanceOf[T])
        case Some(other) =>
          summon[DyadicOperator[Eager]].op(f)(s, other.asInstanceOf[T])
        case _ =>
          Failure(AlgebraException(s"Unexpected tryConvertAndOp: ${s.getClass.getSimpleName} === ${e.getClass.getSimpleName}"))
      }
    case n: Nat =>
      // TODO: Handle overflow - need Nat.toBigInt or safe conversion
      f(WholeNumber(n.toInt), s.asInstanceOf[T])
    case _ =>
      Failure(AlgebraException(s"Unexpected tryConvertAndCompare: ${s.getClass.getSimpleName} === ${e.getClass.getSimpleName}"))
  }

  private def complexToEager(c: Complex): Option[Eager] =
    FP.whenever(c.complex.isReal && c.complex.isExact)(c.complex.asReal.map(Eager(_)))
}