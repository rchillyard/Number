/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.core.Valuable.valuableToMaybeField
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.algebraic.Algebraic_Quadratic
import com.phasmidsoftware.number.core.inner.{PureNumber, Rational}
import com.phasmidsoftware.number.core.numerical.{CoreExceptionWithCause, Field, ComplexCartesian}
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
    * Converts the current `Lazy` instance into an `Eager` instance by forcing
    * the resolution or evaluation of its underlying value.
    *
    * The `materialize` method ensures that the computation of the value represented
    * by this `Lazy` instance is completed and encapsulated within an appropriate
    * `Eager` representation. This is particularly useful in scenarios where
    * deferred computation needs to be explicitly resolved prior to further processing.
    *
    * @return the materialized `Eager` instance representing the resolved value
    */
  lazy val materialize: Eager = this

  /**
    * Retrieves an optional name associated with this instance.
    *
    * @return an `Option[String]` containing the name if present, otherwise `None`
    */
  def maybeName: Option[String]

  /**
    * Normalizes the current instance of `Eager`.
    * This operation ensures that the instance is in its standard or canonical form.
    * Be very wary of changing the way normalization works!
    * One place that we don't routinely normalize (maybe we should) is in the Eq and FuzzyEq comparisons.
    *
    * @return the normalized instance of `Eager` as `Self`
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
    * This method is not yet implemented and will raise an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if called.
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

  /**
    * Adds the current `Eager` instance to another `Eager` instance using the `DyadicOperator` mechanism.
    * The addition operation is implemented by delegating to the `addEagers` function for specific behavior,
    * ensuring proper handling of various subtypes of `Eager`.
    *
    * @param y the `Eager` instance to be added to this `Eager` instance
    * @return a `Try[Eager]` containing the result of the addition if successful, or a failure if the operation fails
    */
  def add(y: Eager): Try[Eager] =
    summon[DyadicOperator[Eager]].op[Eager, Eager](addEagers)(this, y)

  /**
    * Multiplies the current `Eager` instance with another `Eager` instance.
    * The multiplication operation is carried out using the `DyadicOperator` mechanism,
    * which delegates to the `multiplyEagers` function for specific behavior.
    * This ensures proper handling of various subtypes of `Eager`.
    *
    * @param y the `Eager` instance to multiply with this `Eager` instance
    * @return a `Try[Eager]` containing the result of the multiplication if successful,
    *         or a failure if the operation cannot be performed
    */
  def multiply(y: Eager): Try[Eager] =
    summon[DyadicOperator[Eager]].op[Eager, Eager](multiplyEagers)(this, y)

  /**
    * Subtracts the given `Eager` instance `y` from the current `Eager` instance.
    * This operation is performed using the `DyadicOperator` mechanism, which delegates
    * to the `subtractEagers` function to handle the specific subtraction behavior.
    *
    * @param y the `Eager` instance to be subtracted from this `Eager` instance
    * @return a `Try[Eager]` containing the result of the subtraction if successful,
    *         or a failure if the operation fails
    */
  def subtract(y: Eager): Try[Eager] =
    summon[DyadicOperator[Eager]].op[Eager, Eager](subtractEagers)(this, y)

  /**
    * Performs division of the current `Eager` instance by another `Eager` instance.
    * This operation is implemented using the `DyadicOperator` mechanism, delegating
    * the actual division logic to the `divideEagers` function.
    *
    * @param y the `Eager` instance to divide the current instance by
    * @return a `Try[Eager]` containing the result of the division if successful,
    *         or a failure if the operation cannot be performed
    */
  def divide(y: Eager): Try[Eager] =
    summon[DyadicOperator[Eager]].op[Eager, Eager](divideEagers)(this, y)

  /**
    * Attempts to add two Eager instances, returning the result wrapped in a Try.
    * The addition is performed based on the concrete types of the input arguments.
    *
    * @param x The first Eager instance to be added.
    * @param y The second Eager instance to be added.
    * @return A Try[Eager] containing the result of the addition if successful, 
    *         or a Failure if the addition cannot be performed.
    */
  private def addEagers(x: Eager, y: Eager): Try[Eager] = (x, y) match
    case (a: Monotone, b: Monotone) => a.add(b)
    case (a: Solution, b: Solution) => Success(a + b)
    case _ => Failure(AlgebraException(s"Cannot add $x and $y"))

  /**
    * Attempts to multiply two Eager values and returns the result wrapped in a Try.
    *
    * @param x the first Eager value to be multiplied
    * @param y the second Eager value to be multiplied
    * @return a Try containing the resulting Eager if the multiplication is valid, 
    *         or a Failure with an AlgebraException if the multiplication cannot be performed
    */
  private def multiplyEagers(x: Eager, y: Eager): Try[Eager] = (x, y) match
    case (a: Monotone, b: Monotone) =>
      a.multiply(b)
    case (a: Solution, b: Rational) =>
      Success(a.scale(b))
    case (a: Rational, b: Solution) =>
      Success(b.scale(a))
    case _ =>
      Failure(AlgebraException(s"Cannot multiply $x and $y"))

  /**
    * Subtracts the `Eager` instance `y` from the `Eager` instance `x`.
    * This method supports the subtraction operation only for instances of subtype `Monotone`.
    * If either `x` or `y` is not of type `Monotone`, the method returns a failure containing an `AlgebraException`.
    *
    * @param x the minuend, an instance of `Eager`
    * @param y the subtrahend, an instance of `Eager` to subtract from `x`
    * @return a `Try[Eager]` containing the result of the subtraction if both `x` and `y` are of type `Monotone`,
    *         or a failure with an `AlgebraException` if the operation is not supported for the provided inputs
    */
  private def subtractEagers(x: Eager, y: Eager): Try[Eager] = (x, y) match
    case (a: Monotone, b: Monotone) =>
      a.subtract(b)
    case _ =>
      Failure(AlgebraException(s"Cannot subtract $y from $x"))

  /**
    * Divides two `Eager` instances and returns the result as a `Try[Eager]`.
    * The division is only supported for instances of the subtype `Monotone`. 
    * If the provided `Eager` instances cannot be divided (e.g., if one or both are not `Monotone`), 
    * the method returns a failure containing an `AlgebraException`.
    *
    * @param x the first `Eager` instance, representing the dividend
    * @param y the second `Eager` instance, representing the divisor
    * @return a `Try[Eager]` containing the result of the division if successful, or a failure with an `AlgebraException`
    *         if the operation is not supported for the provided inputs
    */
  private def divideEagers(x: Eager, y: Eager): Try[Eager] = (x, y) match
    case (a: Monotone, b: Monotone) =>
      a.divide(b)
    case _ =>
      Failure(AlgebraException(s"Cannot divide $x by $y"))
}

/**
  * The `Eager` object provides factory methods to create instances of `Valuable` entities
  * that are evaluated eagerly. These entities can represent numerical values parsed from
  * strings, long integers, or specific types of mathematical fields.
  */
object Eager {
  /**
    * A lazily initialized constant representing the number zero, encapsulated in an `Eager` object.
    * This value is pre-defined and serves as the canonical representation of zero within the `Eager` type.
    *
    * Delegates its value to `Number.zero`, a predefined numerical constant.
    */
  lazy val zero: Eager = Number.zero
  /**
    * A lazily initialized constant value `one` within the `Eager` class.
    * This value corresponds to the numerical representation of one.
    *
    * @return an `Eager` instance representing the value `1`.
    */
  lazy val one: Eager = Number.one
  /**
    * A lazily initialized constant -1 as a lazily evaluated `Eager` instance.
    *
    * This value is initialized by accessing a pre-defined `minusOne` instance from 
    * the `Number` class and is shared across all uses of the containing class.
    *
    * The value is immutable and can be used wherever an `Eager` representation of 
    * the constant -1 is required.
    */
  lazy val minusOne: Eager = Number.minusOne
  /**
    * A lazily initialized constant value of 2 within the `Eager` type system.
    *
    * This value is instantiated as a `Scalar(2)`, which is a specific subtype of `Eager`.
    * It signifies the literal value `2` and can be used in computations or comparisons
    * involving `Eager` instances.
    */
  lazy val two: Eager = Scalar(2)
  /**
    * A lazily initialized constant one-half, wrapped as a `RationalNumber` instance
    * constructed from `Rational.half`.
    *
    * This value is of type `Eager` and is calculated lazily, meaning it is
    * initialized only when accessed. The `half` constant provides a convenient
    * representation of the fractional value 1/2 in the `Eager` context.
    */
  lazy val half: Number = RationalNumber(Rational.half)
  /**
    * A lazily initialized constant value of `10` within the `Eager` context.
    *
    * This value is encapsulated as an instance of the `Scalar` type in the `Eager` framework.
    * The `ten` lazy value is pre-computed and serves as a reusable numeric constant
    * within operations or evaluations involving the `Eager` type.
    *
    * The lazy evaluation ensures that the value is computed only when accessed,
    * thus optimizing performance and memory usage in contexts where `ten` is unused.
    */
  lazy val ten: Eager = Scalar(10)
  /**
    * A lazily initialized constant π (pi) as an `Eager` instance.
    *
    * This value is precomputed and corresponds to the mathematical 
    * constant π, providing precise representation through the `Eager` type.
    */
  lazy val pi: Eager = Angle.pi
  /**
    * A lazily initialized constant π/2 (pi divided by 2),
    * expressed as an instance of `Eager`. This value corresponds to half of the
    * constant π, which is commonly used in trigonometry and other mathematical
    * computations involving angles.
    */
  lazy val piBy2: Eager = Angle.piBy2
  /**
    * A lazily initialized constant π/4 (pi divided by 4) as an instance of the `Eager` type.
    *
    * This value is predefined and corresponds to a quarter of π,
    * commonly used in trigonometric calculations and other mathematical contexts.
    *
    * It is lazily initialized using the `Angle.piBy4` definition
    * and provides an efficient representation of this angle.
    *
    * @return A constant `Eager` representation of π/4.
    */
  lazy val piBy4: Eager = Angle.piBy4
  /**
    * A lazily initialized constant `e` in an `Eager` representation.
    *
    * The value of `e` is imported from the `NaturalExponential` object,
    * which defines the mathematical constant `e` (approximately 2.71828...) to arbitrary precision.
    *
    * As a lazy value, the computation or retrieval of `e` is deferred until it is accessed for the first time.
    */
  lazy val e: Eager = NaturalExponential.e

  lazy val i: Eager = Complex(ComplexCartesian(0, numerical.Number.one))

  /**
    * Exact value of iPi.
    */
  lazy val iPi: Eager = Complex(ComplexCartesian(0, numerical.Number.pi))

  /**
    * A lazily initialized constant for infinity: an `Eager` instance that encapsulates
    * a `RationalNumber` with an infinite value.
    *
    * This property is provided as a convenient representation of positive infinity
    * in the context of `Eager` computations. It utilizes the `Rational.infinity`
    * to initialize the encapsulated value.
    */
  lazy val infinity: Eager = RationalNumber(Rational.infinity)
  /**
    * Represents the mathematical concept of negative infinity as an `Eager` instance.
    *
    * This value is constructed using the `Rational.negInfinity`, which signifies negative
    * infinity in the rational number system, wrapped within the `RationalNumber` representation.
    *
    * It can be used in computations where negative infinity needs to be explicitly modeled.
    */
  lazy val negInfinity: Eager = RationalNumber(Rational.negInfinity)
  /**
    * Represents the square root of 2 (√2) as an instance of `Eager`.
    *
    * `root2` is lazily initialized as an `InversePower` instance with a numerator of 2
    * and a denominator of 2. It includes an optional descriptive label "√2".
    *
    * The `Eager` type supports numerical representations and operations,
    * enabling mathematical expressions and computations involving this constant.
    */
  lazy val root2: Eager = new InversePower(2, 2)(Some("√2"))
  /**
    * Represents the square root of 1/2, lazily initialized.
    *
    * This value is an instance of `Eager`, constructed using the `InversePower`
    * class with a base of 2 and an exponent of `Eager.half`. It models the 
    * mathematical constant √½ and is labeled with its LaTeX representation for 
    * display purposes.
    *
    * The creation of this value demonstrates the use of lazy initialization
    * to defer computation until it is first accessed.
    */
  lazy val rootHalf: Eager = new InversePower(2, Eager.half)(Some("√½"))
  /**
    * Represents the square root of 3 as a lazy initialization of an `Eager` type.
    *
    * Internally implemented as an `InversePower` instance with base 2 and exponent 3,
    * providing an efficient representation of √3. Optionally tagged with a string
    * descriptor `"√3"` for identification.
    */
  lazy val root3: Eager = new InversePower(2, 3)(Some("√3"))
  /**
    * Represents the golden ratio, commonly denoted as φ (phi), in the `Eager` context.
    *
    * The golden ratio is a mathematical constant approximately equal to 1.6180339887, 
    * and is defined as the positive solution to the quadratic equation x^2 - x - 1 = 0.
    *
    * This lazy value retrieves its definition from the `QuadraticSolution.phi`.
    */
  lazy val phi: Eager = QuadraticSolution.phi
  /**
    * A lazily initialized constant of type `Eager` representing one of the roots of the quadratic equation.
    * The value is initialized based on the definition provided by `QuadraticSolution.psi`.
    *
    * This property is part of the `Eager` class, which represents mathematical expressions and constants.
    *
    * The lazy initialization ensures that the computation is deferred until the first access,
    * optimizing resource usage and preventing unnecessary calculations during object instantiation.
    */
  lazy val psi: Eager = QuadraticSolution.psi

  /**
    * Parses the given string into a `Valuable` representation. If the string cannot be parsed
    * into a valid `Number`, an exception is thrown.
    *
    * @param str the input string representing a numerical value.
    * @return a `Valuable` representation of the parsed `Number`.
    * @note Throws CoreExceptionWithCause if parsing the string fails.
    */
  def apply(str: String): Eager =
    NumberParser.parseNumber(str) match {
      case Success(number) =>
        Scalar(number)
      case Failure(exception) =>
        throw CoreExceptionWithCause("Eager.apply", exception)
    }

  /**
    * Creates an `Eager` instance representing the given double value.
    *
    * @param x the input value of type `Double` to be wrapped in an `Eager` representation.
    * @return an `Eager` instance corresponding to the input value.
    */
  def apply(x: Double): Eager = Real(x)

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
    * @return a `Valuable` representation of the input `Field` as a `Scalar`.
    * @note Throws [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the provided `Field` is not of type `Real`.
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
        QuadraticSolution(m1, m2, q.branch, false)
      case _ =>
        throw AlgebraException(s"Valuable.apply: Algebraic not yet implemented: $field")
    }

  /**
    * Converts an `Eager` instance into a `Field` representation.
    * If the conversion fails, it recovers by throwing a `AlgebraException`
    * with an appropriate error message indicating the failure.
    *
    * CONSIDER making this an instance method of `Eager`.
    *
    * @param eager the `Eager` instance to be converted into a `Field`.
    *              This is expected to represent a numerical value.
    *
    * @return the `Field` representation of the input `Valuable`.
    * @note Throws [[com.phasmidsoftware.number.algebra.util.AlgebraException]]
    *       If conversion is not possible.
    */
  def eagerToField(eager: Eager): Field =
    FP.recover(valuableToMaybeField(eager))(AlgebraException(s"Valuable:eagerToField: Cannot convert $eager to a Field"))

  /**
    * LatexRenderer for Eager (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val eagerLatexRenderer: LatexRenderer[Eager] = LatexRenderer.instance {
    case s: Solution => s.toLatex
    case s: Structure => s.toLatex
    case n: Nat => n.render
    case m =>
      throw new IllegalArgumentException(s"No LaTeX renderer for Eager type: ${m.getClass.getName}")
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

  import scala.util.Try

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

  /**
    * Converts the given value and factor into a `Monotone` representation.
    * Throws an `AlgebraException` if the conversion does not result in a valid `Monotone` instance.
    *
    * @param value  the input value of type `inner.Value` to be converted into a `Monotone`.
    * @param factor the input factor of type `inner.Factor` used for the conversion process.
    * @return a `Monotone` instance representing the input value and factor.
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the conversion yields an unexpected result.
    */
  private def convertToMonotone(value: inner.Value, factor: inner.Factor): Monotone =
    Eager(numerical.Real(numerical.Number.one.make(value, factor))) match {
      case m: Monotone => m
      case _ => throw AlgebraException(s"convertToMonotone: unexpected value: $value")
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