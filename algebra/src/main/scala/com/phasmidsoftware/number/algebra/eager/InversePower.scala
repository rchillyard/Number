/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import algebra.CommutativeGroup
import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.LatexRenderer.{LatexRendererOps, nthRoot}
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.inner.Rational.toIntOption
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{Fuzziness, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag
import scala.util.{Success, Try}

/**
  * Represents a mathematical Root object, parameterized with an integral root degree and a base number.
  *
  * The `Root` class extends `Monotone`, `MultiplicativeWithPower[Root]`, and `Ordered[Root]`,
  * providing mathematical operations for roots and ensuring ordering functionality.
  *
  * TODO this may be only a temporary class because we can model all roots (and more) as solutions to Algebraic equations.
  *
  * CONSIDER making this an (abstract?) class and having subclasses for square root and cube root.
  * 
  * TODO there are problems with multiplication (class cast exception, for example).
  * Therefore, we should remove the `CanMultiplyAndDivide` mixin.
  *
  * @param n      the degree of the root, specified as an integer
  * @param number the base `Number` value on which the root operation is defined
  */
case class InversePower(n: Int, number: Number)(val maybeName: Option[String] = None) extends Transformed with CanMultiplyAndDivide[Monotone] with Scalable[InversePower] with CanPower[Number] with Ordered[InversePower] {

  require(n > 0, s"InversePower: n must be positive, but was $n")
  require(!number.isZero, s"InversePower: number must be non-zero, but was $number")

  /**
    * This yields the scale function for this Functional.
    *
    * @return a function to transform the nominal value into the actual value as it would appear in a PureNumber context.
    */
  val scaleFunction: Double => Double = math.pow(_, 1.0 / n)

  /**
    * Computes the result of raising an instance of type `T` to the power
    * specified by the given `ExactNumber`.
    *
    * This method performs the power operation and returns the result wrapped
    * in an `Option[T]`. If the operation is invalid or cannot be performed,
    * `None` is returned.
    *
    * @param that the `ExactNumber` exponent to which the instance is raised
    * @return an `Option[T]` containing the result of the power operation if valid,
    *         or `None` if the operation could not be performed
    */
  infix def pow(that: ExactNumber): Option[Number] = number match {
    case x: CanPower[Number] @unchecked =>
      x.pow(that * Rational(n).invert)
    case _ =>
      None
  }

  /**
    * Normalizes this `Valuable` to its simplest equivalent form.
    * This may change the type (e.g., RationalNumber → WholeNumber, Complex(5,0) → WholeNumber(5)).
    *
    * For Expression types, this will attempt to simplify and materialize if the result is exact.
    * For Eager types, this will reduce to the simplest type representation.
    *
    * @return the simplest `Valuable` representation of this value
    */
  def normalize: Monotone = n match {
    case 1 =>
      number.normalize
    case 2 | 3 =>
      val lookups = Map(2 -> Rational.squareRoots, 3 -> Rational.cubeRoots)
      val normalized = number.normalize
      normalized match {
        case q: Q if q.isInstanceOf[Number] =>
          val defaultValue: InversePower = if (number == normalized) this else InversePower(n, q.asInstanceOf[Number])
          val r = q.toRational
          val rootValue = getRoot(r, lookups).map(WholeNumber(_))
          lazy val recipRootValue = getRoot(r.invert, lookups).map(x => RationalNumber(Rational(x).invert))
          (rootValue orElse recipRootValue) getOrElse defaultValue
      }
    case _ =>
      number.normalize match {
        case normalized: Number if normalized == number =>
          this
        case x: Number =>
          InversePower(n, x)
      }
  }

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = ???

  /**
    * Returns a new instance of `Monotone` that is the negation of the current instance.
    * CONSIDER sorting out the use of CanNegate so that we can extend that for Monotone.
    *
    * @return a `Monotone` representing the negation of this instance
    * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the negation operation is not defined for the current instance
    */
  def negate: Monotone =
    throw AlgebraException(s"InversePower.negate: cannot negate $this")

  /**
    * Compares the current `InversePower` instance with another `InversePower` to determine their order.
    *
    * The comparison behavior depends on whether the instances are `exact`:
    * - If both instances are exact, it attempts an exact comparison using `compareExact`.
    * - Otherwise, it falls back to comparing the `n` values of the two instances.
    *
    * @param that the `InversePower` instance to compare with the current instance
    * @return an `Int` where:
    *         - A negative value indicates that the current instance is less than the provided instance.
    *         - Zero indicates equality between the instances.
    *         - A positive value indicates that the current instance is greater than the provided instance.
    */
  def compare(that: InversePower): Int = (this.isExact, that.isExact) match {
    case (true, true) =>
      compareExact(that).getOrElse(0) // TODO fix this
    case _ =>
      this.n.compare(that.n)
  }

  /**
    * Represents the multiplicative identity element of the structure.
    *
    * The `one` value serves as the neutral element for the multiplication operation, meaning
    * that for any instance `t` of type `T`, the equation `one * t = t * one = t` holds true.
    */
  def one: InversePower = InversePower(1, Real.one)(Some("1"))

  /**
    * Scales the current instance by the given factor.
    *
    * This method applies a scaling operation on the instance using the provided
    * rational factor and returns the resulting scaled instance.
    *
    * @param factor the rational number representing the scale factor
    * @return the scaled instance of type `T`
    */
  infix def *(factor: Rational): InversePower = {
    val scaledNumber = Range(0,n).foldLeft(number)((a,_) => a.scale(factor).asInstanceOf[Number])
    InversePower(n, scaledNumber)
  }

  /**
    * Retrieves an optional fuzziness value associated with this instance.
    *
    * The fuzziness value, if present, provides information about the level of uncertainty
    * or imprecision, modeled as a `Fuzziness[Double]`.
    *
    * @return an `Option` containing the `Fuzziness[Double]` value if defined, or `None` if no fuzziness is specified.
    */
  def fuzz: Option[Fuzziness[Double]] =
    Eager(Eager.eagerToField(number).power(numerical.Number(Rational(n).invert))) match {
      case fuzzy: WithFuzziness =>
        fuzzy.fuzz
      case _ =>
        None // CONSIDER should this throw an exception?
    }

  /**
    * Compares the current `Root` instance with another `Number` to determine their exact order.
    *
    * If the provided `Number` is a `Root`, this method compares their underlying radian values.
    * If the provided `Number` is not a `Root`, the comparison cannot be performed, and `None` is returned.
    *
    * @param that the `Number` instance to compare with the current `Root` instance
    * @return an `Option[Int]`, where `Some(-1)` indicates that the current `Root` is less than the provided `Root`,
    *         `Some(0)` indicates that both roots are equal, `Some(1)` indicates that the current `Root` is greater,
    *         and `None` is returned if the comparison cannot be made
    */
  def compareExact(that: Monotone): Option[Int] = that match {
    case InversePower(m, x) if m == n => // TODO - there are other situations where the result should be Some(0) (see Factor class).
      Some(number.compare(x))
    case _ =>
      None
  }

  /**
    * Converts the current number to a representation of the specified type `T`, if possible.
    *
    * This method attempts to convert the number to a type `T` that has implicit evidence
    * of `Ordering`. If the conversion is successful, it returns an `Option` containing the
    * resulting typed value. If the conversion is not valid or not possible for the given
    * type `T`, it returns `None`.
    *
    * @return an `Option` containing the converted value of type `T` if successful, or `None` if the conversion is not possible.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T] = (normalize, t) match {
    case (x: WholeNumber, _) =>
      x.convert(t)
    case (x: RationalNumber, _) =>
      x.convert(t)
    case (x@InversePower(m, z), r: Real) =>
      z.approximation(true).flatMap(x => x.power(Rational(m).invert)).asInstanceOf[Option[T]]
    case x =>
      None
  }

  /**
    * Checks for equality between the current instance of `Eager` and another `Eager` instance.
    *
    * This method determines equality by comparing the structure of the two instances:
    * - If both instances are of type `InversePower`, their associated parameters (`n` and `x`) are compared.
    *   If their `n` values match, the equality of their `x` values is evaluated recursively using their own `eqv` method.
    * - For other cases, the method defers to the `eqv` implementation of the superclass.
    *
    * @param that the `Eager` instance to compare with the current instance
    * @return a `Try[Boolean]` where:
    *         - `Success(true)` indicates the two instances are equal.
    *         - `Success(false)` indicates the two instances are not equal.
    *         - A failed `Try` may be returned if an error occurs during comparison.
    */
  override def eqv(that: Eager): Try[Boolean] = (this, that) match {
    case (InversePower(n1, x1), InversePower(n2, x2)) =>
      if (n1 == n2) {
        x1.eqv(x2)
      } else {
        Success(false)
      }
    case _ =>
      super.eqv(that)
  }

  /**
    * Compares this instance with another `Eager` instance using a specified precision.
    * The comparison involves converting both instances to `Real` and performing
    * a fuzzy equivalence check with the specified precision.
    *
    * @param p    The precision parameter used in the fuzzy equivalence comparison.
    * @param that The `Eager` instance to be compared against.
    * @return A `Try[Boolean]` indicating the result of the fuzzy equivalence check.
    *         Success with `true` if the instances are equivalent within the given precision,
    *         `false` if they are not equivalent, or a failure if a comparison cannot be performed.
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a@InversePower(n1, x1), b: Structure) =>
      for {
        r1 <- FP.toTry(a.convert(Real.zero))(FP.fail(s"InversePower.fuzzyEqv: cannot convert $a to Real"))
        r2 <- FP.toTry(b.convert(Real.zero))(FP.fail(s"InversePower.fuzzyEqv: cannot convert $b to Real"))
        z <- r1.fuzzyEqv(p)(r2)
      } yield z
    case _ =>
      FP.fail(s"InversePower.fuzzyEqv: cannot compare $this and $that")
  }

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = number.isZero

  /**
    * Determines the sign of the scalar value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  def signum: Int =
    if (number.signum > 0)
      1
    else
      n % 2 match {
        case 0 => 1
        case _ => -1
      }

  /**
    * Method to determine if this Structure object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this Structure object is exact in the context of No factor, else false.
    */
  def isExact: Boolean = number.isExact // TODO there are some situations where a Root is actually exact.

  /**
    * Converts this `Number` into its corresponding `Rational` representation, if possible.
    *
    * @return an `Option[Rational]` containing the `Rational` representation of this `Number`
    *         if it can be converted, or `None` if the conversion is not possible.
    */
  def toRational: Option[Rational] = number match {
    case z: Z =>
      z.toRational.power(Rational(n).invert).toOption
    case _ =>
      None
  }

  /**
    * Renders this `Root` instance as a string representation of base in terms of π.
    *
    * The method formats the radius equivalent to π, omitting the numeric coefficient if it is 1.
    *
    * @return a string representation of the `Root` in terms of π
    */
  def render: String = maybeName getOrElse {
    val suffix = number.render
    n match {
      case 2 => s"√$suffix"
      case 3 => s"³√$suffix"
      case _ => toString
    }
  }

  /**
    * Computes the potential factor associated with this instance.
    *
    * @return an `Option` containing a `Factor` if available, otherwise `None`
    */
  def maybeFactor(context: Context): Option[Factor] = n match {
    case 2 if context.factorQualifies(SquareRoot) => Some(SquareRoot)
    case 3 if context.factorQualifies(SquareRoot) => Some(CubeRoot)
    case _ => None
  }

  /**
    * Provides an approximation of this number, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of this `Number`, or `None` if no approximation is available.
    */
  def approximation: Option[Real] = convert(Real.zero)

  /**
    * Retrieves the root value from the provided lookups map based on the given rational number.
    *
    * @param r       The Rational number used as a key for the lookup operation.
    * @param lookups A nested map where the outer key is an integer and the inner map
    *                maps integers to their associated root values.
    * @return An Option containing the root value if found, or None if no corresponding
    *         value exists in the lookups map.
    */
  private def getRoot(r: Rational, lookups: Map[Int, Map[Int, Int]]): Option[Int] =
    for {
      lookup <- lookups.get(n)
      y <- toIntOption(r)
      p <- lookup.get(y)
    } yield p

  /**
    * Scales the current instance using the provided `Number`.
    *
    * The method performs a scaling operation by applying the given `Number` to the current instance,
    * producing an optional result of type `T`. If the scaling operation cannot be defined for the given `Number`,
    * it returns `None`.
    *
    * @param that the `Number` used to scale the current instance
    * @return an `Option[T]` containing the result of the scaling operation if successful, or `None` if the operation cannot be performed
    */
  infix def doScale(that: Number): Option[InversePower] =
    (that, number) match {
      case (x: CanPower[Number] @unchecked, y: Z) =>
        val triedRational = y.toRational.power(Rational(n).invert).toOption
        val value: Option[Number] = triedRational.flatMap(r => x.pow(RationalNumber(r)))
        value.asInstanceOf[Option[InversePower]]
      // TODO need to match on types, not use isInstanceOf, etc.
      case _ =>
        throw AlgebraException(s"InversePower.doScale: cannot scale $this by $that")
    }
}

/**
  * The `Root` companion object contains utility methods, predefined constants, and
  * typeclass instances for working with roots. Angles are represented using
  * rational numbers and comply with the algebraic structure of a commutative group.
  */
object InversePower {
  /**
    * Creates a new instance of InversePower using the provided parameters.
    *
    * @param n The integer exponent to be used in the computation.
    * @param x The base number for the inverse power calculation.
    * @return An instance of InversePower computed with the specified parameters.
    */
  def apply(n: Int, x: Number): InversePower = new InversePower(n, x)()

  /**
    * Creates an instance of `InversePower` using the given parameters.
    * TODO why is this showing it's recursive?
    *
    * @param n The exponent value to be used in the `InversePower` instance.
    * @param x The numeric value to be wrapped within the `WholeNumber` type and used in the `InversePower` instance.
    * @return A new instance of `InversePower` with the specified parameters.
    */
  def apply(n: Int, x: Int): InversePower =
    InversePower(n, WholeNumber(x))

  val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * A given instance of `DyadicOperator` for the `InversePower` type. 
    * This instance allows the application of a binary operation on 
    * two operands of types `InversePower` and a subtype of `InversePower`
    * (`B <: InversePower`), producing a result wrapped in a `Try`.
    *
    * @return A `Try[Z]` containing the result of applying the provided 
    *         operation `f` to the operands `x` and `y`, or an exception 
    *         if the operation fails.
    */
  given DyadicOperator[InversePower] = new DyadicOperator[InversePower] {
    def op[B <: InversePower, Z](f: (InversePower, B) => Try[Z])(x: InversePower, y: B): Try[Z] =
      f(x, y)
  }

  /**
    * Defines an equality `Eq` instance for the `InversePower` type, utilizing a dyadic operator 
    * to determine equivalence between two `InversePower` instances. The equality comparison 
    * is performed by delegating to an eagerly evaluated function `x.eqv(y)`, where `x` and `y`
    * are `Eager` instances, and the result is optionally returned.
    *
    * @return An instance of `Eq[InversePower]` that provides a mechanism to compare `InversePower`
    *         instances for equality by applying the dyadic operator and resolving the result.
    */
  given Eq[InversePower] = Eq.instance {
    (x, y) =>
      summon[DyadicOperator[InversePower]].op((x: Eager, y: Eager) => x.eqv(y))(x, y).getOrElse(false)
  }

  /**
    * Provides a fuzzy equality instance for the `InversePower` type.
    *
    * This implementation allows comparing two `InversePower` instances, `x` and `y`, 
    * based on a given probability `p`. Two instances are considered approximately equal 
    * if they are either strictly equal (`x === y`) or satisfy the fuzzy equality predicate 
    * when evaluated against `p`.
    *
    * @return A `FuzzyEq` instance specific to the `InversePower` type.
    */
  given FuzzyEq[InversePower] = FuzzyEq.instance {
    (x, y, p) =>
      x === y || x.fuzzyEqv(p)(y).getOrElse(false)
  }

  /**
    * LatexRenderer for InversePower.
    *
    * Renders as:
    * - \sqrt{base} for power 2
    * - \sqrt[n]{base} for other powers
    */
  implicit val inversePowerLatexRenderer: LatexRenderer[InversePower] = LatexRenderer.instance { ip =>
    val baseLatex = ip.number.asMonotone.toLatex

    if (ip.n == 2) {
      LatexRenderer.sqrt(baseLatex)
    } else {
      nthRoot(ip.n, baseLatex)
    }
  }

//  private def tryConvertAndCompareTransformed[B <: Transformed, Z](f: (Transformed, B) => Try[Z])(s: Logarithm, e: B): Try[Z] =
//    FP.fail(s"Transformed: unsupported cross-type operation: ${s.getClass.getSimpleName} op ${e.getClass.getSimpleName}")

  /**
    * Represents the multiplicative identity for roots.
    *
    * This value denotes a root of zero base, serving as the identity element in
    * the group structure of roots. It is constructed using the `Root` companion object
    * initialized with the additive identity of `RationalNumber`.
    */
  val one: InversePower = InversePower(1, WholeNumber.one)(Some("1"))

  /**
    * Provides an implicit `Show` instance for the `Root` class, enabling conversion
    * of a `Root` instance to a string representation using its `render` method.
    *
    * This allows the `Root` class to integrate seamlessly with libraries or frameworks
    * requiring a `Show` typeclass instance for displaying or logging purposes.
    */
  implicit val showInversePower: Show[InversePower] = Show.show(_.render)

  /**
    * Provides an implicit implementation of a commutative group for the `Root` type, supporting
    * group operations such as identity, combination, and inversion.
    *
    * This allows `Root` objects to adhere to the algebraic structure of a commutative group, where
    * the `combine` operation is associative and commutative, an identity element exists, and
    * each element has a multiplicative inverse.
    */
  implicit object rootIsCommutativeGroup extends CommutativeGroup[InversePower] {
    /**
      * Provides the identity element for the `Root` group, representing a root of one.
      *
      * @return a `Root` instance with zero base, acting as the identity element in the group structure.
      */
    def empty: InversePower = InversePower.one

    /**
      * Combines two `Root` instances by adding their respective base.
      * TODO implement this method for InversePower (or eliminate it).
      *
      * @param x the first `Root` to combine
      * @param y the second `Root` to combine
      * @return a new `Root` representing the sum of the base of the two provided `Root` instances
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the provided `Root` instances are not of the same type.
      */
    def combine(x: InversePower, y: InversePower): InversePower = (x, y) match {
      case (InversePower(n1, x1: Number), InversePower(n2, x2: Number)) =>
        throw AlgebraException(s"InversePower.combine: cannot combine $x and $y")
    }

    /**
      * Computes the additive inverse of the given `Root`.
      * TODO eliminate this method.
      *
      * This method inverts the input root, returning a `Root` instance
      * that represents its multiplicative inverse, relative to `Root.one`.
      *
      * @param a the `Root` instance to be inverted
      * @return a new `Root` instance representing the multiplicative inverse of the input
      * @note Throws an [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the provided `Root` instance is not invertible.
      */
    def inverse(a: InversePower): InversePower =
      throw AlgebraException(s"InversePower.inverse: cannot invert $a")
  }
}
