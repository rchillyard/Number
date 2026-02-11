/*
 * Copyright (c) 2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import cats.Show
import cats.implicits.catsSyntaxEq
import cats.kernel.Eq
import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.*
import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.util.FP.recover
import com.phasmidsoftware.number.algebra.util.LatexRenderer.{LatexRendererOps, nthRoot}
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.inner.Rational.toIntOption
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{Fuzziness, Prime, WithFuzziness}
import org.slf4j.{Logger, LoggerFactory}

import scala.reflect.ClassTag
import scala.util.{Success, Try}

/**
  * Represents an eager mathematical root, parameterized with an integral root degree and a base number.
  *
  * TODO this may be only a temporary class because we can model all roots (and more) as solutions to Algebraic equations.
  *
  * CONSIDER making this an (abstract?) class and having subclasses for square root and cube root.
  * 
  * TODO there are problems with multiplication (class cast exception, for example).
  * Therefore, we should remove the `CanMultiplyAndDivide` mixin.
  *
  * NOTE do not invoke the constructor directory: all construction should go via the apply method(s) which will normalize as appropriate.
  *
  * @param n      the degree of the root, specified as an integer
  * @param number the base `Number` value on which the root operation is defined
  */
case class InversePower(n: Int, number: Number)(val maybeName: Option[String] = None) extends Transformed with CanMultiplyAndDivide[Monotone] with Scalable[InversePower] with CanPower[Number] {

  require(n > 0, s"InversePower: n must be positive, but was $n")
  require(!number.isZero, s"InversePower: number must be non-zero, but was $number")

  /**
    * This yields the scale function for this Functional.
    *
    * @return a function to transform the nominal value into the actual value as it would appear in a PureNumber context.
    */
  val scaleFunction: Double => Double = math.pow(_, 1.0 / n)

  /**
    * Represents the derivative function associated with this `Functional` instance.
    * That's to say `d(f(number))` by `d(number)` where `f` is this `Functional`.
    * For a Monotone, the derivative should be positive, however, it is possible
    * that it is not positive for certain types of `Functional`.
    *
    * The `derivativeFunction` provides a mathematical operation that computes the derivative
    * with respect to a given input value. It is typically used to evaluate rates of change
    * or sensitivity in the context of numerical transformations.
    *
    * @return A function that accepts a `Double` value and returns the computed derivative as a `Double`.
    */
  val derivativeFunction: Double => Double = x => scaleFunction(x) / n / x

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
        case rn@RationalNumber(r@IntRational(num, den), _) =>
          // Handle rational bases by taking root of numerator and denominator separately
          val defaultValue: InversePower = if (number == normalized) this else InversePower(n, rn)
          val isNegative = num < 0
          val absNum = if (isNegative) -num else num

          val maybeLookup = lookups.get(n)

          val result = for {
            lookup <- maybeLookup
            nr <- if (!isNegative || n % 2 == 1) lookup.get(absNum) else None
            dr <- lookup.get(den)
          } yield RationalNumber(handleNegativeRoot(BigInt(nr), n, isNegative), BigInt(dr))

          result.getOrElse(defaultValue)

        case q: Q if q.isInstanceOf[Number] =>
          // Handle integer bases (including WholeNumber)
          val defaultValue: InversePower = if (number == normalized) this else InversePower(n, q.asInstanceOf[Number])
          val r = q.toRational
          val isNegative = r.signum < 0
          val absR = if (isNegative) r.negate else r

          val rootValue = if (!isNegative || n % 2 == 1) {
            getRoot(absR, lookups).map { root =>
              WholeNumber(handleNegativeRoot(root, n, isNegative))
            }
          } else None

          lazy val recipRootValue = if (!isNegative || n % 2 == 1) {
            getRoot(absR.invert, lookups).map { x =>
              RationalNumber(Rational(handleNegativeRoot(x, n, isNegative)).invert)
            }
          } else None

          (rootValue orElse recipRootValue).getOrElse(defaultValue)
      }

    case _ => // For higher roots, use prime factorization
      doNormalize match {
        case Left(value) => value
        case Right(value) => value
      }
  }

  /**
    * Handles the root operation result for negative values.
    *
    * @param value      The result of the root operation as a BigInt.
    * @param n          The degree of the root.
    * @param isNegative A flag indicating whether the original value was negative.
    * @return The adjusted root value, negated if the original value 
    *         was negative and the root degree is odd.
    */
  private def handleNegativeRoot(value: BigInt, n: Int, isNegative: Boolean): BigInt =
    if (isNegative && n % 2 == 1) -value else value

  private lazy val simplified: InversePower = doNormalize match {
    case Left(number) =>
      number
    case Right(number) =>
      // NOTE this is the best we can do in this situation
      new InversePower(1, number)()
  }

  private lazy val doNormalize: Either[InversePower, Number] =
    number.normalize match {
      case IsInteger(z) =>
        val (extracted, remaining) = InversePower.normalizeIntegralRoot(z, n)
        if (remaining == 1)
          Right(WholeNumber(extracted))
        else
          Left(InversePower(n, WholeNumber(remaining)))
      case r@IntRational(num, den) =>
        val (numExtracted, numRemaining) = InversePower.normalizeIntegralRoot(num, n)
        val (denExtracted, denRemaining) = InversePower.normalizeIntegralRoot(den, n)

        if (numRemaining == 1 && denRemaining == 1)
          Right(RationalNumber(numExtracted, denExtracted))
        else
          Left(InversePower(n, r)) // Also: use IntRational, not `r`
      case x: Number =>
        Left(InversePower(n, x))
    }

  /**
    * Defines a transformation that transforms a `Monotone` instance into a corresponding `Scalar` value.
    *
    * The transformation defines how a `Monotone` is interpreted or converted in the context of `Scalar`.
    *
    * @return a transformation that maps a `Monotone` object to a `Scalar` result
    */
  def transformation[T: ClassTag]: Option[T] = None // TODO Implement this

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
    * Compares this `InversePower` instance with another `Functional` instance.
    *
    * @param that The `Functional` instance to compare against.
    * @return An integer where:
    *         - a negative value indicates this instance is less than `that`,
    *         - zero indicates this instance is equal to `that`,
    *         - a positive value indicates this instance is greater than `that`.
    */
  def compare(that: Functional): Int = that match {
    case i@InversePower(m, x) if isExact && i.isExact =>
      recover(simplified.compareExact(i.simplified))(AlgebraException(s"InversePower.compare: cannot compare $this with InversePower: $that"))
    case _ =>
      recover(for {
        a <- convert(Real.zero)
        b <- that.convert(Real.zero)
      } yield a.compare(b))(AlgebraException(s"InversePower.compare: cannot compare $this and $that"))
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
    *
    * @return true if the number is zero, false otherwise
    */
  def isZero: Boolean = false

  /**
    * Determines whether this object represents unity.
    *
    * @return true if the object represents unity, false otherwise
    */
  def isUnity: Boolean = number.isUnity

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
      case _ => s"${n}th root of $suffix"
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

  /**
    * Normalizes a given radicand with respect to a specified root degree.
    * The method calculates the product of factors that can be extracted outside
    * of the root and the product of factors that remain inside the root.
    *
    * @param radicand   The radicand value to normalize. It must be a positive integer.
    * @param rootDegree The root degree (e.g., square root, cube root) to consider.
    *                   It must be a positive integer greater than zero.
    *
    * @return A tuple where the first element represents the extracted product (outside the root)
    *         and the second element represents the remaining product (inside the root).
    *
    * @throws AlgebraException If any prime factor of the radicand is out of range for an Int.
    */
  def normalizeIntegralRoot(radicand: Int, rootDegree: Int): (Int, Int) = {
    // We need to check all prime factors up to radicand itself
    // But we can limit to primes up to sqrt(radicand) for the factorization process
    // Actually, for complete factorization, we need all primes up to radicand
    val absRadicand = radicand.abs

    val factors: Seq[Option[Int]] = Prime.primeFactors(BigInt(absRadicand)).map(_.toIntOption)
    val xs: Seq[Int] = for {
      prime <- Prime.primeFactors(BigInt(absRadicand))
      x <- prime.toIntOption
    } yield x

    if (xs.length != factors.length) throw AlgebraException("at least one prime factor is out of range for Int")

    // Group by prime value and count occurrences
    val factorCounts: Map[Int, Int] = xs.groupBy(x => x).view.mapValues(_.size).toMap

    // For each prime, extract what can come out of the root
    var extractedProduct = 1
    var remainingProduct = 1

    factorCounts.foreach { case (prime, count) =>
      val extracted = count / rootDegree // How many come out
      val remaining = count % rootDegree // How many stay inside

      extractedProduct *= Math.pow(prime, extracted).toInt
      remainingProduct *= Math.pow(prime, remaining).toInt
    }

    // Handle the sign for odd roots
    val signedExtracted = if (radicand < 0 && rootDegree % 2 == 1) -extractedProduct else extractedProduct

    (signedExtracted, remainingProduct)
  }

  val logger: Logger = LoggerFactory.getLogger(getClass)

  /**
    * Provides a `Show` instance for the `InversePower` type, enabling the conversion
    * of an `InversePower` instance into a human-readable `String` representation.
    *
    * @return A `Show` instance for the `InversePower` type.
    */
  given Show[InversePower] = Show.show(_.render)

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

    if (ip.n == 2) 
      LatexRenderer.sqrt(baseLatex) 
    else 
      nthRoot(ip.n, baseLatex)
  }

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
}
