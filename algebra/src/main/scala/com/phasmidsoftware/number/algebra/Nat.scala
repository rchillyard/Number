package com.phasmidsoftware.number.algebra

import algebra.ring.Semiring
import com.phasmidsoftware.number.algebra.Nat.natIsSemiring
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Rational}
import scala.annotation.tailrec

/**
  * Represents natural numbers using Peano arithmetic.
  *
  * The `Nat` trait has two possible subtypes:
  * - `Zero`, representing the number 0.
  * - `Succ`, representing the successor of another `Nat` (e.g., `Succ(Zero)` represents 1, `Succ(Succ(Zero))` represents 2, and so on).
  *
  * This encoding provides a type-level representation of natural numbers
  * that can be used in functional programming constructs.
  *
  * Operations on natural numbers, such as addition and multiplication,
  * can be defined recursively via pattern matching.
  *
  * CONSIDER increasing the use of the `asInt` value for better performance.
  * But note that doing that destroys the Peano aspect of this class (keep in mind that we also have WholeNumber).
  */
sealed trait Nat extends Valuable with N {
  /**
    * Adds the specified natural number to this natural number.
    *
    * @param that the natural number to be added to this instance
    * @return a new natural number representing the sum of this instance and the specified natural number
    */
  def +(that: Nat): Nat

  /**
    * Increments this natural number by one.
    *
    * @return a new natural number representing the successor of the current instance.
    */
  lazy val inc: Nat = Succ(this)

  /**
    * Converts this natural number into an integer.
    * TODO eliminate this method and use toInt instead.
    *
    * @return the integer value corresponding to this natural number
    */
  def asInt: Int

  /**
    * Method to render this Structure for presentation to the user.
    *
    * @return a String
    */
  lazy val render: String = asInt.toString

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
  def isExact: Boolean = true

  /**
    * Converts this instance of `Z` to its corresponding int representation--if possible.
    *
    * @return an `Option[Int]` representing this value, providing that it can fit in an `Int`.
    *         If the value cannot fit in an `Int`, then the `Option` will be `None`.
    *         If the value can fit in an `Int`, then the `Option` will be `Some(Int)`.
    */
  def toInt: Int = asInt

  /**
    * Converts this instance of `Q` to its corresponding rational representation.
    *
    * @return a Rational instance representing the current value
    */
  def toRational: Rational = Rational(asInt)

  /**
    * Computes a potential representation of this `Nat` instance as `Z`.
    *
    * @return an `Option[Z]` containing a corresponding instance of `Z` if available; otherwise, `None`.
    */
  def maybeZ: Option[Z] = Some(WholeNumber(asInt))

  /**
    * Converts the current instance to a Double representation.
    * CONSIDER changing to maybeDouble returning Option[Double].
    *
    * @return the Double value corresponding to the current instance
    */
  def asDouble: Double = toInt.toDouble

  /**
    * Creates an instance of `R` from the given `Rational` value.
    *
    * @param q the `Rational` value to be converted into an instance of `R`
    * @return an instance of `R` representing the specified `Rational` value
    */
  def maybeQ: Option[Q] = Some(RationalNumber(toRational))

  /**
    * Yields an approximation of this `Valuable` object, if applicable.
    *
    * This method attempts to compute an approximate representation of the number
    * in the form of a `Real`, which encapsulates uncertainty or imprecision
    * in its value. If no meaningful approximation is possible for the number, it
    * returns `None`.
    *
    * @return an `Option[Real]` containing the approximate representation
    *         of the number, or `None` if no approximation is available.
    */
  lazy val approximation: Option[Real] =
    maybeDouble map Real.apply

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  lazy val maybeDouble: Option[Double] = Some(asInt)

  /**
    * Attempts to yield a factor for the instance, if available.
    *
    * A `Factor` is a representation of the underlying numerical domain, for example, `PureNumber`, `Radian`, etc.
    *
    * @return an `Option[Factor]` containing the factor representation of this object,
    *         or `None` if factorization is not applicable or unavailable.
    */
  lazy val maybeFactor: Option[Factor] = Some(PureNumber)
}

/**
  * Represents the natural number 0 in Peano arithmetic.
  *
  * `Zero` is the base case of the `Nat` type, signifying the absence of any successors.
  * It serves as the foundation for constructing all other natural numbers through
  * the `Succ` type.
  */
case object Zero extends Nat {
  /**
    * Adds the specified natural number to this instance.
    *
    * @param that a natural number to be added to this
    * @return that, since Zero is the additive identity.
    */
  def +(that: Nat): Nat = that

  /**
    * Converts this natural number to its integer representation.
    *
    * @return 0
    */
  val asInt: Int = 0
}

/**
  * Represents the successor of a natural number in Peano arithmetic.
  *
  * The `Succ` class is a case class that extends the `Nat` trait
  * and signifies the successor of another natural number.
  *
  * @param pred The predecessor of this natural number, which is also a `Nat`.
  */
case class Succ(pred: Nat) extends Nat {
  /**
    * Compares this `Succ` instance for equality with another object.
    *
    * This method determines whether the current `Succ` instance represents
    * the same natural number as another object, based on their predecessors.
    * Only instances of type `Succ` are eligible for comparison.
    *
    * @param obj the object to compare to this instance
    * @return true if the specified object is a `Succ` instance and represents the same natural number as this instance; false otherwise
    */
  override def equals(obj: Any): Boolean = obj match {
    case that: Succ =>
      @tailrec
      def inner(x: Nat, y: Nat): Boolean = (x, y) match {
        case (Zero, Zero) => true
        case (Zero, _) => false
        case (_, Zero) => false
        case (Succ(xPred), Succ(yPred)) => inner(xPred, yPred)
      }

      inner(this.pred, that.pred)
    case _ =>
      false
  }

  /**
    * Adds the specified natural number to this natural number.
    *
    * This method leverages the `natIsSemiring.plus` implementation, which defines
    * addition for natural numbers recursively using Peano arithmetic.
    *
    * @param that the natural number to be added to this instance
    * @return a new natural number representing the sum of this instance and the specified natural number
    */
  def +(that: Nat): Nat =
    natIsSemiring.plus(this, that)

  /**
    * Converts this natural number into an integer.
    *
    * @return the integer value corresponding to this natural number
    */
  lazy val asInt: Int = {
    @tailrec
    def inner(r: Int)(n: Nat): Int = n match {
      case Zero => r
      case Succ(nPred) => inner(r + 1)(nPred)
    }

    inner(0)(this)
  }
}

/**
  * Provides a companion object for the sealed trait `Nat`, incorporating utility methods
  * and type class instances for handling natural numbers defined via Peano arithmetic.
  *
  * The object includes an implicit implementation of the `Semiring` type class,
  * which introduces algebraic operations such as addition and multiplication
  * specifically tailored for natural numbers (`Nat`).
  */
object Nat {
  /**
    * Converts a given non-negative integer into its corresponding `Nat` representation.
    *
    * The method uses Peano arithmetic to construct the natural number representation.
    * It begins with the base case `Zero` and iteratively applies `Succ` to build
    * the representation of the integer.
    *
    * @param x A non-negative integer to be converted into a `Nat` representation.
    *          Must be greater than or equal to 0.
    * @return The `Nat` representation of the given integer. The result is `Zero`
    *         if `x` is 0, or a `Succ` chain equivalent to `x` if `x` is greater than 0.
    */
  def apply(x: Int): Nat = {
    assert(x >= 0, s"Nat.apply: x ($x) must be >= 0")

    @tailrec
    def inner(r: Nat)(z: Int): Nat = z match {
      case 0 => r
      case _ => inner(Succ(r))(z - 1)
    }

    inner(Zero)(x)
  }

  /**
    * Provides an implementation of the `Semiring` type class for natural numbers (`Nat`),
    * enabling operations such as addition and multiplication following the rules of Peano arithmetic.
    *
    * This instance defines the behavior for:
    * - `zero`: The base natural number `Zero`, representing the additive identity.
    * - `one`: The smallest positive natural number, represented as the successor of `Zero`.
    * - `plus`: Addition of two natural numbers, defined recursively.
    * - `times`: Multiplication of two natural numbers, defined recursively using addition.
    * - `compare`: Comparison of two natural numbers, defined recursively.
    */
  implicit object natIsSemiring extends Semiring[Nat] with Ordering[Nat] {
    /**
      * Represents the zero value of a natural number.
      *
      * @return the Zero instance of the Nat type, representing the starting point of natural numbers.
      */
    val zero: Nat = Zero

    /**
      * Represents the natural number one in Peano arithmetic.
      *
      * @return A value of type `Nat` representing the successor of zero.
      */
    val one: Nat = Succ(Zero)

    /**
      * Computes the sum of two natural numbers represented using Peano arithmetic.
      *
      * The addition operation is implemented iteratively with tail recursion:
      * - If both numbers are `Zero`, the result is `Zero`.
      * - If at least one of the numbers has a successor (`Succ`), the result
      * is recursively incremented by adding the predecessors of the numbers.
      *
      * @param x the first natural number to be added
      * @param y the second natural number to be added
      * @return the sum of the two natural numbers `x` and `y`
      */
    def plus(x: Nat, y: Nat): Nat = {
      @tailrec
      def inner(r: Nat)(w: Nat, z: Nat): Nat =
        (w, z) match {
          case (Zero, Zero) => r
          case (Succ(wPred), _) => inner(r.inc)(wPred, z)
          case (_, Succ(zPred)) => inner(r.inc)(w, zPred)
        }

      inner(Zero)(x, y)
    }

    /**
      * Computes the product of two natural numbers represented using Peano arithmetic.
      *
      * The multiplication operation is implemented iteratively with tail recursion:
      * - If either number is `Zero`, the result is `Zero`.
      * - Otherwise, the result is computed by recursively adding one number to itself
      * based on the value of the other number.
      *
      * @param x the first natural number to be multiplied
      * @param y the second natural number to be multiplied
      * @return the product of the two natural numbers `x` and `y`
      */
    def times(x: Nat, y: Nat): Nat = {
      @tailrec
      def inner(r: Nat)(w: Nat, z: Nat): Nat = (w, z) match {
        case (Zero, _) | (_, Zero) =>
          r
        case (`one`, `one`) =>
          r
        case (`one`, _) =>
          plus(r, y)
        case (_, `one`) =>
          plus(r, x)
        case (Succ(wPred), _) =>
          inner(plus(r, z))(wPred, z)
      }

      val ten = Nat(10)
      // XXX if either `Nat` object is greater than ten (arbitrary),
      //  then we use `Int` multiplication instead (for performance reasons)
      if compare(x, ten) > 0 || compare(y, ten) > 0 then
        Nat(x.asInt * y.asInt)
      else
        inner(zero)(x, y)
    }

    /**
      * Compares two natural numbers represented using Peano arithmetic.
      *
      * The comparison is performed as follows:
      * - If both numbers are `Zero`, they are considered equal, and the result is 0.
      * - If one number is `Zero` and the other is not, the result is -1 or 1 depending on the order.
      * - If both numbers are successors, the comparison is performed recursively on their predecessors.
      *
      * @param x the first natural number to be compared
      * @param y the second natural number to be compared
      * @return an integer representing the result of the comparison:
      *         - 0 if `x` and `y` are equal
      *         - -1 if `x` is less than `y`
      *         - 1 if `x` is greater than `y`
      */
    def compare(x: Nat, y: Nat): Int = {
      @tailrec
      def inner(x: Nat, y: Nat): Int = (x, y) match {
        case (Zero, Zero) => 0
        case (Zero, _) => -1
        case (_, Zero) => 1
        case (Succ(xPred), Succ(yPred)) => inner(xPred, yPred)
      }

      inner(x, y)
    }
  }
}
