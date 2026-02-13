/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.algebra.eager

import com.phasmidsoftware.number.algebra
import com.phasmidsoftware.number.algebra.core.DyadicOperator
import com.phasmidsoftware.number.algebra.core.FuzzyEq.~=
import com.phasmidsoftware.number.algebra.eager.Real
import com.phasmidsoftware.number.algebra.util.LatexRenderer.LatexRendererOps
import com.phasmidsoftware.number.algebra.util.{AlgebraException, FP, LatexRenderer}

import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/**
  * Represents an Algebraic Structure.
  * In common parlance, we might call such an object, a "number" or "quantity" or a mathematical thing.
  * A `Structure` supports functionality such as exactness evaluation, numeric conversion,
  * rendering, and set membership analysis.
  * In general, we cannot order `Structure` objects, but we can test them for exactness.
  */
trait Structure extends Eager {

  /**
    * Attempts to approximate the current instance to a `Real` value.
    * If the instance is already of type `Real`, it is simply returned, wrapped inside `Some`.
    * Otherwise, depending on the value of `force`, it either attempts a conversion
    * to a default `Real` (if `force` is true), or returns `None`.
    *
    * NOTE that this method tries to keep exact quantities exact.
    *
    * @param force a boolean flag indicating whether to force the conversion to a default `Real`
    *              value when the current instance is not of type `Real`
    * @return an `Option[Real]` containing the approximated value if successful, or `None` if approximation fails
    */
  def approximation(force: Boolean = false): Option[Real] = this match {
    case real: Real => Some(real)
    case _ if force => convert(Real.zero)
    case _ => None
  }

  /**
    * Converts the given `Structure` object to an optional instance of the same type.
    *
    * TODO refactor this method to use a type class instance of `CanConvert[T, Structure]` instead of ClassTag.
    *
    * @param t the input object of type `T` which is a subtype of `Structure`.
    * @return an `Option` containing a transformed instance of type `T` if the conversion is successful, or `None` otherwise.
    */
  def convert[T <: Structure : ClassTag](t: T): Option[T]

  /**
    * Converts this `Structure` object into an optional `java.lang.Number` provided that the conversion can be
    * performed without loss of precision.
    *
    * The method determines whether the current `Structure` object can be represented as a `java.lang.Number`
    * by leveraging the `asNumber` method and further evaluating certain conditions:
    * - If the `Structure` object is an `ExactNumber` and its factor is `PureNumber`, the result
    *   is converted using `Value.asJavaNumber`.
    * - If the `Structure` object is a `Real` with a `wiggle` value below a specified tolerance,
    *   the result is also converted using `Value.asJavaNumber`.
    * - In all other cases, `None` is returned.
    *
    * @return an optional `java.lang.Number` representation of this object. The result is `Some(java.lang.Number)`
    *         if the conversion is successful under the stated conditions; otherwise, `None`.
    */
  def asJavaNumber: Option[java.lang.Number] = this match {
    case Real(value, _) =>
      Some(value)
    case s: Structure =>
      s.convert(Real.zero).flatMap(x => x.asJavaNumber)
  }

  /**
    * Performs a fuzzy equality comparison between the current `Structure` instance and another `Eager` instance.
    * The comparison is based on a specified tolerance level.
    *
    * If both instances are of type `Structure`, this method attempts to convert them to `Real`
    * and performs a fuzzy equality check with the specified tolerance. If the conversion or comparison fails,
    * a failure with an appropriate `AlgebraException` is returned.
    *
    * If either of the instances is not of type `Structure`, a failure with an `AlgebraException` is returned.
    *
    * @param p    the tolerance level (as a `Double`) to be used for the fuzzy equality comparison.
    * @param that the `Eager` instance to compare against the current instance.
    * @return a `Try[Boolean]` indicating whether the two instances are fuzzy equal within the specified tolerance.
    *         Returns a `Success(true)` or `Success(false)` if the comparison completes successfully,
    *         or a `Failure(AlgebraException)` if an error occurs.
    */
  override def fuzzyEqv(p: Double)(that: Eager): Try[Boolean] = (this, that) match {
    case (a: Structure, b: Structure) =>
      FP.toTry(for {
        p: Real <- a.convert(Real.one)
        q: Real <- b.convert(Real.one)
        r = p ~= q
      } yield r)(Failure(AlgebraException("Structure.fuzzyEqv")))
    case _ =>
      Failure(AlgebraException(s"Structure.fuzzyEqv: unexpected input: $this and $that"))
  }
}

/**
  * Companion object for the `Structure` trait, providing utility functions and type class instances
  * to facilitate operations on `Structure` objects.
  */
object Structure {

  /**
    * Provides an implicit instance of `DyadicOperator` for the `Structure` type, defining
    * the behavior for binary operations involving two `Structure` instances or a `Structure`
    * and its subtype. The operation is performed in a type-safe manner and returns the result
    * wrapped in a `Try` to handle potential errors.
    *
    * The behavior considers specific cases:
    * - If both operands are of type `Monotone`, the corresponding `DyadicOperator[Monotone]`
    *   instance is invoked.
    * - For all other cases, the provided binary operation is applied directly to the operands.
    *
    * @return An implicit instance of `DyadicOperator[Structure]`, which includes the `op` method 
    *         used to define binary operations on `Structure` and its subtypes.
    */// CONSIDER do we need this?
  given DyadicOperator[Structure] = new DyadicOperator[Structure] {
    def op[B <: Structure, Z](f: (Structure, B) => Try[Z])(x: Structure, y: B): Try[Z] = (x, y) match {
      case (a: Monotone, b: Monotone) =>
        implicitly[DyadicOperator[Monotone]].op(f)(a, b)
      case (a, b) =>
        f(a, b)
    }
  }

  /**
    * LatexRenderer for Eager (general case).
    *
    * Attempts to render based on the concrete type.
    */
  implicit val structureLatexRenderer: LatexRenderer[Structure] = LatexRenderer.instance {
    case m: Monotone => m.toLatex
    case m => throw new IllegalArgumentException(s"No LaTeX renderer for Structure type: ${m.getClass.getName}")
  }

  /**
    * Attempts to cast the provided `Structure` instance to the specified subtype `T`.
    * Throws an `AlgebraException` if the provided instance cannot be cast to the target type.
    *
    * @param x the input instance of `Structure` to be cast to the desired type `T`.
    * @tparam T the target subtype of `Structure` to which the input instance will be cast.
    * @return the input instance cast to the type `T` if the cast is valid and successful.
    * @note Throws [[com.phasmidsoftware.number.algebra.util.AlgebraException]] if the input instance cannot be cast to the type `T`.
    */
  def asT[T <: Structure : ClassTag](x: Structure): T = {
    val clazz = summon[ClassTag[T]].runtimeClass
    if (clazz.isAssignableFrom(x.getClass))
      x.asInstanceOf[T]
    else
      throw AlgebraException(s"Logic error: Can.asT failed to cast ${x.getClass} to $clazz")
  }
}
