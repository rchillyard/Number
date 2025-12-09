package com.phasmidsoftware.number.core.numerical

import com.phasmidsoftware.number.core.algebraic.{Algebraic, Solution}
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.core.numerical.Number.convertExpression
import org.scalactic.Equality

trait FuzzyEquality {

  /**
    * An implicit equality implementation for instances of `NumberLike`.
    * Provides functionality to determine whether two instances are equal based
    * on their underlying types and properties.
    *
    * This object is used to enable tailored equality comparisons for `NumberLike` instances
    * according to their specific types, such as `Expression`, `Solution`, `Rational`, `Field`,
    * or `Number`.
    *
    * Equality checks are delegated to other equivalence implementations (`FieldEquality`
    * or `NumberEquality`) depending on the runtime type of the given `NumberLike` instance.
    *
    * Implementation Details:
    * - For an `Expression`, it uses the materialized form and delegates to `FieldEquality`.
    * - For a `Solution`, it converts to a `Field` and compares using `FieldEquality`.
    * - For a `Rational`, it converts to a `Number` and delegates to `NumberEquality`.
    * - For a `Field`, it directly delegates to `FieldEquality`.
    * - For a `Number`, it directly delegates to `NumberEquality`.
    */
  implicit object NumberLikeEquality extends Equality[NumberLike] {

    def areEqual(a: NumberLike, b: Any): Boolean = a match {
      case e: Expression =>
        FieldEquality.areEqual(e.materialize, b)
      case s: Solution =>
        FieldEquality.areEqual(s.asField, b)
      case r: Rational =>
        NumberEquality.areEqual(Number(r), b)
      case f: Field =>
        FieldEquality.areEqual(f, b)
      case n: Number =>
        NumberEquality.areEqual(n, b)
    }
  }

  /**
    * An implicit equality comparator for the `Field` trait used to test structural equality
    * between different instances of `Field` or between a `Field` instance and another value.
    *
    * This equality implementation delegates its comparison to different equality strategies
    * depending on the specific type of the `Field` instance.
    *
    * - If the `Field` instance is of type `Algebraic`, the equality is determined by solving
    * the algebraic expression and delegating to `NumberLikeEquality`.
    * - If the `Field` instance is of type `Complex`, it checks if the other value is an instance
    * of `Numerical` and evaluates equality using the `Complex.isSame` method.
    * - If the `Field` instance is of type `Real`, it delegates the comparison to `NumberEquality`.
    *
    * This equality comparison ensures compatibility with mixed types while maintaining precise
    * behavior for the underlying mathematical constructs.
    */
  implicit object FieldEquality extends Equality[Field] {

    def areEqual(a: Field, b: Any): Boolean = a match {
      case algebraic: Algebraic =>
        NumberLikeEquality.areEqual(algebraic.solve, b)
      case complex: Complex => b match {
        case n: Numerical =>
          complex.isSame(n)
        case _ =>
          false
      }
      case Real(x) =>
        NumberEquality.areEqual(x, b)
    }
  }

  /**
    * An implicit object providing a custom equality implementation for comparing instances of the `Real` type.
    *
    * The equality comparison is based on specific matching rules:
    * - When compared with another instance of `Real`, the equality is determined by comparing their respective values.
    * - When compared with an instance of `Algebraic`, the equality is determined by solving the algebraic representation and using the associated `NumberLikeEquality`.
    * - For all other cases, the comparison falls back to the `NumberEquality` logic.
    *
    * This implementation is useful for ensuring that comparisons between `Real` and other numerical representations behave in a predictable and type-safe manner.
    */
  implicit object RealEquality extends Equality[Real] {

    def areEqual(a: Real, b: Any): Boolean = b match {
      case Real(y) =>
        a.x.isSame(y)
      case alg: Algebraic =>
        NumberLikeEquality.areEqual(alg.solve, b)
      case _ =>
        NumberEquality.areEqual(a.x, b)
    }
  }

  /**
    * Implicit object that provides equality logic for the `Number` type within the context of an `Equality[Number]` instance.
    *
    * Defines custom equality behavior for `Number` objects in comparison to various types,
    * including `Expression`, primitives (`Int`, `Double`), `Rational`, and `NumberLike`.
    */
  implicit object NumberEquality extends Equality[Number] {

    def areEqual(a: Number, b: Any): Boolean = b match {
      case n: Number =>
        a.isSame(n)
      case n: Expression =>
        a.compare(n) == 0
      case x: Int =>
        a.isSame(Number(x))
      case x: Rational =>
        a.isSame(Number(x))
      case x: Double =>
        a.isSame(Number(x))
      case n: NumberLike =>
        NumberLikeEquality.areEqual(n, a)
    }
  }
}
