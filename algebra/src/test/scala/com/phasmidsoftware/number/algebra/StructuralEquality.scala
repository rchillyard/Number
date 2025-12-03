package com.phasmidsoftware.number.algebra

import org.scalactic.Equality

trait StructuralEquality {

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
  implicit object StructureEquality extends Equality[Structure] {

    def areEqual(a: Structure, b: Any): Boolean = (a, b) match {
      case (x: Angle, y: Angle) =>
        implicitly[cats.kernel.Eq[Angle]].eqv(x, y)
      case _ =>
        a == b
    }
  }

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
  implicit object AngleEquality extends Equality[Angle] {

    def areEqual(a: Angle, b: Any): Boolean = b match {
      case y: Angle =>
        implicitly[cats.kernel.Eq[Angle]].eqv(a, y)
      case _ =>
        a == b
    }
  }
}
