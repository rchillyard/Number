package com.phasmidsoftware.number.algebra

import cats.kernel.Eq
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
  implicit object EagerEquality extends Equality[Eager] {
    def areEqual(a: Eager, b: Any): Boolean = (a, b) match {
      case (x: Eager, y: Eager) =>
        summon[Eq[Eager]].eqv(x, y)
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
  implicit object StructureEquality extends Equality[Structure] {

    /**
      * Compares two objects to determine if they are considered equal.
      *
      * If both objects are of type `Angle`, it uses an implicit equality instance
      * to determine their equality. For other types, it falls back to the default
      * equality operator `==`.
      *
      * @param a the first object of type `Structure` to compare.
      * @param b the second object of type `Any` to compare.
      * @return a boolean indicating whether the two objects are considered equal.
      */
    def areEqual(a: Structure, b: Any): Boolean = (a, b) match {
      case (x: Angle, y: Angle) =>
        summon[Eq[Angle]].eqv(x, y)
      case _ =>
        a == b
    }
  }

  /**
    * Implicit object that provides an implementation of the Equality type class for the Angle type.
    *
    * This defines a custom equality comparison for instances of Angle. The equality comparison
    * first checks if the other object is also of type Angle. If the other object is of type Angle,
    * it uses the equality comparison provided by the `cats.kernel.Eq[Angle]` type class to evaluate equality.
    * If the other object is not of type Angle, it defaults to using standard equality comparison.
    */
  implicit object AngleEquality extends Equality[Angle] {

    def areEqual(a: Angle, b: Any): Boolean = b match {
      case y: Angle =>
        implicitly[cats.kernel.Eq[Angle]].eqv(a, y)
      case _ =>
        a == b
    }
  }

  /**
    * Provides an implicit definition of equality for `RationalNumber` instances.
    *
    * This object overrides the `areEqual` method to determine equality of two objects:
    * - If the second object is a `RationalNumber`, the equality is derived using the 
    *   `Eq` instance for `RationalNumber` from the Cats library.
    * - For all other cases, standard equality is used.
    *
    * It implements the `Equality` typeclass for the `RationalNumber` type.
    */
  implicit object RationalNumberEquality extends Equality[RationalNumber] {

    def areEqual(a: RationalNumber, b: Any): Boolean = b match {
      case y: RationalNumber =>
        implicitly[Eq[RationalNumber]].eqv(a, y)
      case _ =>
        a == b
    }
  }
}
