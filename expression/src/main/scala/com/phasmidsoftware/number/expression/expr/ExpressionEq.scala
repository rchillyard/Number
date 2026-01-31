/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import cats.Eq
import com.phasmidsoftware.number.algebra.eager.Eager

/**
  * Cats Eq instance for Expression providing structural equality.
  * Two expressions are equal if they are structurally identical.
  */
object ExpressionEq {

  /**
    * Eq instance for Expression that performs structural equality comparison.
    * This compares the structure of expressions rather than their evaluated values.
    * For commutative operations (Sum, Product), operand order doesn't matter.
    */
  given Eq[Expression] = new Eq[Expression] {
    def eqv(x: Expression, y: Expression): Boolean = (x, y) match {
      // Atomic expressions - use default equality
      case (a: AtomicExpression, b: AtomicExpression) =>
        a == b

      // UniFunction - use default equality
      case (a: UniFunction, b: UniFunction) =>
        a == b

      // BiFunction with commutative operations
      case (BiFunction(a1, b1, f1), BiFunction(a2, b2, f2)) if isCommutative(f1) && f1 == f2 =>
        // For commutative operations, check both orderings
        (eqv(a1, a2) && eqv(b1, b2)) || (eqv(a1, b2) && eqv(b1, a2))

      // BiFunction with non-commutative operations
      case (BiFunction(a1, b1, f1), BiFunction(a2, b2, f2)) if f1 == f2 =>
        eqv(a1, a2) && eqv(b1, b2)

      // Aggregate with commutative operations
      case (Aggregate(f1, es1), Aggregate(f2, es2)) if isCommutative(f1) && f1 == f2 =>
        // For commutative operations, compare as sets (order-independent)
        es1.length == es2.length && es1.forall(e1 => es2.exists(e2 => eqv(e1, e2)))

      // Aggregate with non-commutative operations
      case (Aggregate(f1, es1), Aggregate(f2, es2)) if f1 == f2 =>
        es1.length == es2.length && es1.zip(es2).forall { case (e1, e2) => eqv(e1, e2) }

      // Different types or functions - not equal
      case _ => false
    }
  }

  /**
    * Determines if a binary function is commutative.
    * Currently handles Sum and Product as commutative operations.
    */
  private def isCommutative(f: ExpressionBiFunction): Boolean =
    f.commutes

  /**
    * Alternative Eq instance that compares expressions based on their evaluated values.
    * This is useful when you want to know if two different expressions represent the same value.
    * Note: This may not be suitable for all use cases as it materializes the expressions.
    */
  def eqByValue(implicit eagerEq: Eq[Eager]): Eq[Expression] =
    (x: Expression, y: Expression) => {
      (x.evaluateAsIs, y.evaluateAsIs) match {
        case (Some(a), Some(b)) => eagerEq.eqv(a, b)
        case (None, None) => x == y // Fall back to structural equality if neither evaluates
        case _ => false
      }
    }
}
