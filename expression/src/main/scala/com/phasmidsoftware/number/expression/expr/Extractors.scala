package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.{Eager, InversePower, QuadraticSolution, WholeNumber}
import com.phasmidsoftware.number.core.inner.Rational
import com.phasmidsoftware.number.expression.algebraic.{Equation, QuadraticEquation}

object Extractors

/**
  * Companion object for the `IsZero` extractor.
  *
  * This object provides a custom pattern matching mechanism for the `Expression` trait.
  * It matches any `Expression` that evaluates to a zero value, allowing concise and readable
  * extraction of such cases.
  */
object IsZero {
  /**
    * Extractor method to match an `Expression` that evaluates to zero.
    * If the given `Expression` has a zero value, the method returns `Some(expr)`;
    * otherwise, it returns `None`.
    *
    * @param expr the `Expression` to check for a zero value
    * @return an `Option[Expression]` containing the input `expr` if it evaluates to zero, or `None` otherwise
    */
  def unapply(expr: Expression): Option[Expression] =
    Option.when(expr.isZero)(expr)
}

/**
  * Object `IsUnity` is used to identify and extract instances of `Expression` that represent unity.
  * Unity is determined based on the `isUnity` property of the `Expression`.
  */
object IsUnity {
  /**
    * Extracts an `Expression` if it represents unity.
    *
    * This method serves as an extractor that can be used in pattern matching
    * to determine if the provided `Expression` is unity, as defined by its `isUnity` property.
    * If the `Expression` is unity, it is returned wrapped in an `Option`. Otherwise, `None` is returned.
    *
    * @param expr the `Expression` to be checked and potentially extracted
    * @return an `Option` of the `Expression` if it represents unity, or `None` otherwise
    */
  def unapply(expr: Expression): Option[Expression] =
    Option.when(expr.isUnity)(expr)
}

/**
  * Provides a mechanism to determine whether an `Expression` can be represented
  * as an integer value and, if so, extracts the corresponding integer value.
  *
  * The `IsIntegral` object acts as an extractor, allowing pattern matching
  * against `Expression` instances to identify those that represent whole
  * numbers. This is particularly useful when working with complex expressions
  * that may or may not simplify to integers.
  */
object IsIntegral {
  /**
    * Extractor method for determining if an `Expression` represents an integer value.
    *
    * This method attempts to match the given `Expression` and identify if it can
    * be reduced to an integer value. If the `Expression` is a `ValueExpression`
    * containing a `WholeNumber`, or a `Rational` that is an integer (i.e., it has
    * no fractional part), the corresponding integer value is returned.
    *
    * @param expr the `Expression` to be analyzed for possible extraction of an integer value
    * @return an `Option[Int]` containing the integer value if the input expression can
    *         be reduced to an integer, or `None` otherwise
    */
  def unapply(expr: Expression): Option[Int] = expr match {
    case v: ValueExpression =>
      v.value match {
        case w: WholeNumber => Some(w.toInt)
        case r: Rational if r.isWhole => Some(r.toInt)
        case _ => None
      }
    case _ => None
  }
}

/**
  * The `QuadraticValue` object provides functionality for extracting components
  * of a quadratic-related `Expression` through pattern matching.
  * It identifies specific cases such as `Root` or `Literal` with a `QuadraticSolution`
  * and produces structured data accordingly.
  */
object QuadraticValue {
  /**
    * Extractor method for deconstructing an `Expression` into an `Equation` and an integer branch index, if applicable.
    * This method matches specific patterns within the `Expression`: either a `Root` case or a `Literal` case
    * containing a `QuadraticSolution`. If such patterns are not found, the method returns `None`.
    *
    * @param expr the `Expression` instance to be deconstructed
    * @return an `Option` containing a tuple of an `Equation` and an integer branch index if the match succeeds,
    *         otherwise `None`
    */
  def unapply(expr: Expression): Option[(Equation, Int)] = expr match {
    case Root(eq, branch) =>
      Some((eq, branch))
    case Literal(qs: QuadraticSolution, _) =>
      val r = QuadraticRoot(qs)
      Some((r.equation, r.branch))
    case _ =>
      None
  }
}

/**
  * An object that provides utilities for working with nth root expressions.
  * Specifically, it allows for the deconstruction and extraction of components
  * from expressions that match specific root-related patterns.
  */
object NthRoot {
  /**
    * Extracts components of an expression that matches specific root-related patterns.
    *
    * @param expr The expression to be examined for matches.
    * @return An optional tuple containing components of the matched expression:
    *         - The `Eager` operand of the root expression.
    *         - The integer degree of the root.
    *         - The integer branch adjustment factor.
    *         Returns `None` if the expression does not match the expected patterns.
    */
  def unapply(expr: Expression): Option[(Eager, Int, Int)] = expr match {
    case Root(QuadraticEquation(Rational.zero, r), branch) =>
      Some((-r, 2, branch))
    case Literal(v, _) => v match {
      case QuadraticSolution(eager.IsZero(base), InversePower(n, radicand), coeff, imag) =>
        Some((radicand, n, (1 - coeff) / 2))
      // Add other root types if they exist
      case _ => None
    }
    case _ => None
  }
}

/**
  * Object `SquareRoot` provides a mechanism to extract and match square root operations
  * represented as a specific `Expression`.
  */
object SquareRoot {
  /**
    * Extractor method for matching a square root operation represented as an `NthRoot` with a radicand,
    * index of 2, and a branch.
    *
    * @param expr the `Expression` to be matched and decomposed.
    * @return an `Option` containing a tuple with the radicand and branch of the square root expression
    *         if the input matches; otherwise, `None`.
    */
  def unapply(expr: Expression): Option[(Eager, Int)] = expr match {
    case NthRoot(radicand, 2, branch) => Some(radicand, branch)
    case _ => None
  }
}

/**
  * Object `CubeRoot` provides an extractor method for matching and decomposing
  * expressions that represent cube root operations. This is specifically aimed
  * at identifying instances of `NthRoot` where the root value is 3.
  */
object CubeRoot {
  /**
    * Extractor method used to match an `NthRoot` expression with a radicand and a root
    * value of 3, identifying cube roots specifically. This method also extracts the
    * branch value of the `NthRoot`.
    *
    * @param expr the input `Expression` to be matched and decomposed
    * @return an `Option` containing a tuple of the radicand (of type `Eager`) and the branch (an `Int`)
    *         if the input is a valid `NthRoot` with a root value of 3; otherwise, `None`
    */
  def unapply(expr: Expression): Option[(Eager, Int)] = expr match {
    case NthRoot(radicand, 3, branch) => Some(radicand, branch)
    case _ => None
  }
}