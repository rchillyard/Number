/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.*
import com.phasmidsoftware.number.core.inner.Factor
import com.phasmidsoftware.number.expression.expr.Expression.em.MonadicDuple
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, em}
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier
import com.phasmidsoftware.number.{algebra, expression}

import java.util.Objects
import scala.language.implicitConversions

/**
  * This class represents a monadic function of the given expression.
  *
  * @param x the expression being operated on.
  * @param f the function to be applied to x.
  */
case class UniFunction(x: Expression, f: ExpressionMonoFunction) extends CompositeExpression {
  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
    * TODO implement this in CompositeExpression (then can remove all the overrides).
    *
    * @return an optional `Factor`.
    */
  def maybeFactor(context: Context): Option[Factor] =
    evaluate(context) flatMap (v => v.maybeFactor(context))

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
  lazy val isExact: Boolean = x.isExact

  /**
    * Renders the `UniFunction` as a string representation of an expression.
    * The format typically involves applying the function to its argument and
    * rendering it in a human-readable form.
    *
    * @return a string representing the `UniFunction` as an expression
    */
  lazy val renderAsExpression: String = s"$f(${x.render})"

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  lazy val terms: Seq[Expression] = Seq(x)

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the 1 + depth of x.
    */
  lazy val depth: Int =
    1 + x.depth

  /**
    * Action to simplify this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def evaluate(context: Context): Option[Eager] =
    f.evaluate(x)(context)

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
  def approximation(force: Boolean): Option[eager.Real] = {
    // CONSIDER is this correct? Shouldn't we try to evaluate first?
    x.approximation(force) flatMap (
      x =>
        f.apply(x) match {
          case r: eager.Real =>
            Some(r)
          case _ =>
            None

        }
      )
  }

  /**
    * Simplifies the components of this `Expression` by transforming it using the `matchSimpler`
    * expression transformer. The resulting transformed expression is used to create a copy
    * of the current instance with the updated components.
    *
    * @return an `em.AutoMatcher[Expression]` representing the transformation of the components
    *         of this `Expression` using the `matchSimpler` logic.
    */
  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction:operandsMatcher") {
      case u@UniFunction(_, f) =>
        componentsSimplifier(u.terms, { xs => val Seq(newX) = xs; UniFunction(newX, f) })
    }

  /**
    * Attempts to apply trivial simplifications to this `Expression`.
    * Currently, this method always fails with a message indicating no trivial simplifications are available.
    *
    * @return an `em.AutoMatcher[Expression]` that always fails with a miss case,
    *         as no trivial simplifications are possible.
    */
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction:identitiesMatcher") {
      case UniFunction(Zero, f@Odd()) if f != Reciprocal => // NOTE if the function has odd parity, then f(0) = 0.
        em.Match(Zero)
      case UniFunction(Zero, Cosh | Cosine) if f != Reciprocal =>
        em.Match(One)
      // XXX Take care of the cases whereby the inverse of a log expression is a log expression with operand and base swapped.
      case UniFunction(UniFunction(x, Ln), Reciprocal) =>
        em.Match(BiFunction(E, x, Log))
      case UniFunction(UniFunction(x, Exp), Reciprocal) =>
        em.Match(UniFunction(-x, Exp))
      case UniFunction(BiFunction(x, b, Log), Reciprocal) =>
        em.Match(BiFunction(b, x, Log))
      case UniFunction(Infinity, Exp) =>
        em.Match(Infinity)
      case UniFunction(x, Exp) =>
        matchExponential(x)
      // XXX we check for certain exact literal function results
      case UniFunction(e: ValueExpression, f) if e.monadicFunction(f).isDefined =>
        em.matchIfDefined(e.monadicFunction(f))(e)
      case UniFunction(I, Reciprocal) =>
        em.Match(-I)
      case UniFunction(r: Root, Reciprocal) =>
        em.Match(r.reciprocal)
      case UniFunction(r: Root, Negate) =>
        em.Match(r.negate)
      case expr =>
        em.Miss("UniFunction: identitiesMatcher: no trivial simplifications", expr)
    }

  /**
    * Simplifies a composite `Expression` by attempting to match it with a simpler form.
    * This method applies specific rules to detect and handle cases where the composite expression
    * consists of functions that are complementary (e.g., exponential/logarithmic, negation/negation and reciprocal/reciprocal).
    * If no such simplification is possible, the method returns a miss case without modification to the input.
    *
    * @return An `em.AutoMatcher[Expression]` that will either match the simplified expression or
    *         indicate a miss if no simplification can be applied.
    */
  lazy val structuralMatcher: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction:structuralMatcher") {
      // exp(i·θ)  →  Euler(1, θ)
      case UniFunction(BiFunction(I, θ, Product), Exp) =>
        em.Match(Euler(One, θ))

      // exp(i·θ)  →  Euler(1, θ)  (commuted: i on right)
      case UniFunction(BiFunction(θ, I, Product), Exp) =>
        em.Match(Euler(One, θ))

      // exp((i·θ) * k)  →  Euler(1, θ*k)  (parser produces this form for e.g. e^{i\pi/2})
      case UniFunction(BiFunction(BiFunction(I, θ, Product), k, Product), Exp) =>
        em.Match(Euler(One, θ * k))

      // exp(k * (i·θ))  →  Euler(1, θ*k)  (commuted outer)
      case UniFunction(BiFunction(k, BiFunction(I, θ, Product), Product), Exp) =>
        em.Match(Euler(One, θ * k))

      // exp(a + i·b)  →  Euler(exp(a), b)
      case UniFunction(BiFunction(a, BiFunction(I, b, Product), Sum), Exp) =>
        em.Match(Euler(UniFunction(a, Exp), b))

      // exp(a + b·i)  →  Euler(exp(a), b)  (commuted inner)
      case UniFunction(BiFunction(a, BiFunction(b, I, Product), Sum), Exp) =>
        em.Match(Euler(UniFunction(a, Exp), b))

      // exp(Aggregate{*, i, ...terms})  →  Euler(1, product of remaining terms)
      // (operandsMatcher flattens (i*π)*½ to Aggregate{*,i,π,½} before structuralMatcher runs)
      case UniFunction(Aggregate(Product, terms), Exp) if terms.contains(I) =>
        val remaining = terms.filterNot(_ == I)
        val theta = remaining.reduce((a, b) => BiFunction(a, b, Product))
        em.Match(Euler(One, theta))

      case UniFunction(UniFunction(x, f), g) if em.complementaryMonadic(f, g) =>
        em.Match(x)

      case x: Expression =>
        em.Miss[Expression, Expression]("UniFunction.structuralMatcher: not complementary", x)
    }

  /**
    * Compares this `AbstractRoot` instance with another object for equality.
    * The method checks if the other object is of a compatible type and
    * whether all relevant fields of both objects are equal.
    *
    * @param other the object to compare for equality with this instance
    * @return true if the given object is an instance of `AbstractRoot`,
    *         has `canEqual` compatibility with this instance, and
    *         if all relevant fields are equal; otherwise, false
    */
  override def equals(other: Any): Boolean = other match {
    case that: UniFunction =>
      that.canEqual(this) &&
        x == that.x &&
        f == that.f
    case _ =>
      false
  }

  /**
    * Generates a hash code for the instance based on its `equ` and `branch` fields.
    *
    * @return an integer hash code value obtained by hashing the `equ` and `branch` fields.
    */
  override def hashCode(): Int =
    Objects.hash(x, f)

  /**
    * Determines if the given object is of a type that can be compared for equality with this instance.
    *
    * @param other the object to compare with this instance
    * @return true if the given object is an instance of AbstractRoot, false otherwise
    */
  def canEqual(other: Any): Boolean =
    other.isInstanceOf[UniFunction]

  override def toString: String =
    s"$f(${x.show})"

  /**
    * Matches the given expression to specific exponential cases and provides a simplification if applicable.
    *
    * @param x the input expression to be matched and potentially simplified.
    * @return an `em.MatchResult[Expression]` indicating either a successful match with the simplified expression or a failure with an explanation.
    */
  private def matchExponential(x: Expression): em.MatchResult[Expression] = x match {
    case Zero =>
      em.Match(One)
    case One =>
      em.Match(E)
    case IPi() =>
      em.Match(MinusOne)
    case BiFunction(IPi(), Half, Product) | BiFunction(Half, IPi(), Product) =>
      em.Match(I)
    case _ =>
      em.Miss("UniFunction: matchExponential: no trivial simplifications", x)
  }
}

/**
  * The `UniFunction` companion object provides utility methods for initializing and converting `UniFunction` instances.
  * It includes methods to construct a `UniFunction` from a `MonadicDuple` as well as an implicit conversion mechanism.
  */
object UniFunction {
  /**
    * Constructs a UniFunction instance based on the provided MonadicDuple.
    * The method converts the left side of the MonadicDuple to a tuple and pairs it with the right side,
    * creating a UniFunction with the extracted components.
    *
    * NOTE: this is here mostly for historical purposes.
    * We no longer use MonadicDuple except as a convenience in the unit tests.
    *
    * @param md the MonadicDuple from which the UniFunction will be initialized.
    *           The left side of the MonadicDuple is expected to provide a tuple,
    *           and the right side will be combined with this tuple to construct the UniFunction.
    * @return a new UniFunction initialized with the components derived from the given MonadicDuple.
    */
  def apply(md: MonadicDuple): UniFunction = {
    val tuple = (md.l, md.r)
    new UniFunction(tuple._2, tuple._1)
  }

  implicit def convertFromMonadicDuple(md: MonadicDuple): UniFunction = apply(md)

  import com.phasmidsoftware.number.algebra.util.LatexRenderer

  /**
    * LatexRenderer for UniFunction expressions.
    *
    */
  implicit val uniFunctionLatexRenderer: LatexRenderer[UniFunction] = LatexRenderer.instance { uni =>
    s"$uni.f(${uni.x.render})"
  }
}
