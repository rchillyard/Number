/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.WholeNumber.convIntWholeNumber
import com.phasmidsoftware.number.algebra.eager.{Angle, Eager, NatLog, QuadraticSolution, RationalNumber, Structure, WholeNumber}
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.algebraic.{Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Radian, Rational}
import com.phasmidsoftware.number.core.misc.Bumperator
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Number, Real}
import com.phasmidsoftware.number.expression.algebraic
import com.phasmidsoftware.number.expression.expr.Expression.em.{DyadicTriple, MonadicDuple}
import com.phasmidsoftware.number.expression.expr.Expression.{em, given_LatexRenderer_Expression, matchSimpler}
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier
import com.phasmidsoftware.number.{algebra, core, expression}

import java.util.Objects
import scala.language.implicitConversions
import scala.util.*

/**
  * An abstract class which extends Expression while providing an instance of ExpressionMatchers for use
  * with simplification.
  *
  */
sealed trait CompositeExpression extends Expression {
  /**
    * Indicates whether this expression is atomic.
    *
    * @return false as the default value, indicating the expression is not atomic.
    */
  def isAtomic: Boolean = false

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  lazy val maybeDouble: Option[Double] =
    FP.whenever(isExact)(evaluate(RestrictedContext(PureNumber)).flatMap(_.maybeDouble))

  /**
    * Determines if the current number is equal to zero.
    *
    * @return true if the number is zero, false otherwise
    */
  lazy val isZero: Boolean = evaluateAsIs.exists(x => x.isZero)

  /**
    * Determines the sign of the Monotone value represented by this instance.
    * Returns an integer indicating whether the value is positive, negative, or zero.
    *
    * @return 1 if the value is positive, -1 if the value is negative, and 0 if the value is zero
    */
  lazy val signum: Int = evaluateAsIs.map(_.signum).getOrElse(0)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression]

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  def doSimplifyOperands: em.AutoMatcher[Expression]

  /**
    * Simplifies the exact form of a `CompositeExpression` by identifying and transforming
    * sub-expressions into their precise, simplified forms. This method focuses on exact
    * mathematical simplifications that can be performed without introducing approximations.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying
    *         exact expressions. The result contains the simplified `Expression` if successful,
    *         or indicates no simplification was possible.
    */
  lazy val simplifyExact: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyExact") {
      (expr: Expression) =>
        expr.evaluateAsIs match {
          case Some(value) =>
            em.Match(ValueExpression(value)).filter(_.isExact) // NOTE double-check that the result is actually exact.
          case None =>
            em.Miss[Expression, Expression]("BiFunction: simplifyExact: no simplifications", this)
        }
    }

  /**
    * Attempts to simplify the `CompositeExpression` by identifying and reducing trivial expressions
    * into their simpler or more elementary forms, if possible.
    * A trivial simplification is one that depends on fortuitous expressions, not necessarily constants,
    * that can therefore be combined in some way.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify trivial forms
    *         within the `CompositeExpression`. The result contains either the simplified `Expression`
    *         or indicates that no trivial simplifications were possible.
    */
  def simplifyIdentities: em.AutoMatcher[Expression]

  /**
    * Simplifies an `Expression` by checking if its value can be directly evaluated into a constant.
    * If the `Expression` can be evaluated as a `Field`, it is replaced with a `Literal` containing that value.
    * Otherwise, no simplification is performed, and the match operation indicates a miss.
    *
    * @return an `em.AutoMatcher[Expression]` that matches `Expression` instances which can be directly
    *         evaluated into constants, and simplifies them by replacing with a `Literal`. If simplification
    *         is not possible, it returns a miss state with the original `Expression`.
    */
  lazy val simplifyConstant: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyConstant") {
    expr =>
      expr.evaluateAsIs match {
        case Some(f) =>
          em.MatchCheck(Expression(f))(expr).map(_.simplify)
        case _ =>
          em.Miss("matchSimpler: cannot be simplified", expr)
      }
  }

  /**
    * Attempts to simplify this `CompositeExpression` by applying pattern-based rewrites.
    * It delegates the simplification logic to the defined matchers that aim to reduce the expression
    * into its simpler or optimized form, if possible.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify the `CompositeExpression`.
    *         The result contains either the simplified `Expression` or indicates no further simplifications were possible.
    */
  def doSimplifyStructural: em.AutoMatcher[Expression]

  /**
    * Method to render this Structure in a presentable manner.
    * CONSIDER why don't we just render `simplified`` as an expression?
    *
    * @return a String
    */
  lazy val render: String = matchSimpler(this) match {
    case em.Match(e) =>
      e.render
    case _ =>
      renderAsExpression
  }

  /**
    * Renders this `CompositeExpression` as a string representation of the expression itself,
    * typically in a structured or human-readable mathematical format.
    *
    * @return a string representation of the `CompositeExpression` as an expression.
    */
  def renderAsExpression: String
}

/**
  * Companion object for the `Aggregate` case class.
  *
  * Provides a utility method to create an `Aggregate` instance by converting a sequence of `Field` inputs
  * into `Literal` expressions and wrapping them in an `Aggregate`.
  */
object CompositeExpression {

  /**
    * Creates an `Aggregate` instance from the given sequence of `Field` inputs.
    * Each `Field` is converted to a `Literal` expression and combined into an `Aggregate`.
    *
    * @param f  The function to be applied to all elements of the result.
    * @param xs The sequence of `Field` instances used to create the `Aggregate`.
    * @return An `Aggregate` instance containing the converted `Literal` expressions.
    */
  def apply(f: ExpressionBiFunction, xs: Seq[Expression]): Expression =
    xs.toList match {
      case Nil =>
        throw new IllegalArgumentException("Empty Sequence")
      case h :: Nil =>
        h
      case h :: j :: Nil =>
        BiFunction(h, j, Sum)
      case _ =>
        expression.expr.Aggregate(Sum, xs)
    }

  /**
    * Creates a `Aggregate` instance from the given sequence of `Field` inputs.
    * Each `Field` is converted to a `Literal` expression and combined into a `Aggregate`.
    *
    * @param f  the `ExpressionBiFunction` to be used to combine all the given elements.
    * @param xs The sequence of `Field` instances used to create the `Aggregate`.
    * @return An `Aggregate` instance containing the converted `Literal` expressions.
    */
  def create(f: ExpressionBiFunction, xs: Eager*): Expression =
    apply(f, xs map (x => Literal(x, Some(x.render))))
}

/**
  * This class represents a monadic function of the given expression.
  *
  * @param x the expression being operated on.
  * @param f the function to be applied to x.
  */
case class UniFunction(x: Expression, f: ExpressionMonoFunction) extends expression.expr.CompositeExpression {
  /**
    * Method to determine what `Factor`, if there is such, this `Structure` object is based on.
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
  lazy val terms: Seq[Expression] = Seq(x) // TESTME

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
  def approximation(force: Boolean): Option[eager.Real] =
    // TODO asInstanceOf
    x.approximation(force) map (x => f.apply(x).asInstanceOf[eager.Real])

  /**
    * Simplifies the components of this `Expression` by transforming it using the `matchSimpler`
    * expression transformer. The resulting transformed expression is used to create a copy
    * of the current instance with the updated components.
    *
    * @return an `em.AutoMatcher[Expression]` representing the transformation of the components
    *         of this `Expression` using the `matchSimpler` logic.
    */
  lazy val doSimplifyOperands: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction: simplifyOperands") {
      case UniFunction(x, f) =>
        componentsSimplifier(Seq[Expression](x), { xs => val Seq(newX) = xs; UniFunction(newX, f) })
    }

  /**
    * Attempts to apply trivial simplifications to this `Expression`.
    * Currently, this method always fails with a message indicating no trivial simplifications are available.
    *
    * @return an `em.AutoMatcher[Expression]` that always fails with a miss case,
    *         as no trivial simplifications are possible.
    */
  lazy val simplifyIdentities: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction: simplifyIdentities") {
      case uni@UniFunction(e, f) =>
        // TODO sort this out.
        if (e.isInstanceOf[ValueExpression]) {
          val ve = e.asInstanceOf[ValueExpression]
        }
        uni match {
          // XXX Take care of the cases whereby the inverse of a log expression is a log expression with operand and base swapped.
          case UniFunction(UniFunction(x, Ln), Reciprocal) =>
            em.Match(BiFunction(E, x, Log))
          case UniFunction(BiFunction(x, b, Log), Reciprocal) =>
            em.Match(BiFunction(b, x, Log))
          // XXX we check for certain exact literal function results
          case UniFunction(e: ValueExpression, f) if e.monadicFunction(f).isDefined =>
            val result = e.monadicFunction(f)
            em.matchIfDefined(result)(e)
          case UniFunction(r: Root, Reciprocal) =>
            em.Match(r.reciprocal)
          case UniFunction(r: Root, Negate) =>
            em.Match(r.negate)
          case expr =>
            em.Miss("UniFunction: simplifyIdentities: no trivial simplifications", expr)
        }
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
  lazy val doSimplifyStructural: em.AutoMatcher[Expression] =
    em.Matcher("doSimplifyStructural") {
      case UniFunction(UniFunction(x, f), g) if em.complementaryMonadic(f, g) =>
        em.Match(x)
      case x: Expression =>
        em.Miss[Expression, Expression]("UniFunction.doSimplifyStructural: not complementary", x)
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
    case that: expression.expr.UniFunction =>
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
    other.isInstanceOf[expression.expr.UniFunction]

  override def toString: String = f.toString + "(" + x + ")"
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
    *
    * @return a new UniFunction initialized with the components derived from the given MonadicDuple.
    */
  def apply(md: MonadicDuple): expression.expr.UniFunction = {
    val tuple = (md.l, md.r)
    new expression.expr.UniFunction(tuple._2, tuple._1)
  }

  implicit def convertFromMonadicDuple(md: MonadicDuple): expression.expr.UniFunction = apply(md)

  import com.phasmidsoftware.number.algebra.util.LatexRenderer

  /**
    * LatexRenderer for UniFunction expressions.
    *
    */
  implicit val uniFunctionLatexRenderer: LatexRenderer[UniFunction] = LatexRenderer.instance { uni =>
    s"$uni.f(${uni.x.render})"
  }
}

/**
  * This class represents a dyadic function of the two given expressions.
  *
  * @param a the first expression being operated on.
  * @param b the second expression being operated on.
  * @param f the function to be applied to a and b.
  */
case class BiFunction(a: Expression, b: Expression, f: ExpressionBiFunction) extends expression.expr.CompositeExpression {
  /**
    * Attempts to retrieve a factor based on the provided context.
    * This method evaluates whether there is an applicable factor within the given context.
    *
    * @param context the context in which the factor is evaluated.
    * @return an optional `Factor` if one qualifies under the provided context; otherwise, `None`.
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
  def isExact: Boolean = a.isExact && b.isExact

  /**
    * Renders the `CompositeExpression` as a string representation of the expression itself,
    * typically in a structured or human-readable mathematical format.
    *
    * @return a string representation of the `CompositeExpression` as an expression.
    */
  def renderAsExpression: String =
    s"(${a.render} $f ${b.render})"

  /**
    * Simplifies the components of a `BiFunction` expression by applying a matcher that reduces its
    * constituent expressions (`x` and `y`), individually, to their simpler forms.
    *
    * @return An `AutoMatcher` for the `Expression` type that matches and simplifies `BiFunction` expressions.
    */
  lazy val doSimplifyOperands: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyOperands") {
      case BiFunction(x, y, f) =>
        componentsSimplifier(Seq[Expression](x, y), { xs => val Seq(newX, newY) = xs; BiFunction(newX, newY, f) })
    }

  /**
    * Simplifies a given `BiFunction` expression where trivial simplifications can be applied.
    * This method identifies patterns in the expression that represent mathematical identities
    * or redundancies and replaces them with their simplified form. Examples of such patterns
    * include removing identities, handling zero and one in products or powers, and other basic
    * algebraic simplifications.
    *
    * The simplifications include:
    * - Removing or simplifying identity elements (e.g., `x * 1 = x`, `x + 0 = x`).
    * - Handling special cases like zeroes in products or powers.
    * - Applying reductions for specific constants or algebraic terms.
    *
    * If no simplification can be applied, the method encapsulates this as a "miss" result.
    * Implementations of this method aim to provide lightweight simplification logic,
    * with more complex cases deferred to other parts of the system.
    *
    * @return an `em.AutoMatcher[Expression]` instance encapsulating the simplified `Expression`
    *         if simplification was possible, or a "miss" if no trivial simplification could be applied.
    */
  lazy val simplifyIdentities: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction: simplifyIdentities") {
      case BiFunction(a, b, f) if matchingIdentity(a, f, left = true).contains(true) =>
        em.Match(b)
      case BiFunction(a, b, f) if matchingIdentity(b, f, left = false).contains(true) =>
        em.Match(a)
      case BiFunction(a, b, Sum) =>
        matchBiFunctionIdentitiesSum(a, b)
      case BiFunction(a, b, Product) =>
        matchBiFunctionIdentitiesProduct(a, b)
      case BiFunction(a, b, Power) =>
        matchBiFunctionIdentitiesPower(a, b)
      case BiFunction(a, b, Log) =>
        matchBiFunctionIdentitiesLog(a, b)
      case BiFunction(q1@QuadraticRoot(e1, b1), q2@QuadraticRoot(e2, b2), f) if e1 == e2 =>
        // TODO asInstanceOf unrelated type
        val quadratic: Quadratic = e1.asInstanceOf[Quadratic]
        f match {
          case Sum if b1 != b2 =>
            em.Match(quadratic.conjugateSum)
          case Product if b1 != b2 =>
            em.Match(quadratic.conjugateProduct)
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: matchRoot: QuadraticRoots and $f", BiFunction(q1, q2, f)) // TESTME
        }
      // TODO Handle tha Atan cases
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyIdentities: no trivial simplifications", this)
    }

  /**
    * Simplifies a `CompositeExpression` represented as a `BiFunction` by applying various matchers
    * to identify opportunities for simplification. Specifically:
    * - Converts the `BiFunction` into an `Aggregate` for consistent simplification processing.
    * - Attempts to simplify complementary terms within the `BiFunction`.
    * - Applies additional simplification logic as defined by `Expression.doSimplifyStructural` and `matchSimpler`.
    *
    * TODO try to shorten this method.
    *
    * If the expression cannot be simplified, the result will indicate the failure.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying the `BiFunction`.
    *         It provides either the simplified `Expression` or indicates that no simplification was possible.
    */
  lazy val doSimplifyStructural: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction: doSimplifyStructural") {
    case BiFunction(a, b, Sum) if a == b =>
      em.Match(BiFunction(a, Two, Product))
    case BiFunction(BiFunction(w, x, Power), BiFunction(y, z, Power), Product) =>
      if (w == y)
        em.Match(BiFunction(w, x plus z, Power))
      else
        em.Miss("bases not equal", this)
//    case BiFunction(a, UniFunction(b, Negate), Sum) if a == b && a.maybeFactor(AnyContext).contains(Radian) =>
//      em.Match(Literal(Angle.zero))
    case BiFunction(a, Literal(b, _), Sum) if (a.materialize.add(b)).toOption.exists(_.isZero) && a.maybeFactor(AnyContext).contains(Radian) =>
      em.Match(Literal(Angle.zero))
    case BiFunction(a, b, Product) if a == b =>
      em.Match(BiFunction(a, Two, Power))
//    case BiFunction(_, b, Power) if b.materialize.isZero =>
//      em.Match(One)
//    case BiFunction(_, Infinity, Power) =>
//      em.Match(Infinity)
    case BiFunction(BiFunction(a, b, Product), p, Power) =>
      em.Match(BiFunction(a ∧ p, b ∧ p, Product))
    case BiFunction(BiFunction(a, b, Power), p, Power) =>
      em.Match(BiFunction(a, b * p, Power))
    case BiFunction(r: Root, x, f) =>
      matchRoot(r, x, f)
    case BiFunction(x, r: Root, f) if f.commutes =>
      r.evaluateAsIs match {
        case Some(y: QuadraticSolution) =>
          modifyAlgebraicQuadratic(y, x, f)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: doSimplifyStructural: no trivial simplification for Root,  and $f", this) // TESTME
      }
    case BiFunction(l: Literal, q: QuadraticRoot, Sum) =>
      matchLiteral(l, q, Sum)
    case BiFunction(Literal(a: QuadraticSolution, _), x, f) =>
      modifyAlgebraicQuadratic(a, x, f)
    case BiFunction(x, Literal(a: QuadraticSolution, _), f) if f.commutes =>
      modifyAlgebraicQuadratic(a, x, f)
    // NOTE these first two cases are kind of strange! CONSIDER removing them.
    case BiFunction(a, UniFunction(b, Negate), Product) if a == b =>
      // NOTE: duplicate code
      val xSq = Expression.simplifyExact(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power)) // x²
      em.Match(expression.expr.UniFunction(xSq, Negate))
    case BiFunction(UniFunction(a, Negate), b, Product) if a == b => // TESTME
      val xSq = Expression.simplifyExact(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power))
      em.Match(expression.expr.UniFunction(xSq, Negate))
    // CONSIDER carefully reinstating this. But for now, it adds failed tests!
    //    case BiFunction(a, b, Product) =>
    //      matchProduct
    // NOTE this case is definitely required
    case x@BiFunction(_, _, _) =>
      // TODO this frequently results in a Miss which is interpreted as a failure.
      //  A Miss in doSimplifyStructural should be treated as the termination of the simplify process.
      // NOTE the reason we see this as a Miss so often, is that it is always
      // the last match to be attempted.
      ((em.complementaryTermsEliminatorBiFunction(em.isComplementary) |
        em.matchBiFunctionAsAggregate & em.literalsCombiner) &
        em.alt(matchSimpler))(x)
    case x => // NOTE we cannot reach this case.
      em.Miss("doSimplifyStructural", x) // TESTME
  }

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int =
    1 + math.max(a.depth, b.depth)

  /**
    * Action to simplify this Expression as a Field.
    * NOTE that because we need to be able to evaluate this Expression exactly,
    * we need to be sure that the Context passed in to f.evaluate is not None.
    *
    * @return the materialized Field.
    */
  def evaluate(context: Context): Option[Eager] =
    f.evaluate(a, b)(context)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression] =
    Seq(a, b)

  /**
    * Render this BiFunction for debugging purposes.
    *
    * @return a String showing a, f, and b in parentheses (or in braces if not exact).
    */
  override def toString: String = f match {
    case Log => s"log_$a($b)"
    case Power => s"($a ^ $b)"
    case Sum => s"($a + $b)"
    case Product => s"($a * $b)"
    case Atan => s"atan($a,$b)"
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
  def approximation(force: Boolean): Option[eager.Real] = {
    val maybeValuable = for {x <- a.approximation(true); y <- b.approximation(true)} yield f(x, y)
    // TODO asInstanceOf
    // FIXME this cast is a problem! We need to force the approximation to be be fuzzy otherwise we get a ClassCastException
    maybeValuable.asInstanceOf[Option[eager.Real]]
  }

  /**
    * Regular hashCode method.
    *
    * @return an Int depending on f, a, and b.
    */
  override def hashCode(): Int =
    java.util.Objects.hash(f, a, b)

  /**
    * An equals method which considers two BiFunctions, which are non-identical but symmetric, to be equal.
    *
    * @param obj the other object.
    * @return true if the values of these two expressions would be the same (without any evaluation).
    */
  override def equals(obj: Any): Boolean = obj match {
    case BiFunction(c, d, g) =>
      f == g && (operandsMatch(c, d) || f.commutes && operandsMatch(d, c))
    case _ =>
      false
  }

  /**
    * Determines if the two given operands match `a` and `b`.
    *
    * @param op1 the first operand
    * @param op2 the second operand
    * @return true if the operands match the criteria, false otherwise
    */
  private def operandsMatch(op1: Expression, op2: Expression): Boolean =
    a == op1 && b == op2

  /**
    * Matches a given literal expression against another expression using a specified binary function.
    * If a specific pattern is identified, returns a simplified or transformed expression wrapped in a MatchResult.
    * Otherwise, returns a Miss indicating the match was unsuccessful.
    *
    * CONSIDER 1st and 2nd params should probably by ValueExpression
    *
    * @param l the literal expression used as the matching base
    * @param x the expression to match against the literal
    * @param f the binary function dictating the transformation or operation between the literal and the expression
    * @return a MatchResult of type Expression indicating success (Match with the resulting expression)
    *         or failure (Miss with additional debug information)
    */
  private def matchLiteral(l: Expression, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] = (l, x, f) match {
    //    case (Literal(a@QuadraticSolution(_, _, _), _), q@QuadraticRoot(_, _), Sum) =>
    //      em.Match(Literal(a add q.algebraic))
    case (Literal(Algebraic_Quadratic(_, e1, b1), _), Literal(Algebraic_Quadratic(_, e2, b2), _), f) if e1 == e2 =>
      f match {
        case Sum if b1 != b2 =>
          em.Match(e1.conjugateSum)
        case Product if b1 != b2 =>
          em.Match(e1.conjugateProduct)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: matchLiteral: no trivial simplification for Algebraics and $f", this) // TESTME
      }
    //    case (a, b, Product) =>
    //      val qqq: Option[Valuable] = Product.applyExact(a, b)
    //      em.matchIfDefined(qqq)(this).map(q => expression.expr.ValueExpression(q))
    case (Literal(a: CanPower[Structure] @unchecked, _), Literal(b: RationalNumber, _), Power) =>
      em.matchIfDefined(a.pow(b).map(x => Literal(x)))(this)
    case (a, b, Power) =>
      val qqq: Option[Expression] = for {
        w <- a.evaluateAsIs
        z <- b.evaluate(RestrictedContext(PureNumber))
        y <- f.applyExact(w, z)
      } yield y
      em.matchIfDefined(qqq)(this)
    case _ =>
      em.Miss[Expression, Expression](s"BiFunction: matchLiteral: ", BiFunction(l, x, f)) // TESTME
  }

  /**
    * Matches a given root with an expression and applies the specified bi-function.
    *
    * @param r the root to match, which could be a simple root or a quadratic root
    * @param x the expression to match with the root
    * @param f the bi-function defining the operation to apply, such as Sum, Product, or Power
    * @return a `MatchResult` containing the resulting expression if the match is successful,
    *         otherwise a `Miss` detailing the reason the match failed
    */
  private def matchRoot(r: Root, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] = (r, x, f) match {
    case (r, p, Power) if !p.isInstanceOf[ValueExpression] =>
      p.evaluate(RestrictedContext(PureNumber)) match {
        //        case Some(n: Q) =>
        //          em.Match(BiFunction(r, Literal(n), Power)) `flatMap` matchSimpler
        case _ =>
          em.Miss("BiFunction:matchRoot Power", BiFunction(r, p, Power))
      }
    case (q1@QuadraticRoot(_, _), q2@QuadraticRoot(_, _), Sum) =>
      val maybeRoot = q1 add q2
      em.matchIfDefined(maybeRoot)(BiFunction(q1, q2, f))
    //    case (q@QuadraticRoot(_, _), Literal(a@Algebraic_Quadratic(_, _, _), _), Sum) =>
    //      em.Match(Literal(a + q.solution))
    case _ =>
      modifyQuadratic(r, x, f)
  }

  /**
    * Matches two expressions and simplifies them into a BiFunction if they are equal.
    * If the expressions do not satisfy the conditions for simplification, a Miss is returned.
    *
    * @param a the first expression to be matched
    * @param b the second expression to be matched
    * @return a MatchResult containing the simplified BiFunction if the expressions are equal,
    *         or a Miss if the conditions for simplification are not met
    */
  private def matchBiFunctionIdentitiesSum(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (a, b) if a.plus(b) == Zero =>
        em.Match(Zero) // TESTME there are other ways of simplifying this.
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionSum: no trivial simplification for Sum", this)
    }

  /**
    * Attempts to simplify two given expressions through pattern matching for special cases
    * of product operations that can be reduced to simpler forms.
    *
    * @param a the first expression to match and simplify
    * @param b the second expression to match and simplify
    * @return a match result containing the simplified expression if a trivial simplification pattern is found,
    *         otherwise a miss result with an explanation
    */
  private def matchBiFunctionIdentitiesProduct(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (Zero, _) | (_, Zero) =>
        em.Match(Zero)
      case (a, One) =>
        em.Match(a)
      case (One, b) =>
        em.Match(b)
      case (a, MinusOne) =>
        em.Match(expression.expr.UniFunction(a, Negate))
      case (MinusOne, b) =>
        em.Match(expression.expr.UniFunction(b, Negate))
      // CONSIDER the following case should be copied for Sum (not just Product)
      case (a, b) if !a.isExact && !b.isExact => // if both a and b are inexact, we might as well combine them here
        em.matchIfSuccess(a.materialize.multiply(b.materialize).map(Literal(_)))((a, b))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionProduct: no trivial simplification for Product", this)
    }

  /**
    * Attempts to match and simplify a bi-function of type "Power" based on specific rules.
    *
    * @param a The base expression of the power.
    * @param b The exponent expression of the power.
    * @return A MatchResult containing the simplified expression if a matching rule is found,
    *         or a Miss result if no rules apply.
    */
  private def matchBiFunctionIdentitiesPower(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (_, Zero) =>
        em.Match(One)
      case (_, MinusOne) =>
        em.Match(expression.expr.UniFunction(a, Reciprocal))
      case (_, Infinity) =>
        em.Match(Infinity)
      case (E, ValueExpression(v: eager.Number, _)) =>
        em.Match(Literal(NatLog(v)))
      case (x, BiFunction(y, z, Log)) if x == y =>
        em.Match(z)
      // TODO CHECK if this is correct: we should be checking more generally
      case (r@Literal(QuadraticSolution.phi, _), ValueExpression(w: WholeNumber, _)) if w - 1 >= 0 =>
        em.Match(BiFunction(r + 1, w - 1, Power))
      // Match either named constants like Two or Literal whole numbers
      case (r@QuadraticRoot(algebraic.QuadraticEquation(p, q), branch), exp) if p.negate.isUnity =>
        exp.evaluate(RestrictedContext(PureNumber)) match {
          case Some(rn: Q) =>
            rn.toRational match {
              case rat if rat != Rational.one =>
                em.Match(r.power(rat))
              case _ =>
                em.Miss("phi identity: exponent is unity or not rational", this)
            }
          case _ =>
            em.Miss("phi identity: exponent not suitable", this)
        }
      case (r@QuadraticRoot(algebraic.QuadraticEquation(p, q), branch), Literal(w: WholeNumber, _)) =>
        em.Match(BiFunction(r + q, w - 1, Power))
      case (BiFunction(x, y, Power), z) =>
        em.Match(BiFunction(x, y :* z, Power))
      // CONSIDER generalizing this later but beware the general case breaks a lot of tests.
      case (Two, Literal(RationalNumber(r, _), _)) if r == Rational.half =>
        em.Match(Root.squareRoot(Rational(2), 0))
      // TODO this is a temporary suppression
      //      case (ValueExpression(x: ExactNumber,_), ValueExpression(RationalNumber(r: Rational, _), _)) if r==Rational.half =>
      //        em.Match(Root.squareRoot(x.toRational,0)) // NOTE we arbitrarily choose the positive branch here.
      case (E, BiFunction(ConstI, Pi, Product)) | (E, BiFunction(Pi, ConstI, Product)) =>
        em.Match(MinusOne)
      case (E, Literal(ComplexCartesian(Number.zero, Number.pi), _)) =>
        em.Match(MinusOne) // NOTE Euler's identity
      case (E, Literal(ComplexPolar(Number.pi, Number.piBy2, _), _)) =>
        em.Match(MinusOne) // NOTE Also Euler's identity
      case (x, BiFunction(y, z, Log)) if x == z =>
        em.Match(y)
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionPower: no trivial simplifications for Power", this)
    }

  /**
    * Matches two expressions and attempts to simplify them based on a set of predefined rules
    * for handling specific cases. If matching succeeds, a simplified `Expression` is returned.
    * Otherwise, it returns a failed match result with an appropriate message.
    *
    * @param a the first `Expression` to be matched
    * @param b the second `Expression` to be matched
    * @return an `em.MatchResult[Expression]`, representing either a successfully matched and
    *         simplified expression or a failure with a descriptive message
    */
  private def matchBiFunctionIdentitiesLog(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (One, _) =>
        em.Match(Zero)
      case (a, b) if a == b =>
        em.Match(One)
      case (a, E) =>
        em.Match(expression.expr.UniFunction(a, Ln))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionLog: no trivial simplifications for Log", this)
    }

  /**
    * Checks if a given `Expression` matches the identity element of a specific `ExpressionBiFunction`.
    * This method identifies if the evaluated value of the given `Expression` is equal to the identity
    * element provided by the `ExpressionBiFunction`. The search for the identity depends on whether
    * the function operates on the left or right operand.
    *
    * @param exp  the `Expression` to be evaluated and checked against the identity element.
    * @param f    the `ExpressionBiFunction` containing the potential identity elements.
    * @param left a boolean determining whether to check the left operand's identity (`true`) or
    *             the right operand's identity (`false`). If the right identity is not present,
    *             the method will fall back to the left identity.
    *
    * @return an `Option[Boolean]` indicating whether the `Expression` matches the identity:
    *         - `Some(true)` if the match is successful.
    *         - `Some(false)` if the match fails or if the `Expression` is not atomic.
    *         - `None` if no identity element exists in the provided `ExpressionBiFunction`.
    */
  private def matchingIdentity(exp: Expression, f: ExpressionBiFunction, left: Boolean): Option[Boolean] =
    exp match {
      case expression: AtomicExpression =>
        for
          identity <- if left then f.maybeIdentityL else f.maybeIdentityR orElse f.maybeIdentityL
          field <- expression.evaluateAsIs
        yield field == identity
      case _ =>
        Some(false)
    }

  /**
    * Scales a quadratic algebraic term by a given expression and attempts to simplify the operation.
    * If the expression is atomic and reducible (e.g., a rational number), the scaling is simplified.
    * Otherwise, returns a "miss" result indicating that no simplification was possible.
    *
    * @param r the `Algebraic_Quadratic` term that is being scaled.
    * @param x the `Expression` to scale `a` by.
    * @return a `MatchResult[Expression]`, which either contains the simplified scaled expression
    *         or indicates that no simplification was possible.
    */
  private def modifyQuadratic(r: Root, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] =
    x match {
      case expr: AtomicExpression =>
        expr.evaluateAsIs match {
          case Some(y: Real) if y.isExact =>
            (y.x.toNominalRational, f) match {
              case (Some(x), Power) =>
                em.MatchCheck(r.power(x))(x)
              case (_, _) =>
                em.Miss[Expression, Expression](s"BiFunction: modifyQuadratic: no trivial simplification for $r $f $x (not Rational)", this) // TESTME
            }
          case Some(y: QuadraticSolution) if f.commutes =>
            modifyAlgebraicQuadratic(y, x, f)
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: modifyQuadratic: no trivial simplification for $r $f $x (not Real)", this)
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: modifyQuadratic: no trivial simplification for $r $f $x (not Atomic)", this) // TESTME
    }

  /**
    * Scales a quadratic algebraic term by a given expression and attempts to simplify the operation.
    * If the expression is atomic and reducible (e.g., a rational number), the scaling is simplified.
    * Otherwise, returns a "miss" result indicating that no simplification was possible.
    *
    * @param a the `Algebraic_Quadratic` term that is being scaled.
    * @param x the `Expression` to scale `a` by.
    * @return a `MatchResult[Expression]`, which either contains the simplified scaled expression
    *         or indicates that no simplification was possible.
    */
  private def modifyAlgebraicQuadratic(a: QuadraticSolution, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] =
    x match {
      case expr: AtomicExpression =>
        expr.evaluateAsIs match {
          case Some(y: Real) if y.isExact =>
            (y.x.toNominalRational, f) match {
              case (Some(z), Sum) =>
                em.Match(Literal(a.add(z)))
              //              case (Some(z), Product) =>
              //                em.Match(Literal(a.scale(z)))
              //              case (Some(r), Power) =>
              //                 XXX in this case, we revert this `Algebraic_Quadratic` (viz., a Field) to a `Root` (viz., an `Expression`)
              //                val root = Root(a.equ, a.branch).power(r)
              //                em.Match(root)
              case (_, _) =>
                em.Miss[Expression, Expression](s"BiFunction: modifyAlgebraicQuadratic: no trivial simplification for $a $f $x (not Rational)", this) // TESTME
            }
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: modifyAlgebraicQuadratic: no trivial simplification for $a $f $x (not Real)", this)
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: modifyAlgebraicQuadratic: no trivial simplification for $a $f $x (not Atomic)", this)
    }
}

object BiFunction {
  /**
    * Constructs a BiFunction instance based on the provided DyadicTriple.
    * The method converts the left side of the DyadicTriple to a tuple and pairs it with the right side,
    * creating a BiFunction with the extracted components.
    *
    * NOTE: this is here mostly for historical purposes.
    * We no longer use DyadicTriple except as a convenience in the unit tests.
    *
    * @param dt the DyadicTriple from which the BiFunction will be initialized.
    *           The left side of the DyadicTriple is expected to provide a tuple,
    *           and the right side will be combined with this tuple to construct the BiFunction.
    *
    * @return a new BiFunction initialized with the components derived from the given DyadicTriple.
    */
  def apply(dt: DyadicTriple): BiFunction = {
    val tuple = (dt.l.asTuple, dt.r)
    new BiFunction(tuple._1._2, tuple._2, tuple._1._1)
  }

  /**
    * Analyzes the current instance and tries to interpret it as an aggregate expression.
    * An aggregate expression is a specific pattern of nested operations (e.g., sums, products, powers)
    * that can be represented in a more general form.
    *
    * The method matches different cases where the current instance can be reduced to an
    * aggregate representation. If such a representation exists, it returns it wrapped
    * in an `Option`. If the instance does not match any of the aggregate patterns,
    * it returns `None`.
    *
    * @return An `Option` containing the aggregate representation, or `None` if the instance
    *         cannot be interpreted as an aggregate.
    */
  def asAggregate(b: BiFunction): Option[Expression] = b match {
    case BiFunction(BiFunction(w, x, Sum), BiFunction(y, z, Sum), Product) =>
      Some(expression.expr.Aggregate(Sum, Seq(w :* y, w :* z, x :* y, x :* z)).simplify)
    // TODO add simplify to the following matches like the one above.
    case BiFunction(BiFunction(w, x, f), BiFunction(y, z, g), h) if f == g && g == h =>
      Some(expression.expr.Aggregate(f, Seq(w, x, y, z)))
    case BiFunction(BiFunction(w, x, Power), y, Power) =>
      Some(expression.expr.Aggregate(Power, Seq(w, x :* y)))
    case BiFunction(BiFunction(w, x, f), y, h) if f == h =>
      Some(expression.expr.Aggregate(f, Seq(w, x, y)))
    case BiFunction(x, BiFunction(y, z, f), h) if f == h =>
      Some(expression.expr.Aggregate(f, Seq(x, y, z)))
    case x =>
      None
  }

  implicit def convertFromDyadicTriple(dt: DyadicTriple): BiFunction = apply(dt)

  import com.phasmidsoftware.number.algebra.util.LatexRenderer

  /**
    * LatexRenderer for BiFunction expressions.
    *
    */
  implicit val biFunctionLatexRenderer: LatexRenderer[BiFunction] = LatexRenderer.instance { bi =>
    val latexRenderer = implicitly[LatexRenderer[Expression]]
    s"{${latexRenderer.toLatex(bi.a)} ${bi.f} ${latexRenderer.toLatex(bi.b)}}"
  }
}

/**
  * Represents a composite expression that computes the total of a sequence of expressions.
  * The sequence must be non-empty.
  *
  * @constructor Constructs an Aggregate instance with a sequence of expressions.
  * @param xs A non-empty sequence of expressions to be totaled.
  */
case class Aggregate(function: ExpressionBiFunction, xs: Seq[Expression]) extends expression.expr.CompositeExpression {
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
  lazy val isExact: Boolean = xs.forall(_.isExact)

  /**
    * Renders the `CompositeExpression` as a string representation of the expression itself,
    * typically in a structured or human-readable mathematical format.
    *
    * @return a string representation of the `CompositeExpression` as an expression.
    */
  def renderAsExpression: String =
    s"{$function ${xs.map(_.render).mkString(" ")}}"

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  lazy val doSimplifyOperands: em.AutoMatcher[Expression] =
    em.Matcher("Aggregate: simplifyOperands") {
      case Aggregate(f, xs) =>
        componentsSimplifier(xs, ys => expression.expr.Aggregate(f, ys))
    }

  /**
    * Simplifies a given `Expression` by removing trivial components or replacing grouped terms
    * with their corresponding higher-level mathematical function (if applicable).
    *
    * This method uses pattern matching to identify and transform instances of `Aggregate`:
    * - Removes identity elements for the aggregate function.
    * - Groups identical terms and replaces them with a new aggregate expression
    *   using the next higher mathematical function.
    *
    * @return an `AutoMatcher` for `Expression` capable of identifying and performing
    *         simplifications of trivial aggregates.
    */
  lazy val simplifyIdentities: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("Aggregate: simplifyIdentities") {
      // Remove identity values
      case a@Aggregate(f, xs) =>
        val nonIdentity = xs filterNot (x => f.maybeIdentityL.contains(x))
        // NOTE we ensure that it only returns a Match when the result is smaller than the original
        em.MatchResult(nonIdentity.size < xs.size, a, expression.expr.Aggregate(f, nonIdentity))
    }

  /**
    * Simplifies a composite `Expression` by leveraging the `simplifyAggregate` method.
    * This is typically used to reduce composite mathematical expressions into their simplest form when possible.
    *
    * @return an `AutoMatcher` for `Expression` that applies the simplification logic defined in `simplifyAggregate`.
    */
  lazy val doSimplifyStructural: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction: doSimplifyStructural") {
    case t: expression.expr.Aggregate =>
      em.simplifyAggregate(t)
    case x: Expression => // TESTME
      em.Miss[Expression, Expression]("Aggregate.doSimplifyStructural: not aggregate", x)
  }

  /**
    * Eliminates complementary terms from the current aggregate based on a specified predicate.
    *
    * The method identifies pairs of expressions within the aggregate that satisfy the provided
    * `complementaryPredicate` function and removes them, returning the remaining expressions.
    *
    * CONSIDER This is currently called only once so it could be converted to a lazy def by fixing the parameter.
    *
    * @param complementaryPredicate a function that determines whether two expressions within the
    *                               context of a provided aggregate function are complementary.
    *                               The predicate takes three arguments:
    *                               - The aggregate function (`ExpressionBiFunction`: e.g., Sum or Product).
    *                               - The first expression to evaluate (`Expression`).
    *                               - The second expression to evaluate (`Expression`).
    *                               It returns a `Boolean` indicating if the expressions are complementary.
    *
    * @return a `Try[List[Expression]]` containing the list of expressions after removing complementary pairs
    *         identified by the predicate. If an error occurs (e.g., unsupported operation), a `Failure` is returned.
    */
  def eliminateComplementaryTerms(complementaryPredicate: (ExpressionBiFunction, Expression, Expression) => Boolean): Try[List[Expression]] = {
    val invertFunction: Double => Double = function match {
      case Sum =>
        x => Math.abs(x)
      case Product =>
        x => if x < 1 then 1 / x else x
      case _ =>
        throw new IllegalArgumentException("complementaryTermsEliminatorAggregate: Power function not supported")
    }

    // NOTE this ordering is really only appropriate when f is Sum.
    // TODO find a better way to find complementary elements.
    val sortFunction: Expression => Double =
      x => invertFunction(FP.recover(x.approximation.map(_.toDouble))(ExpressionException(s"Cannot approximate $x for complementary term elimination")))

    val expressions = xs.sortBy(sortFunction)
    Try(expressions) map (sorted => Bumperator[Expression](sorted) { (x, y) => complementaryPredicate(function, x, y) }.toList)
  }

  /**
    * Attempts to retrieve a factor based on the provided context.
    * This method evaluates whether there is an applicable factor within the given context.
    *
    * @param context the context in which the factor is evaluated.
    * @return an optional `Factor` if one qualifies under the provided context; otherwise, `None`.
    */
  def maybeFactor(context: Context): Option[Factor] =
    evaluate(context) flatMap (v => v.maybeFactor(context))

  /**
    * Evaluates the given context to produce an optional field, leveraging the aggregate function
    * and associated evaluation logic. The method employs an iterative process to evaluate a sequence
    * of terms within the provided context while considering identity elements and context transformation rules.
    *
    * @param context the context in which the evaluation should take place. This defines the constraints
    *                and qualifications for the terms involved in the evaluation process.
    *
    * @return an `Option[Field]` representing the result of the evaluation. Returns `None`
    *         if the evaluation cannot produce a valid field or if an invalid context is encountered.
    */
  def evaluate(context: Context): Option[Eager] = {
    val vo = xs.foldLeft(function.maybeIdentityL) {
      (ao, x) =>
        for (a <- ao; b <- x.evaluateAsIs; c <- function.applyExact(a, b)) yield c
    }
    context.qualifyingEagerValue(vo)
  }

  /**
    * Adds a new expression to the current aggregate.
    *
    * @param x the expression to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def add(x: Expression): expression.expr.Aggregate =
    copy(xs = xs :+ x)

  /**
    * Adds a sequence of expressions to the current aggregate.
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): expression.expr.Aggregate =
    copy(xs = xs ++ ys)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return xs.
    */
  def terms: Seq[Expression] = xs

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the depth (an atomic expression has depth of 1).
    */
  def depth: Int =
    xs.map(_.depth).max + 1

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
    val identity: Eager = function.maybeIdentityL.getOrElse(Eager.zero) // NOTE should never require the default
    val vos: Seq[Option[eager.Real]] = xs map (x => x.approximation(force))
    // TODO asInstanceOf
    FP.sequence(vos) map (xs => xs.foldLeft[Eager](identity)(function.apply).asInstanceOf[eager.Real])
  }

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  override def toString: String =
    xs.mkString(s"Aggregate{${function.toString},", ",", "}")
}

/**
  * Companion object for the `Aggregate` class.
  */
object Aggregate {

  /**
    * Creates an empty `Aggregate` instance using the provided binary function.
    * The resulting `Aggregate` will have an empty sequence of expressions.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @return an empty `Aggregate` instance.
    */
  def empty(function: ExpressionBiFunction): expression.expr.Aggregate =
    new expression.expr.Aggregate(function, Seq.empty)

  /**
    * Creates an instance of `Aggregate` using the given binary function and sequence of expressions.
    * Throws an `IllegalArgumentException` if the sequence of expressions is empty.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @param xs       a sequence of `Expression` objects to be aggregated.
    * @return an `Aggregate` instance composed of the binary function and the sequence of expressions.
    * @note Throws java.lang.IllegalArgumentException if the sequence of expressions is empty.
    */
  def create(function: ExpressionBiFunction, xs: Seq[Expression]): expression.expr.Aggregate =
    if xs.nonEmpty then
      new expression.expr.Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)")

  /**
    * Constructs an `Aggregate` expression that applies the `Sum` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
    * @return an `Expression` representing the sum of the input expressions.
    */
  def total(xs: Expression*): expression.expr.Aggregate =
    create(Sum, xs)

  /**
    * Constructs an `Aggregate` expression that applies the `Product` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
    * @return an `Expression` representing the product of the input expressions.
    */
  def product(xs: Expression*): expression.expr.Aggregate =
    create(Product, xs)

  /**
    * Processes a sequence of expressions and separates them into angles, reciprocal angles,
    * and other non-angle expressions. The method applies specific transformations to recognize
    * angles and reciprocal angles, and combines the results into a single sequence.
    *
    * @param xs the sequence of expressions (`Seq[Expression]`) to be analyzed and processed.
    *           The expressions may include angles, reciprocal angles, or other types.
    * @return a sequence of processed expressions, including angles, reciprocal angles,
    *         and other non-angle expressions.
    */
  def getAnglesEtc(xs: Seq[Expression]): Seq[Expression] = {
    val angles: Seq[Expression] = xs.collect { case Literal(Angle(n, _), _) => Seq(Literal(n), Pi) }.flatten
    val others = xs.filterNot {
      case Literal(_: Angle, _) => true
      case _ => false
    }
    val reciprocalAngles = others.collect { case UniFunction(Literal(Angle(n, _), _), Reciprocal) => Seq(Literal(n).reciprocal, Pi.reciprocal) }.flatten
    val nonAngles = others.filterNot {
      case UniFunction(Literal(_: Angle, _), Reciprocal) => true
      case _ => false
    }
    angles ++ reciprocalAngles ++ nonAngles
  }

  /**
    * Determines whether the given sequence of expressions contains at least one
    * expression that represents a reciprocal angle.
    *
    * @param xs the sequence of `Expression` objects to analyze.
    *           Each `Expression` may represent an angle, a reciprocal angle,
    *           or another type of mathematical expression.
    * @return `true` if the sequence contains a reciprocal angle, otherwise `false`.
    */
  def hasReciprocalAngles(xs: Seq[Expression]): Boolean =
    xs.exists {
      case UniFunction(Literal(_: Angle, _), Reciprocal) => true
      case _ => false
    }

  /**
    * Determines whether the given sequence of expressions contains at least one
    * expression that represents an angle.
    *
    * @param xs the sequence of `Expression` objects to analyze. Each `Expression`
    *           may represent an angle or another type of mathematical expression.
    * @return `true` if the sequence contains at least one expression that represents an angle,
    *         otherwise `false`.
    */
  def hasAngles(xs: Seq[Expression]): Boolean =
    xs.exists {
      case Literal(_: Angle, _) => true
      case _ => false
    }
}

