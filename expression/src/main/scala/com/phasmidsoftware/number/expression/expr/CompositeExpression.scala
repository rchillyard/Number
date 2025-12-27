/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.core.Valuable.valuableToMaybeField
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.{Eager, NatLog, QuadraticSolution, RationalNumber, Structure}
import com.phasmidsoftware.number.algebra.util.FP
import com.phasmidsoftware.number.core.algebraic.{Algebraic_Quadratic, Quadratic}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Field, Number, Real}
import com.phasmidsoftware.number.expression.expr.Expression.em.{DyadicTriple, MonadicDuple}
import com.phasmidsoftware.number.expression.expr.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.{algebra, core, expression}
import java.util.Objects
import scala.language.implicitConversions

type OldNumber = Number

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
    * Method to determine if this `NumberLike` object is exact.
    * For instance, `Number.pi` is exact, although if you converted it into a `PureNumber`, it would no longer be exact.
    *
    * @return true if this `NumberLike` object is exact in the context of No factor, else false.
    */
//  def isExact: Boolean =
//    evaluateAsIs.exists(_.isExact)

  /**
    * If this `Valuable` is exact, it returns the exact value as a `Double`.
    * Otherwise, it returns `None`.
    * NOTE: do NOT implement this method to return a Double for a fuzzy Real--only for exact numbers.
    *
    * @return Some(x) where x is a Double if this is exact, else None.
    */
  def maybeDouble: Option[Double] = FP.whenever(isExact)(evaluate(RestrictedContext(PureNumber)).flatMap(_.maybeDouble))

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
  def simplifyComponents: em.AutoMatcher[Expression]

  /**
    * Simplifies the exact form of a `CompositeExpression` by identifying and transforming
    * sub-expressions into their precise, simplified forms. This method focuses on exact
    * mathematical simplifications that can be performed without introducing approximations.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying
    *         exact expressions. The result contains the simplified `Expression` if successful,
    *         or indicates no simplification was possible.
    */
  def simplifyExact: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyExact") {
      (expr: Expression) =>
        expr.evaluateAsIs match {
          case Some(value) =>
//            println(s"BiFunction: simplifyExact: value = $value")
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
  def simplifyTrivial: em.AutoMatcher[Expression]

  /**
    * Simplifies an `Expression` by checking if its value can be directly evaluated into a constant.
    * If the `Expression` can be evaluated as a `Field`, it is replaced with a `Literal` containing that value.
    * Otherwise, no simplification is performed, and the match operation indicates a miss.
    *
    * @return an `em.AutoMatcher[Expression]` that matches `Expression` instances which can be directly
    *         evaluated into constants, and simplifies them by replacing with a `Literal`. If simplification
    *         is not possible, it returns a miss state with the original `Expression`.
    */
  def simplifyConstant: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("simplifyConstant") {
    expr =>
      expr.evaluateAsIs match {
        case Some(f) =>
          em.MatchCheck(Expression(f))(expr).map(_.simplify)
        case _ =>
          em.Miss("matchSimpler: cannot be simplified", expr)
      }
  }

  /**
    * Attempts to simplify this `CompositeExpression` by applying matching mechanisms on its components.
    * It delegates the simplification logic to the defined matchers that aim to reduce the expression
    * into its simpler or optimized form, if possible.
    *
    * @return an `em.AutoMatcher[Expression]` encapsulating the logic to simplify the `CompositeExpression`.
    *         The result contains either the simplified `Expression` or indicates no further simplifications were possible.
    */
  def simplifyComposite: em.AutoMatcher[Expression]

  /**
    * Method to render this Structure in a presentable manner.
    *
    * @return a String
    */
  def render: String =
    materialize.normalize.render
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
        throw new IllegalArgumentException("Empty Sequence") // TESTME
      case h :: Nil =>
        h // TESTME
      case h :: j :: Nil =>
        expression.expr.BiFunction(h, j, Sum)
      case _ =>
        expression.expr.Aggregate(Sum, xs) // TESTME
    }

  /**
    * Creates a `Aggregate` instance from the given sequence of `Field` inputs.
    * Each `Field` is converted to a `Literal` expression and combined into a `Aggregate`.
    * TESTME
    *
    * @param f  the `ExpressionBiFunction` to be used to combine all the given elements.
    * @param xs The sequence of `Field` instances used to create the `Aggregate`.
    * @return An `Aggregate` instance containing the converted `Literal` expressions.
    */
  def create(f: ExpressionBiFunction, xs: Eager*): Expression =
    apply(f, xs map (x => Literal(x, Some(x.render)))) // TESTME
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
  def isExact: Boolean = x.isExact

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression] = Seq(x) // TESTME

  /**
    * Method to determine the depth of this Expression.
    *
    * @return the 1 + depth of x.
    */
  def depth: Int =
    1 + x.depth

  /**
    * Action to simplify this Expression as a Field.
    *
    * @return the materialized Field.
    */
  def evaluate(context: Context): Option[Eager] =
    context.qualifyingEagerValue(x.evaluateAsIs flatMap f.applyExact)

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
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction: simplifyComponents") {
      case UniFunction(x, _) =>
        val matcher = matchSimpler map (z => copy(x = z))
        matcher(x)
    }

  /**
    * Attempts to apply trivial simplifications to this `Expression`.
    * Currently, this method always fails with a message indicating no trivial simplifications are available.
    *
    * @return an `em.AutoMatcher[Expression]` that always fails with a miss case,
    *         as no trivial simplifications are possible.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher("UniFunction: simplifyTrivial") {
      // XXX Take care of the cases whereby the inverse of a log expression is a log expression with operand and base swapped.
      case UniFunction(UniFunction(x, Ln), Reciprocal) =>
        em.Match(expression.expr.BiFunction(ConstE, x, Log))
      case UniFunction(BiFunction(x, b, Log), Reciprocal) =>
        em.Match(expression.expr.BiFunction(b, x, Log))
      // XXX we check for certain exact literal function results
      case UniFunction(e: ValueExpression, f) if e.monadicFunction(f).isDefined =>
        em.matchIfDefined(e.monadicFunction(f))(e)
      case UniFunction(r: Root, Reciprocal) =>
        em.Match(r.reciprocal)
      case UniFunction(r: Root, Negate) =>
        em.Match(r.negate)
      case expr =>
        em.Miss("UniFunction: simplifyTrivial: no trivial simplifications", expr)
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
  def simplifyComposite: em.AutoMatcher[Expression] =
    em.Matcher("simplifyComposite") {
      case UniFunction(UniFunction(x, f), g) if em.complementaryMonadic(f, g) =>
        em.Match(x)
      case x: Expression =>
        em.Miss[Expression, Expression]("UniFunction.simplifyComposite: not complementary", x)
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
    * @return a new UniFunction initialized with the components derived from the given MonadicDuple.
    */
  def apply(md: MonadicDuple): expression.expr.UniFunction = {
    val tuple = (md.l, md.r)
    new expression.expr.UniFunction(tuple._2, tuple._1)
  }

  implicit def convertFromMonadicDuple(md: MonadicDuple): expression.expr.UniFunction = apply(md)
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
    * Simplifies the components of a `BiFunction` expression by applying a matcher that reduces its
    * constituent expressions (`x` and `y`) to their simpler forms.
    *
    * @return An `AutoMatcher` for the `Expression` type that matches and simplifies `BiFunction` expressions.
    */
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyComponents") {
//      case b@BiFunction(r1@QuadraticRoot(_, _), r2@QuadraticRoot(_, _), f) if r1.maybeValue.isEmpty && r2.maybeValue.isEmpty =>
//        val so: Option[Solution] = f match {
//          case Sum => r1.solution add r2.solution
//          //          case Product => r1.solution multiply r2.solution
//          case _ => None
//        }
//        val eo: Option[Expression] = so map (s => Literal(Eager(Algebraic(s))))
//        em.matchIfDefined(eo)(b)

      // NOTE I'm confused by my own logic here. I don't know why we need this.
      case BiFunction(x, y, f) =>
        val matcher: em.Matcher[Seq[Expression], expression.expr.BiFunction] =
          em.sequence(matchSimpler) & em.lift { xs => val Seq(newX, newY) = xs; expression.expr.BiFunction(newX, newY, f) }
        // NOTE this is a Match when one or more of the components are simplified.
        val result = matcher.apply(List[Expression](x, y))
        result match {
          case z@em.Match(r) =>
            z
          case z =>
            z
        }
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
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction: simplifyTrivial") {
      case BiFunction(a, b, f) if matchingIdentity(a, f, left = true).contains(true) =>
        em.Match(b)
      case BiFunction(a, b, f) if matchingIdentity(b, f, left = false).contains(true) =>
        em.Match(a)
      case BiFunction(a, b, Sum) =>
        matchBiFunctionSum(a, b)
      case BiFunction(a, b, Product) =>
        matchBiFunctionProduct(a, b)
      case BiFunction(a, b, Power) =>
        matchBiFunctionPower(a, b)
      case BiFunction(a, b, Log) =>
        matchBiFunctionLog(a, b)
      // TODO Handle tha Atan cases
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplifications", this)
    }

  /**
    * Simplifies a `CompositeExpression` represented as a `BiFunction` by applying various matchers
    * to identify opportunities for simplification. Specifically:
    * - Converts the `BiFunction` into an `Aggregate` for consistent simplification processing.
    * - Attempts to simplify complementary terms within the `BiFunction`.
    * - Applies additional simplification logic as defined by `Expression.simplifyComposite` and `matchSimpler`.
    *
    * TODO try to shorten this method.
    *
    * If the expression cannot be simplified, the result will indicate the failure.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying the `BiFunction`.
    *         It provides either the simplified `Expression` or indicates that no simplification was possible.
    */
  def simplifyComposite: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction: simplifyComposite") {
    case BiFunction(a, b, Sum) if a == b =>
      em.Match(expression.expr.BiFunction(a, Two, Product))
    case BiFunction(a, b, Product) if a == b =>
      em.Match(expression.expr.BiFunction(a, Two, Power))
    case BiFunction(ConstE, ValueExpression(v: eager.Number, _), Power) =>
      em.Match(Literal(NatLog(v)))
    case BiFunction(r: Root, x, f) =>
      matchRoot(r, x, f)
    case BiFunction(x, r: Root, f) if f.commutes =>
      r.evaluateAsIs match {
        case Some(y: QuadraticSolution) =>
          modifyAlgebraicQuadratic(y, x, f)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: simplifyComposite: no trivial simplification for Root,  and $f", this) // TESTME
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
      val xSq = Expression.simplifyConstant(expression.expr.BiFunction(a, Two, Power)).getOrElse(expression.expr.BiFunction(a, Two, Power)) // x²
      em.Match(expression.expr.UniFunction(xSq, Negate))
    case BiFunction(UniFunction(a, Negate), b, Product) if a == b => // TESTME
      val xSq = Expression.simplifyConstant(expression.expr.BiFunction(a, Two, Power)).getOrElse(expression.expr.BiFunction(a, Two, Power))
      em.Match(expression.expr.UniFunction(xSq, Negate))
    // CONSIDER carefully reinstating this. But for now, it adds failed tests!
    //    case BiFunction(a, b, Product) =>
    //      matchProduct
    // NOTE this case is definitely required
    case x@BiFunction(_, _, _) =>
      // TODO this frequently results in a Miss which is interpreted as a failure.
      //  A Miss in simplifyComposite should be treated as the termination of the simplify process.
      // NOTE the reason we see this as a Miss so often, is that it is always
      // the last match to be attempted.
      ((em.complementaryTermsEliminatorBiFunction |
          em.matchBiFunctionAsAggregate & em.literalsCombiner) &
          em.alt(matchSimpler))(x)
    case x => // NOTE we cannot reach this case.
      em.Miss("simplifyComposite", x) // TESTME
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
    val cLeft = f.leftContext(context)
    val eo = for {
      x <- a.evaluate(cLeft)
      cRight <- x.maybeFactor(cLeft).map(q => f.rightContext(q)(context)) // XXX Don't split this up.
      y <- b.evaluate(cRight)
      z <- f.applyExact((x, y))
    } yield z
    context.qualifyingEagerValue(eo)

  /**
    * Provides the terms that comprise this `CompositeExpression`.
    *
    * @return a sequence of `Expression` objects representing the individual terms of this `CompositeExpression`.
    */
  def terms: Seq[Expression] =
    Seq(a, b) // TESTME

  /**
    * Render this BiFunction for debugging purposes.
    *
    * @return a String showing a, f, and b in parentheses (or in braces if not exact).
    */
  override def toString: String =
    s"BiFunction{$a $f $b}"

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
    * TESTME
    *
    * @return an Int depending on f, a, and b.
    */
  override def hashCode(): Int =
    java.util.Objects.hash(f, a, b) // TESTME

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
          em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for Algebraics and $f", this) // TESTME
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
      em.Miss[Expression, Expression](s"BiFunction: matchLiteral: ", expression.expr.BiFunction(l, x, f)) // TESTME
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
    case (r, p, Power) =>
      p.evaluate(RestrictedContext(PureNumber)) match {
        case Some(n: Q) =>
          em.Match(expression.expr.BiFunction(n.toRational, p, Power))
        case _ =>
          em.Miss("BiFunction:matchRoot Power", expression.expr.BiFunction(r, p, Power))
      }
    case (q1@QuadraticRoot(e1, b1), q2@QuadraticRoot(e2, b2), f) if e1 == e2 =>
      // TODO asInstanceOf unrelated type
      val quadratic: Quadratic = e1.asInstanceOf[Quadratic]
      f match {
        case Sum if b1 != b2 =>
          em.Match(quadratic.conjugateSum)
        case Product if b1 != b2 =>
          em.Match(quadratic.conjugateProduct)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: matchRoot: QuadraticRoots and $f", expression.expr.BiFunction(q1, q2, f)) // TESTME
      }
    case (q1@QuadraticRoot(_, _), q2@QuadraticRoot(_, _), Sum) =>
      val maybeRoot = q1 add q2
      em.matchIfDefined(maybeRoot)(expression.expr.BiFunction(q1, q2, f))
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
  private def matchBiFunctionSum(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (a, b) if a == b =>
        em.Match(expression.expr.BiFunction(a, Two, Product))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplification for Sum", this)
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
  private def matchBiFunctionProduct(a: Expression, b: Expression): em.MatchResult[Expression] =
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
      case (a, b) if a == b =>
        em.Match(expression.expr.BiFunction(a, Two, Power))
      case (BiFunction(w, x, Power), BiFunction(y, z, Power)) if w == y =>
        em.Match(expression.expr.BiFunction(w, x plus z, Power))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplification for Product", this)
    }

  /**
    * Attempts to match and simplify a bi-function of type "Power" based on specific rules.
    *
    * @param a The base expression of the power.
    * @param b The exponent expression of the power.
    * @return A MatchResult containing the simplified expression if a matching rule is found,
    *         or a Miss result if no rules apply.
    */
  private def matchBiFunctionPower(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (_, Zero) =>
        em.Match(One)
      case (_, MinusOne) =>
        em.Match(expression.expr.UniFunction(a, Reciprocal))
      case (BiFunction(x, y, Power), z) =>
        em.Match(expression.expr.BiFunction(x, y :* z, Power))
      case (ConstE, BiFunction(ConstI, ConstPi, Product)) | (ConstE, BiFunction(ConstPi, ConstI, Product)) =>
        em.Match(MinusOne)
      case (ConstE, Literal(ComplexCartesian(Number.zero, Number.pi), _)) =>
        em.Match(MinusOne) // NOTE Euler's identity
      case (ConstE, Literal(ComplexPolar(Number.pi, Number.piBy2, _), _)) =>
        em.Match(MinusOne) // NOTE Also Euler's identity
      case (x, BiFunction(y, z, Log)) if x == z =>
        em.Match(y)
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplifications for Power", this)
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
  private def matchBiFunctionLog(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (One, _) =>
        em.Match(Zero)
      case (a, b) if a == b =>
        em.Match(One)
      case (a, ConstE) =>
        em.Match(expression.expr.UniFunction(a, Ln))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: simplifyTrivial: no trivial simplifications for Log", this)
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
                em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $r $f $x (not Rational)", this) // TESTME
            }
          case Some(y: QuadraticSolution) if f.commutes =>
            modifyAlgebraicQuadratic(y, x, f)
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $r $f $x (not Real)", this) // TESTME
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $r $f $x (not Atomic)", this) // TESTME
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
                em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Rational)", this) // TESTME
            }
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Real)", this) // TESTME
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Atomic)", this) // TESTME
    }

  // TODO this is never called!
  private def matchProduct: em.MatchResult[Expression] = {
    val z: Option[Field] = for {
      x <- evaluateAsScalar(a)
      y <- evaluateAsScalar(b)
    } yield x * y
    // TODO sort this out!
    val product: Option[Expression] = z match {
      case Some(field) => Some(Literal(Eager(field)))
      case _ => None
    }
    em.matchIfDefined(product)(expression.expr.BiFunction(a, b, Product))
  }

  private def evaluateAsScalar[T <: Structure](x: Expression): Option[numerical.Field] =
    x.evaluate(RestrictedContext(PureNumber)).flatMap(valuableToMaybeField)
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
    * @return a new BiFunction initialized with the components derived from the given DyadicTriple.
    */
  def apply(dt: DyadicTriple): expression.expr.BiFunction = {
    val tuple = (dt.l.asTuple, dt.r)
    new expression.expr.BiFunction(tuple._1._2, tuple._2, tuple._1._1)
  }

  implicit def convertFromDyadicTriple(dt: DyadicTriple): expression.expr.BiFunction = apply(dt)
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
  def isExact: Boolean = xs.forall(_.isExact)

//  def simplifyExact: em.AutoMatcher[Expression] =
//    em.fail("Aggregate: simplifyExact") // TODO implement me properly

  /**
    * Simplifies the components of this `CompositeExpression` using a matching mechanism to identify
    * and transform sub-expressions into simpler forms if possible.
    *
    * @return the result of the simplification attempt encapsulated in a `MatchResult`, which either contains
    *         a simplified `Expression` or indicates that no simplification was possible.
    */
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("Aggregate: simplifyComponents") {
      case Aggregate(f, xs) =>
        val matcher: em.Matcher[Seq[Expression], Expression] =
          em.sequence(matchSimpler) & em.lift { ys => expression.expr.Aggregate(f, ys) }
        matcher(xs)
    }

  /**
    * Simplifies a given `Expression` by removing trivial components or replacing grouped terms
    * with their corresponding higher-level mathematical function (if applicable).
    *
    * This method uses pattern matching to identify and transform instances of `Aggregate`:
    * - Removes identity elements for the aggregate function.
    * - Groups identical terms and replaces them with a new aggregate expression
    * using the next higher mathematical function.
    *
    * @return an `AutoMatcher` for `Expression` capable of identifying and performing
    *         simplifications of trivial aggregates.
    */
  def simplifyTrivial: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction: simplifyTrivial") {
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
  def simplifyComposite: em.AutoMatcher[Expression] = {
    case t: expression.expr.Aggregate =>
      em.simplifyAggregate(t)
    case x: Expression => // TESTME
      em.Miss[Expression, Expression]("Aggregate.simplifyComposite: not aggregate", x)
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
    * TESTME
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): expression.expr.Aggregate =
    copy(xs = xs ++ ys) // TESTME

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
  def approximation(force: Boolean): Option[eager.Real] = { // TESTME
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

  /**
    * Combines a given `Expression`, an optional `Field`, and a `Context` into a new tuple containing an updated
    * optional field and context. The combination logic evaluates the expression within the given context, applying
    * transformations to the field and context when applicable.
    * 
    * CONSIDER this is never called
    *
    * @param x       the expression to be evaluated and combined with the field and context.
    * @param fo      an optional field representing a potential starting value or state that may be updated
    *                during the combination process.
    * @param context the current context used for evaluating the expression and determining the resulting
    *                updated context.
    * @return a tuple consisting of an updated optional field and the resulting context after processing the
    *         given expression with the provided field and context.
    */
  private def combineFieldsAndContexts(x: Expression, fo: Option[Eager], context: Context): (Option[Eager], Context) =
    (for a <- fo; b <- x.evaluate(context) yield {
      val field = function(a, b)
      field -> (for factor <- field.maybeFactor(context) yield function.rightContext(factor)(context))
    }) match {
      case Some((f, Some(qq))) =>
        Some(f) -> qq
      case _ =>
        None -> Context.AnyScalar
    }
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
    * @throws java.lang.IllegalArgumentException if the sequence of expressions is empty.
    */
  def create(function: ExpressionBiFunction, xs: Seq[Expression]): expression.expr.Aggregate =
    if xs.nonEmpty then
      new expression.expr.Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)") // TESTME

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
}

