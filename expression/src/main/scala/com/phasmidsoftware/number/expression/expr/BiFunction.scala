/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.expression.expr

import com.phasmidsoftware.number.algebra.core.*
import com.phasmidsoftware.number.algebra.eager
import com.phasmidsoftware.number.algebra.eager.{Angle, Complex, Eager, InversePower, IsInteger, NaturalExponential, QuadraticSolution, RationalNumber, Structure, WholeNumber}
import com.phasmidsoftware.number.core.inner.{Factor, PureNumber, Radian, Rational}
import com.phasmidsoftware.number.core.numerical
import com.phasmidsoftware.number.core.numerical.{ComplexPolar, Number}
import com.phasmidsoftware.number.expression.algebraic
import com.phasmidsoftware.number.expression.algebraic.QuadraticEquation
import com.phasmidsoftware.number.expression.expr.Expression.em.DyadicTriple
import com.phasmidsoftware.number.expression.expr.Expression.{ExpressionOps, em, given_LatexRenderer_Expression, matchSimpler}
import com.phasmidsoftware.number.expression.expr.ExpressionMatchers.componentsSimplifier
import com.phasmidsoftware.number.{algebra, core, expression}

import scala.language.implicitConversions

/**
  * This class represents a dyadic function of the two given expressions.
  *
  * @param a the first expression being operated on.
  * @param b the second expression being operated on.
  * @param f the function to be applied to a and b.
  */
case class BiFunction(a: Expression, b: Expression, f: ExpressionBiFunction) extends Multiple {
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
  lazy val operandsMatcher: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction:operandsMatcher") {
      case x@HasEuler() =>
        em.Miss(s"BiFunction:simplifyOperands: Euler should not be simplified here: $x", this)
      case b: BiFunction =>
        b.simplifyComponents
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
  lazy val identitiesMatcher: em.AutoMatcher[Expression] =
    em.Matcher[Expression, Expression]("BiFunction:identitiesMatcher") {
      // TODO these identity checks should use the isComplementary method for consistency
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
      case BiFunction(a, b, Atan) =>
        matchBiFunctionIdentitiesAtan(a, b)
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: identitiesMatcher: no trivial simplifications", this)
    }

  /**
    * Simplifies a `CompositeExpression` represented as a `BiFunction` by applying various matchers
    * to identify opportunities for simplification. Specifically:
    * - Converts the `BiFunction` into an `Aggregate` for consistent simplification processing.
    * - Attempts to simplify complementary terms within the `BiFunction`.
    * - Applies additional simplification logic as defined by `Expression.structuralMatcher` and `matchSimpler`.
    *
    * TODO try to shorten this method.
    *
    * If the expression cannot be simplified, the result will indicate the failure.
    *
    * @return an `em.AutoMatcher[Expression]` that encapsulates the logic for simplifying the `BiFunction`.
    *         It provides either the simplified `Expression` or indicates that no simplification was possible.
    */
  lazy val structuralMatcher: em.AutoMatcher[Expression] = em.Matcher[Expression, Expression]("BiFunction:structuralMatcher") {
    case BiFunction(a, b, Sum) if a == b =>
      em.Match(a * Two)
    case BiFunction(BiFunction(w, x, Power), BiFunction(y, z, Power), Product) if w == y =>
      em.Match(w ∧ (x + z))
    case BiFunction(a, Literal(b, _), Sum) if a.materialize.add(b).toOption.exists(_.isZero) && a.maybeFactor(AnyContext).contains(Radian) =>
      em.Match(Literal(Angle.zero))
    case BiFunction(BiFunction(x, k1, Product), BiFunction(y, k2, Product), Sum) if k1 == k2 =>
      em.Match(BiFunction(BiFunction(x, y, Sum), k1, Product))
    // cos(θ) + i·sin(θ)  →  Euler(1, θ)  (either operand order via EulerSumCommutative)
    case SumExpression(EulerSumCommutative(θ1, θ2)) if θ1 == θ2 =>
      em.Match(Euler(One, θ1))
    // cos(θ) - i·sin(θ)  →  Euler(1, -θ)  (conjugate)
    case BiFunction(Cos(θ1), UniFunction(BiFunction(I, Sin(θ2), Product), Negate), Sum) if θ1 == θ2 =>
      em.Match(Euler(One, -θ1))
    // r * exp(i·θ)  →  Euler(r, θ)  (either operand order via EulerProductCommutative)
    case ProductExpression(EulerProductCommutative(r, θ)) =>
      em.Match(Euler(r, θ))
    case BiFunction(a, b, Product) if a == b =>
      em.Match(a ∧ Two)
    case BiFunction(BiFunction(w, x, Sum), BiFunction(y, UniFunction(z, Negate), Sum), Product) if w == y && x == z =>
      em.Match((w ∧ Two) - (x ∧ Two))
    case BiFunction(BiFunction(a, b, Product), p, Power) =>
      em.Match((a ∧ p) * (b ∧ p))
    case BiFunction(BiFunction(a, b, Power), p, Power) =>
      em.Match(a ∧ (b * p))
    case BiFunction(NthRoot(radicand, n, _), IsIntegral(exp), Power) if n == exp =>
      em.Match(Literal(radicand, None))
    case BiFunction(a, UniFunction(b, Negate), Product) if a == b =>
      minusXSquared(a)
    case BiFunction(UniFunction(b, Negate), a, Product) if b == a =>
      minusXSquared(a)
    case b@BiFunction(r1: Root, r2: Root, Sum) =>
      em.matchIfDefined(r1 add r2)(b)

    // de Moivre rules — must come before HasEuler guard
    case BiFunction(Euler(r1, t1), Euler(r2, t2), Product) =>
      em.Match(Euler(r1 * r2, t1 + t2).simplify)
    case BiFunction(Euler(r1, t1), UniFunction(Euler(r2, t2), Reciprocal), Product) =>
      em.Match(Euler(r1 * UniFunction(r2, Reciprocal), t1 - t2).simplify)
    case BiFunction(Euler(r, t), n, Power) =>
      val result = Euler(r ∧ n, t * n)
      val simplified = result.simplify
      em.Match(simplified)
    case BiFunction(Euler(r1, t), Euler(r2, UniFunction(t2, Negate)), Sum) if r1 == r2 && t == t2 =>
      em.Match(Two * r1 * UniFunction(t, Cosine))
    case BiFunction(Euler(r1, t), UniFunction(Euler(r2, UniFunction(t2, Negate)), Negate), Sum) if r1 == r2 && t == t2 =>
      em.Match(Two * I * r1 * UniFunction(t, Sine))

    // r * Euler(1, θ)  →  Euler(r, θ)
    case BiFunction(r, Euler(One, θ), Product) =>
      em.Match(Euler(r, θ))
    case BiFunction(Euler(One, θ), r, Product) =>
      em.Match(Euler(r, θ))

    // FIXME: exp(i*θ)^n recognition fails when exp(i*θ) is simplified to a special value
    // (e.g. Euler(1,π/2) → I) before the outer Power expression is processed.
    // The following cases only fire when exp(i*θ) remains unsimplified.
    // See also: pending test "simplify exp(i*π/2)^2 to -1 via de Moivre" in EulerSpec.
    case BiFunction(UniFunction(BiFunction(I, θ, Product), Exp), n, Power) =>
      em.Match(Euler(One, θ * n).simplify)
    case BiFunction(UniFunction(BiFunction(θ, I, Product), Exp), n, Power) =>
      em.Match(Euler(One, θ * n).simplify)

    // I^n cycles with period 4
    // CONSIDER not sure if we need these here (as well as in identityMatchers)
    //    case BiFunction(I, IsIntegral(n), Power) => n % 4 match {
    //      case 0 => em.Match(One)
    //      case 1 => em.Match(I)
    //      case 2 => em.Match(MinusOne)
    //      case 3 => em.Match(UniFunction(I, Negate))
    //    }

    case x@HasEuler() =>
      em.Miss("structuralMatcher: deferred — has Euler operand", x)
    case x@BiFunction(_, _, _) =>
      x.genericStructuralSimplification
    case x =>
      em.Miss("structuralMatcher", x)
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
  lazy val terms: Seq[Expression] =
    Seq(a, b)

  /**
    * Render this BiFunction for debugging purposes.
    *
    * @return a String showing a, f, and b in parentheses (or in braces if not exact).
    */
  override lazy val toString: String = this match {
    case x@HasEuler() =>
      s"BiFunction(${x.a}, ${x.b}, ${x.f})"
    case BiFunction(a, b, f) =>
      f match {
        case Log => s"log_${b.show}(${a.show})"
        case Power => s"(${a.show} ^ ${b.show})"
        case Sum => s"(${a.show} + ${b.show})"
        case Product => s"(${a.show} * ${b.show})"
        case Atan => s"atan(${a.show},${b.show})"
      }
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
    // TODO this cast is a potential problem! We need to force the approximation to be be fuzzy otherwise we get a ClassCastException
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

  private def minusXSquared(x: Expression) = {
    val xSq = Expression.simplifyLazy(BiFunction(x, Two, Power)).getOrElse(BiFunction(x, Two, Power)) // x²
    em.Match(-xSq)
  }

  /**
    * Lazily initializes the `simplifyComponents` value which represents a simplified combination
    * of components based on provided terms and a lambda function.
    *
    * The lambda function takes a sequence of components, extracts two elements (newX, newY)
    * from the sequence, and combines them with a binary function `f` to produce the result.
    *
    * The initialization relies on `componentsSimplifier`, which processes the given terms
    * and applies the provided lambda function for simplification.
    */
  private lazy val simplifyComponents =
    componentsSimplifier(terms, { xs => val Seq(newX, newY) = xs; BiFunction(newX, newY, f) })

  /**
    * A private lazy evaluation that represents a generic structural simplification process.
    * Combines various functions and matchers using logical operators to streamline and simplify
    * structural components within the specified context.
    *
    * The simplification process includes:
    * - Eliminating complementary terms using a bi-function.
    * - Matching bi-functions as aggregates and combining literals.
    * - Applying alternative match simplification through `alt(matchSimpler)`.
    *
    * Operates within the scope of the current object (`this`) to apply context-specific simplifications.
    */
  private lazy val genericStructuralSimplification =
    ((em.complementaryTermsEliminatorBiFunction(em.isComplementary) |
      em.matchBiFunctionAsAggregate & em.literalsCombiner) &
      em.alt(matchSimpler))(this)

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
      em.Miss[Expression, Expression](s"BiFunction: matchLiteral: ", BiFunction(l, x, f))
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
      case AdditiveIdentityCommutative(a, _) =>
        em.Match(a)
      case (q1@QuadraticRoot(quadratic: QuadraticEquation, b1, _), q2@QuadraticRoot(e2, b2, _)) if quadratic == e2 && b1 != b2 =>
        em.Match(quadratic.conjugateSum)
      case (l: Literal, q: QuadraticRoot) => // TODO this should use the commutative extractor
        matchLiteral(l, q, Sum)
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
      case MultiplicativeIdentityCommutative(a, _) =>
        em.Match(a)
      case ExpressionNegationCommutative(a, _) =>
        em.Match(-a)
      case InversePowerTimesNumberCommutative(ip, b) =>
        em.matchIfDefined(simplifyInversePowerTimesNumber(ip, b))(this)
      case (q1@QuadraticRoot(quadratic: QuadraticEquation, b1, _), q2@QuadraticRoot(e2, b2, _)) if quadratic == e2 && b1 != b2 =>
        em.Match(quadratic.conjugateProduct)
      // CONSIDER the following case should be copied for Sum (not just Product)
      case (a, b) if !a.isExact && !b.isExact =>
        // NOTE if both a and b are inexact, we might as well combine them here
        // CONSIDER is this really such a good idea? We have simplifyByEvaluation to fall back on later.
        em.matchIfSuccess(a.materialize.multiply(b.materialize).map(Literal(_)))((a, b))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionProduct: no trivial simplification for Product", this)
    }

  /**
    * Simplifies the multiplication of an `InversePower` instance with a number that supports exponentiation.
    *
    * @param ip The `InversePower` instance representing an inverse power.
    * @param b  A numeric type instance capable of being raised to a power.
    */
  private def simplifyInversePowerTimesNumber(ip: InversePower, b: CanPower[eager.Number]) =
    b.pow(WholeNumber(ip.n)) map (e => Literal(InversePower(ip.n, ip.number * e)))

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
        em.Match(a.reciprocal)
      case (_, Infinity) =>
        em.Match(Infinity)
      case (E, ValueExpression(Complex(c), _)) if c.isImaginary && c.modulus == numerical.Number.pi =>
        em.Match(MinusOne)
      case (E, ValueExpression(v: eager.Number, _)) =>
        em.Match(Literal(NaturalExponential(v)))
      case (E, UniFunction(x, Ln)) =>
        em.Match(x)
      case (E, BiFunction(x, E, Log)) =>
        em.Match(x) // TESTME
      case (I, IsIntegral(n)) => em.Match(n % 4 match {
        case 0 => One
        case 1 => I
        case 2 => MinusOne
        case 3 => UniFunction(I, Negate)
      })
      case (E, BiFunction(I, Pi, Product)) | (E, BiFunction(Pi, I, Product)) =>
        em.Match(MinusOne)
      case (E, Literal(ComplexPolar(Number.pi, Number.piBy2, _), _)) =>
        em.Match(MinusOne) // TESTME
      case (E, x) =>
        em.Match(UniFunction(x, Exp))
      case (x, BiFunction(y, z, Log)) if x == y =>
        em.Match(z)
      case (r@Literal(QuadraticSolution.phi, _), IsIntegral(w)) if w >= 1 =>
        em.Match((r + One) ∧ (w - 1))
      case (r@QuadraticRoot(algebraic.QuadraticEquation(p, q), branch, _), exp) if p.negate.isUnity =>
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
      case (r@QuadraticRoot(algebraic.QuadraticEquation(p, q), branch, _), IsInteger(w)) =>
        val expression = (r + q) ∧ (w - 1)
        em.Match(expression)
      // CONSIDER generalizing this later but beware the general case breaks a lot of tests.
      case (Two, Literal(RationalNumber(r, _), _)) if r == Rational.half =>
        em.Match(Root.squareRoot(Rational(2), 0))
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
      case (a, E) => // TODO check that this is the correct order: value followed by the base
        em.Match(UniFunction(a, Ln))
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionLog: no trivial simplifications for Log", this)
    }

  /**
    * Matches two expressions and attempts to simplify arctangent operations.
    *
    * The Atan function computes angles based on the relationship between two parameters.
    * This method handles common special cases where the angle can be determined exactly.
    *
    * Special cases:
    * - atan(x>0, 0) = 0 (positive x-axis)
    * - atan(x<0, 0) = π (negative x-axis)
    * - atan(1, 1) = π/4 (45 degrees)
    * - atan(0, 1) = π/2 (90 degrees, positive y-axis)
    *
    * @param a the first parameter of the arctangent function
    * @param b the second parameter of the arctangent function
    * @return a MatchResult containing the simplified angle expression if a pattern matches,
    *         otherwise a Miss indicating no simplification was found
    */
  private def matchBiFunctionIdentitiesAtan(a: Expression, b: Expression): em.MatchResult[Expression] =
    (a, b) match {
      case (x, Zero) if x.signum > 0 =>
        em.Match(Literal(Angle.zero)) // atan(x>0, 0) = 0
      case (x, Zero) if x.signum < 0 =>
        em.Match(Literal(Angle.pi)) // atan(x<0, 0) = π
      case (One, One) =>
        em.Match(Literal(Angle.piBy4)) // atan(1, 1) = π/4
      case (Zero, One) =>
        em.Match(Literal(Angle.piBy2)) // atan(0, 1) = π/2
      case _ =>
        em.Miss[Expression, Expression]("BiFunction: matchBiFunctionIdentitiesAtan: no trivial simplifications for Atan", this)
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
    case BiFunction(Multiple(f, xs), Multiple(g, ys), Commutes(h)) if f == g && g == h =>
      Some(Aggregate(f, xs ++ ys))
    case BiFunction(Multiple(f, xs), y, Commutes(h)) if f == h =>
      Some(Aggregate(f, xs :+ y))
    case BiFunction(x, Multiple(f, ys), Commutes(h)) if f == h =>
      Some(Aggregate(f, x +: ys))
    case BiFunction(BiFunction(w, x, Sum), BiFunction(y, z, Sum), Product) =>
      Some(Aggregate(Sum, Seq(w :* y, w :* z, x :* y, x :* z)).simplify) // TODO why simplify?
    case BiFunction(BiFunction(w, x, Power), y, Power) =>
      Some(Aggregate(Power, Seq(w, x :* y)))
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

object ProductExpression {
  /**
    * Extractor for `BiFunction(a, b, Product)` — returns `(a, b)` as a tuple
    * suitable for use with `CommutativeExtractor` subtypes.
    *
    * @param expr the `Expression` to match
    * @return `Some((a, b))` if the expression is a Product, `None` otherwise
    */
  def unapply(expr: Expression): Option[(Expression, Expression)] =
    expr match {
      case BiFunction(a, b, Product) => Some((a, b))
      case _ => None
    }
}

/**
  * Extractor for `BiFunction(a, b, Sum)` — returns `(a, b)` as a tuple
  * suitable for use with `CommutativeExtractor` subtypes.
  */
object SumExpression {
  def unapply(expr: Expression): Option[(Expression, Expression)] =
    expr match {
      case BiFunction(a, b, Sum) => Some((a, b))
      case _ => None
    }
}

/**
  * Commutative extractor for the Euler recognition pattern cos(θ) + i·sin(θ).
  *
  * Matches a pair `(left, right)` where one side is `cos(θ)` and the other is
  * `i·sin(θ)` (in either multiplication order), returning `(θ_from_cos, θ_from_sin)`.
  * The caller guards with `θ1 == θ2`.
  *
  * `extractLeft` excludes the `i·sin` shape so the two roles are mutually exclusive,
  * preventing the swap path from producing spurious matches.
  */
object EulerSumCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression): Option[Expression] = e match {
    case BiFunction(I, Sin(_), Product) => None // belongs on the right
    case BiFunction(Sin(_), I, Product) => None // belongs on the right
    case Cos(θ) => Some(θ)
    case _ => None
  }

  protected def extractRight(e: Expression): Option[Expression] = e match {
    case BiFunction(I, Sin(θ), Product) => Some(θ)
    case BiFunction(Sin(θ), I, Product) => Some(θ)
    case _ => None
  }
}

/**
  * Commutative extractor for the Euler recognition pattern `r * exp(i·θ)`.
  *
  * Matches a pair `(left, right)` where one side is any modulus expression `r`
  * and the other is `exp(i·θ)`, returning `(r, θ)`.
  *
  * `extractLeft` excludes the `exp(i·…)` shape so the two roles are mutually
  * exclusive, preventing the swap path from producing spurious matches.
  */
object EulerProductCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression): Option[Expression] = e match {
    case UniFunction(BiFunction(I, _, Product), Exp) => None // belongs on the right
    case UniFunction(BiFunction(_, I, Product), Exp) => None // belongs on the right
    case r => Some(r)
  }

  protected def extractRight(e: Expression): Option[Expression] = e match {
    case UniFunction(BiFunction(I, θ, Product), Exp) => Some(θ)
    case UniFunction(BiFunction(θ, I, Product), Exp) => Some(θ)
    case _ => None
  }
}

/**
  * An object that extracts and matches expressions based on the commutative property
  * of multiplication with the identity element (unity).
  *
  * This object extends `CommutativeExtractor` and provides the specific implementation
  * for extracting expressions in the form of a multiplicative operation where one operand
  * is the multiplicative identity (unity), and the other is the remaining expression.
  *
  * It defines the following extraction rules:
  * - The `extractLeft` method always extracts and returns the input expression as-is.
  * - The `extractRight` method matches an expression that is the multiplicative identity
  *   (`IsUnity(x)`) and extracts the contained value `x`.
  *
  * The unapply mechanism allows this extractor to match pairs of expressions in a commutative
  * manner, ensuring symmetry in the analysis of multiplicative operations.
  */
object MultiplicativeIdentityCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression) = Some(e)

  protected def extractRight(e: Expression) = e match {
    case IsUnity(x) => Some(x)
    case _ => None
  }
}

/**
  * Extractor for commutative additive operations with an identity element (zero).
  *
  * This object is a specialized implementation of the `CommutativeExtractor` trait,
  * specifically designed to handle expressions where one side of a commutative addition
  * operation is an additive identity (e.g., zero).
  *
  * It operates under the assumption that addition is commutative and attempts to extract
  * sub-expressions from either side of a binary expression in a manner that accounts for
  * the commutativity of the operation.
  *
  * The left-hand side of the expression is simply returned as-is, while the right-hand
  * side is inspected to check if it represents the additive identity (zero). If the
  * right-hand side is zero, its associated sub-expression is extracted.
  */
object AdditiveIdentityCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression) = Some(e)

  protected def extractRight(e: Expression) = e match {
    case IsZero(x) => Some(x)
    case _ => None
  }
}

/**
  * Extractor object for handling negation expressions in a commutative manner.
  * Matches pairs of expressions where one is an `Expression` and the other is
  * a negated `ValueExpression(-1)`.
  *
  * This object extends the `CommutativeExtractor` to allow the extraction
  * of operands in any order (i.e., it supports both (A, B) and (B, A) configurations).
  */
object ExpressionNegationCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression): Option[Expression] = e match {
    case x: Expression => Some(x)
  }

  protected def extractRight(e: Expression): Option[Expression] = e match {
    case IsMinusOne(x) => Some(x)
    case _ => None
  }
}


/**
  * Extractor object for handling negation expressions in a commutative manner.
  * Matches pairs of expressions where one is a plain `Expression` and the other
  * is its negation `UniFunction(x, Negate)`.
  *
  * `extractLeft` excludes negated expressions so that the two roles are mutually
  * exclusive. This prevents the swap path in `CommutativeExtractor` from binding
  * `a` to a negated expression and producing a spurious `a == b` match.
  *
  * With this fix the commented-out shorthand in `structuralMatcher` works correctly:
  * {{{
  *   case ProductExpression(ExpressionComplementaryCommutative(a, b)) if a == b =>
  *     minusXSquared(a)
  * }}}
  */
object ExpressionComplementaryCommutative extends CommutativeExtractor[Expression, Expression] {
  protected def extractLeft(e: Expression): Option[Expression] = e match {
    case UniFunction(_, Negate) => None // negated expressions belong on the right
    case x => Some(x)
  }

  protected def extractRight(e: Expression): Option[Expression] = e match {
    case UniFunction(x, Negate) => Some(x) // return the inner (un-negated) expression
    case _ => None
  }
}

/**
  * Extractor object for commutative operations between an `InversePower` and a
  * value that can be powered, represented by `CanPower[eager.Number]`.
  *
  * This object extends `CommutativeExtractor`, enabling it to automatically
  * handle both `(left, right)` and `(right, left)` orderings in commutative
  * binary operations. Specifically, it extracts:
  * - An `InversePower` from the "left" side of the expression, provided the
  *   base of the inverse power (`n`) is greater than 1.
  * - A `CanPower[eager.Number]` from the "right" side of the expression.
  *
  * The unapply method allows pattern matching to operate on pairs of expressions,
  * extracting valid `(InversePower, CanPower[eager.Number])` pairs regardless of their
  * position in the tuple.
  */
object InversePowerTimesNumberCommutative extends CommutativeExtractor[InversePower, CanPower[eager.Number]] {
  protected def extractLeft(e: Expression): Option[InversePower] = e match {
    case ValueExpression(ip@InversePower(n, _), _) if n > 1 => Some(ip)
    case _ => None
  }

  protected def extractRight(e: Expression): Option[CanPower[eager.Number]] = e match {
    case ValueExpression(b: CanPower[eager.Number] @unchecked, _) => Some(b)
    case _ => None
  }
}