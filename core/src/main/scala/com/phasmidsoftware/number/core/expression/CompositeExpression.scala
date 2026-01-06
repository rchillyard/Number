/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.expression

import com.phasmidsoftware.number.core.algebraic.{Algebraic, Algebraic_Quadratic, Quadratic, Solution}
import com.phasmidsoftware.number.core.expression.Expression.em.{DyadicTriple, MonadicDuple}
import com.phasmidsoftware.number.core.expression.Expression.{em, matchSimpler}
import com.phasmidsoftware.number.core.inner.*
import com.phasmidsoftware.number.core.misc.FP
import com.phasmidsoftware.number.core.numerical.{ComplexCartesian, ComplexPolar, Constants, Field, Number, Real}

import java.util.Objects
import scala.language.implicitConversions
import scala.util.Try

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
    * Method to determine if this NumberLike object is exact.
    * For instance, Number.pi is exact, although if you converted it into a PureNumber, it would no longer be exact.
    *
    * @return true if this NumberLike object is exact in the context of No factor, else false.
    */
  def isExact: Boolean =
    evaluateAsIs.exists(_.isExact)

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
          em.Match(Expression(f)) `map` (_.simplify)
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
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  def render: String =
    materialize.render

  /**
    * Method to determine what `Factor`, if there is such, this `NumberLike` object is based on.
    *
    * @return an optional `Factor`.
    */
  def maybeFactor: Option[Factor] =
    evaluateAsIs flatMap (_.maybeFactor)
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
        BiFunction(h, j, Sum)
      case _ =>
        Aggregate(Sum, xs) // TESTME
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
  def create(f: ExpressionBiFunction, xs: Field*): Expression =
    apply(f, xs map (x => Literal(x, None))) // TESTME
}

/**
  * This class represents a monadic function of the given expression.
  *
  * @param x the expression being operated on.
  * @param f the function to be applied to x.
  */
case class UniFunction(x: Expression, f: ExpressionMonoFunction) extends CompositeExpression {

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
  def evaluate(context: CoreContext): Option[Field] =
    x match {
      case AtomicExpression(field) =>
        // NOTE: here we catch any exceptions that are thrown by applyExact.
        // CONSIDER: we should never throw exceptions (see e.g., ComplexPolar.apply).
        FP.toOption(Try(f.applyExact(field))).flatten
      case _ =>
        x.evaluate(context) map f
    }
  // NOTE that the equivalent method for BiFunction is as follows
//  context.qualifyingField(f.evaluate(a, b)(context))


  /**
    * Provides an approximation of the result of applying the function `f` to the
    * `approximation` of the expression `x`, if one exists.
    *
    * @return an `Option` containing the approximated result as a `Real` if the
    *         approximation is successfully computed; otherwise, `None`.
    */
  def approximation: Option[Real] =
    x.approximation map f match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None // TESTME
    }

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
        val matcher = matchSimpler `map` (z => copy(x = z))
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
        em.Match(BiFunction(ConstE, x, Log))
      case UniFunction(BiFunction(x, b, Log), Reciprocal) =>
        em.Match(BiFunction(b, x, Log))
      // XXX we check for certain exact literal function results
      case UniFunction(e: FieldExpression, f) if e.monadicFunction(f).isDefined =>
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
}

/**
  * This class represents a dyadic function of the two given expressions.
  *
  * @param a the first expression being operated on.
  * @param b the second expression being operated on.
  * @param f the function to be applied to a and b.
  */
case class BiFunction(a: Expression, b: Expression, f: ExpressionBiFunction) extends CompositeExpression {

  /**
    * Simplifies the components of a `BiFunction` expression by applying a matcher that reduces its
    * constituent expressions (`x` and `y`) to their simpler forms.
    *
    * @return An `AutoMatcher` for the `Expression` type that matches and simplifies `BiFunction` expressions.
    */
  def simplifyComponents: em.AutoMatcher[Expression] =
    em.Matcher("BiFunction: simplifyComponents") {
      case b@BiFunction(r1@QuadraticRoot(_, _), r2@QuadraticRoot(_, _), f) if r1.maybeValue.isEmpty && r2.maybeValue.isEmpty =>
        val so: Option[Solution] = f match {
          case Sum => r1.solution `add` r2.solution
//          case Product => r1.solution multiply r2.solution
          case _ => None
        }
        val eo: Option[Expression] = so map (Algebraic(_))
        em.matchIfDefined(eo)(b)


      // NOTE I'm confused by own logic here. I don't know why we need this.
      case BiFunction(x, y, f) =>
        val matcher: em.Matcher[Seq[Expression], BiFunction] =
          em.sequence(matchSimpler) & em.lift { xs => val Seq(newX, newY) = xs; BiFunction(newX, newY, f) }
        // NOTE this is almost always a Miss.
        matcher.apply(List[Expression](x, y))
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
    case BiFunction(a, b, Product) if a == b =>
      em.Match(BiFunction(a, Two, Power))
    case BiFunction(r: Root, x, f) =>
      matchRoot(r, x, f)
    case BiFunction(l: Literal, q: QuadraticRoot, Sum) =>
      matchLiteral(l, q, Sum)
    case BiFunction(l1: Literal, l2: Literal, f) =>
      matchLiteral(l1, l2, f)
    case BiFunction(x, r: Root, f) if f.commutes =>
      r.evaluateAsIs match {
        case Some(y: Algebraic_Quadratic) =>
          modifyAlgebraicQuadratic(y, x, f)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for Root,  and $f", this) // TESTME
      }
    case BiFunction(Literal(a: Algebraic_Quadratic, _), x, f) =>
      modifyAlgebraicQuadratic(a, x, f)
    case BiFunction(x, Literal(a: Algebraic_Quadratic, _), f) if f.commutes =>
      modifyAlgebraicQuadratic(a, x, f)
    // NOTE these first two cases are kind of strange! CONSIDER removing them.
    case BiFunction(a, UniFunction(b, Negate), Product) if a == b =>
      // NOTE: duplicate code
      val xSq = Expression.simplifyConstant(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power))   // x²
      em.Match(UniFunction(xSq, Negate))
    case BiFunction(UniFunction(a, Negate), b, Product) if a == b =>  // TESTME
      val xSq = Expression.simplifyConstant(BiFunction(a, Two, Power)).getOrElse(BiFunction(a, Two, Power))
      em.Match(UniFunction(xSq, Negate))
    // NOTE this case is definitely required
    case b@BiFunction(_, _, _) =>
      ((em.complementaryTermsEliminatorBiFunction |
          em.matchBiFunctionAsAggregate & em.literalsCombiner) &
          em.alt(matchSimpler))(b)
    case b =>
      em.Miss("simplifyComposite", b) // TESTME
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
  def evaluate(context: CoreContext): Option[Field] =
    context.qualifyingField(f.evaluate(a, b)(context))

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
    * Computes the approximation of the `BiFunction` if approximations for both components (`a` and `b`) exist,
    * and applies the function `f` to these approximations. If the resulting value is a valid `Real`, it is returned.
    * Otherwise, `None` is returned.
    *
    * @return an `Option[Real]` representing the computed approximation if possible; otherwise, `None`.
    */
  def approximation: Option[Real] =
    (for (x <- a.approximation; y <- b.approximation) yield f(x, y)) match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None // TESTME
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
    * @param l the literal expression used as the matching base
    * @param x the expression to match against the literal
    * @param f the binary function dictating the transformation or operation between the literal and the expression
    * @return a MatchResult of type Expression indicating success (Match with the resulting expression)
    *         or failure (Miss with additional debug information)
    */
  private def matchLiteral(l: Literal, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] = (l, x, f) match {
    case (Literal(a@Algebraic_Quadratic(_, _, _), _), q@QuadraticRoot(_, _), Sum) =>
      em.Match(Literal(a `add` q.algebraic))
    case (Literal(Algebraic_Quadratic(_, e1, b1), _), Literal(Algebraic_Quadratic(_, e2, b2), _), f) if e1 == e2 =>
      f match {
        case Sum if b1 != b2 =>
          em.Match(e1.conjugateSum)
        case Product if b1 != b2 =>
          em.Match(e1.conjugateProduct)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for Algebraics and $f", this) // TESTME
      }
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
    case (r, p, Power) =>
      p.evaluate(RestrictedContext(PureNumber)) flatMap (_.toRational) match {
        case Some(n) =>
          em.Match(r.power(n))
        case None =>
          em.Miss("BiFunction:matchRoot Power", BiFunction(r, p, Power))
      }
    case (q1@QuadraticRoot(e1, b1), q2@QuadraticRoot(e2, b2), f) if e1 == e2 =>
      val quadratic: Quadratic = e1.asInstanceOf[Quadratic]
      f match {
        case Sum if b1 != b2 =>
          em.Match(quadratic.conjugateSum)
        case Product if b1 != b2 =>
          em.Match(quadratic.conjugateProduct)
        case _ =>
          em.Miss[Expression, Expression](s"BiFunction: matchRoot: QuadraticRoots and $f", BiFunction(q1, q2, f)) // TESTME
      }
    case (q1@QuadraticRoot(_, _), q2@QuadraticRoot(_, _), Sum) =>
      val maybeRoot = q1 `add` q2
      em.matchIfDefined(maybeRoot)(BiFunction(q1, q2, f))
    case (q@QuadraticRoot(_, _), Literal(a@Algebraic_Quadratic(_, _, _), _), Sum) =>
      em.Match(Literal(a `add` q.algebraic))
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
        em.Match(BiFunction(a, Two, Product))
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
        em.Match(UniFunction(a, Negate))
      case (MinusOne, b) =>
        em.Match(UniFunction(b, Negate))
      case (a, b) if a == b =>
        em.Match(BiFunction(a, Two, Power))
      case (BiFunction(w, x, Power), BiFunction(y, z, Power)) if w == y =>
        em.Match(BiFunction(w, x `plus` z, Power))
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
        em.Match(UniFunction(a, Reciprocal))
      case (BiFunction(x, y, Power), z) =>
        em.Match(BiFunction(x, y * z, Power))
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
        em.Match(UniFunction(a, Ln))
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
    *             the method will fallback to the left identity.
    * @return an `Option[Boolean]` indicating whether the `Expression` matches the identity:
    *         - `Some(true)` if the match is successful.
    *         - `Some(false)` if the match fails or if the `Expression` is not atomic.
    *         - `None` if no identity element exists in the provided `ExpressionBiFunction`.
    */
  private def matchingIdentity(exp: Expression, f: ExpressionBiFunction, left: Boolean): Option[Boolean] =
    exp match {
      case expression: AtomicExpression =>
        for {
          identity <- if (left) f.maybeIdentityL else f.maybeIdentityR orElse f.maybeIdentityL
          field <- expression.evaluateAsIs
        } yield field == identity
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
                em.Match(r.power(x))
              case (_, _) =>
                em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $r $f $x (not Rational)", this) // TESTME
            }
          case Some(y: Algebraic_Quadratic) if f.commutes =>
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
  private def modifyAlgebraicQuadratic(a: Algebraic_Quadratic, x: Expression, f: ExpressionBiFunction): em.MatchResult[Expression] =
    x match {
      case expr: AtomicExpression =>
        expr.evaluateAsIs match {
          case Some(y: Real) if y.isExact =>
            (y.x.toNominalRational, f) match {
              case (Some(z), Sum) =>
                em.Match(Literal(a.add(z)))
              case (Some(z), Product) =>
                em.Match(Literal(a.scale(z)))
              case (Some(r), Power) =>
                // XXX in this case, we revert this `Algebraic_Quadratic` (viz., a Field) to a `Root` (viz., an `Expression`)
                val root = Root(a.equation, a.branch).power(r)
                em.Match(root)
              case (_, _) =>
                em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Rational)", this) // TESTME
            }
          case _ =>
            em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Real)", this) // TESTME
        }
      case _ =>
        em.Miss[Expression, Expression](s"BiFunction: simplifyTrivial: no trivial simplification for $a $f $x (not Atomic)", this) // TESTME
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

  implicit def convertFromDyadicTriple(dt: DyadicTriple): BiFunction = apply(dt)
}

/**
  * Represents a composite expression that computes the total of a sequence of expressions.
  * The sequence must be non-empty.
  *
  * @constructor Constructs an Aggregate instance with a sequence of expressions.
  * @param xs A non-empty sequence of expressions to be totaled.
  */
case class Aggregate(function: ExpressionBiFunction, xs: Seq[Expression]) extends CompositeExpression {

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
          em.sequence(matchSimpler) & em.lift { ys => Aggregate(f, ys) }
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
      case Aggregate(f, xs) =>
        em.Match(Aggregate(f, xs filterNot (x => f.maybeIdentityL.contains(x))))
    }

  /**
    * Simplifies a composite `Expression` by leveraging the `simplifyAggregate` method.
    * This is typically used to reduce composite mathematical expressions into their simplest form when possible.
    *
    * @return an `AutoMatcher` for `Expression` that applies the simplification logic defined in `simplifyAggregate`.
    */
  def simplifyComposite: em.AutoMatcher[Expression] = em.Matcher("simplifyComposite") {
    case t: Aggregate =>
      em.simplifyAggregate(t)
    case x: Expression => // TESTME
      em.Miss[Expression, Expression]("Aggregate.simplifyComposite: not aggregate", x)
  }

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
  def evaluate(context: CoreContext): Option[Field] = {

    // NOTE we combine the expressions of this `Aggregate` but maintain a context which in general changes as we combine terms.
    // The initial context is determined by the parameter `context` and the function's `leftContext` method.
    // The resulting tuple of optional Field and Context is then matched.
    // If the context is impossible (for example, we multiplied a pure number by a logarithmic number such as `e`,
    // then we cannot evaluate this `Aggregate` exactly.
    xs.foldLeft[(Option[Field], CoreContext)]((function.maybeIdentityL, function.leftContext(context)))(combineExpressions) match {
      case (_, ImpossibleContext) =>
        None
      case (fo, _) =>
        fo
    }
  }

  /**
    * Adds a new expression to the current aggregate.
    *
    * @param x the expression to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def add(x: Expression): Aggregate =
    copy(xs = xs :+ x)

  /**
    * Adds a sequence of expressions to the current aggregate.
    * TESTME
    *
    * @param ys the sequence of expressions to be added
    * @return a new Aggregate instance with the updated list of expressions
    */
  def addAll(ys: Seq[Expression]): Aggregate =
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
    * Attempts to compute an approximate value by applying the aggregate function over its components.
    * If all components can be approximated successfully and the aggregate function produces a valid result,
    * the final approximation is returned. If any component fails to be approximated, the result will be `None`.
    *
    * @return an `Option[Real]` where `Some(Real)` represents the successfully computed approximation,
    *         or `None` if the approximation could not be determined.
    */
  def approximation: Option[Real] = { // TESTME
    val identity: Field = function.maybeIdentityL.getOrElse(Constants.zero) // NOTE should never require the default
    val maybeFields: Seq[Option[Field]] = xs.map(e => e.approximation)
    FP.sequence(maybeFields) map (xs => xs.foldLeft[Field](identity)(function.apply)) match {
      case Some(r: Real) =>
        Some(r)
      case _ =>
        None
    }
  }

  /**
    * Method to render this NumberLike in a presentable manner.
    *
    * @return a String
    */
  override def toString: String =
    xs.mkString(s"Aggregate{${function.toString},", ",", "}")

  /**
    * Combines an accumulator and an expression to produce a new tuple containing an optional field
    * and an updated context.
    * This is the function invoked by `foldLeft` in the `evaluate` method.
    *
    * @param accum a tuple consisting of an optional field and the current context. The field represents
    *              a computed value (if any), and the context provides additional information
    *              required for processing.
    * @param x     the expression to be combined with the accumulator. This expression is used to derive
    *              updates to the field and/or context within the tuple.
    * @return a new tuple containing an updated optional field and context after combining
    *         the accumulator and the given expression.
    */
  private def combineExpressions(accum: (Option[Field], CoreContext), x: Expression): (Option[Field], CoreContext) = {
    val (fo, context) = accum
    combineFieldsAndContexts(x, fo, context)
  }

  /**
    * Combines a given `Expression`, an optional `Field`, and a `Context` into a new tuple containing an updated
    * optional field and context. The combination logic evaluates the expression within the given context, applying
    * transformations to the field and context when applicable.
    *
    * @param x       the expression to be evaluated and combined with the field and context.
    * @param fo      an optional field representing a potential starting value or state that may be updated
    *                during the combination process.
    * @param context the current context used for evaluating the expression and determining the resulting
    *                updated context.
    * @return a tuple consisting of an updated optional field and the resulting context after processing the
    *         given expression with the provided field and context.
    */
  private def combineFieldsAndContexts(x: Expression, fo: Option[Field], context: CoreContext): (Option[Field], CoreContext) =
    (for (a <- fo; b <- x.evaluate(context)) yield {
      val field = function(a, b)
      field -> (for (factor <- field.maybeFactor) yield function.rightContext(factor)(context))
    }) match {
      case Some((f, Some(qq))) =>
        Some(f) -> qq
      case _ =>
        None -> context // TESTME
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
  def empty(function: ExpressionBiFunction): Aggregate =
    new Aggregate(function, Seq.empty)

  /**
    * Creates an instance of `Aggregate` using the given binary function and sequence of expressions.
    * Throws an `IllegalArgumentException` if the sequence of expressions is empty.
    *
    * @param function the binary function used to compose the `Aggregate`.
    * @param xs       a sequence of `Expression` objects to be aggregated.
    * @return an `Aggregate` instance composed of the binary function and the sequence of expressions.
    * @throws java.lang.IllegalArgumentException if the sequence of expressions is empty.
    */
  def create(function: ExpressionBiFunction, xs: Seq[Expression]): Aggregate =
    if (xs.nonEmpty)
      new Aggregate(function, xs)
    else
      throw new IllegalArgumentException("total requires at least one argument (use empty if necessary)") // TESTME

  /**
    * Constructs an `Aggregate` expression that applies the `Sum` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Sum` operation.
    * @return an `Expression` representing the sum of the input expressions.
    */
  def total(xs: Expression*): Aggregate =
    create(Sum, xs)

  /**
    * Constructs an `Aggregate` expression that applies the `Product` operation
    * to a variable number of input expressions.
    *
    * @param xs a sequence of `Expression` instances that will be aggregated by the `Product` operation.
    * @return an `Expression` representing the product of the input expressions.
    */
  def product(xs: Expression*): Aggregate =
    create(Product, xs)
}

