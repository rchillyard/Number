/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.expression

import com.phasmidsoftware.number.core.inner.CoreContext.{AnyLog, AnyRoot, AnyScalar}
import com.phasmidsoftware.number.core.inner._
import com.phasmidsoftware.number.core.misc.FP
import com.phasmidsoftware.number.core.numerical.{ComplexPolar, Constants, ExactNumber, Field, Number, Real}
import scala.Option.when

/**
  * Represents a named, generic computation or transformation from an input of type `P`
  * to an output value of type `Field`. The `ExpressionFunction` trait defines
  * an abstract function by extending the `(P => Field)` function type. It can be
  * used to model mathematical or computational expressions with a specific name.
  * It is the super-type of `ExpressionMonoFunction` and `ExpressionBiFunction`.
  *
  * @tparam P the type of the input parameter to the function
  */
sealed trait ExpressionFunction[P] extends (P => Field) {
  /**
    * Returns the name of the expression function.
    *
    * @return a string representing the name of the function
    */
  def name: String
}

/**
  * A lazy monadic expression function.
  *
  * TODO need to mark whether this function is exact or not (but I can't think of many which are exact).
  *
  * TODO implement also for other fields than Numbers.
  *
  * @param f    the function Number => Number.
  * @param name the name of this function.
  */
sealed abstract class ExpressionMonoFunction(val name: String, val f: Field => Field) extends ExpressionFunction[Field] {

  /**
    * Specifies the context in which the parameter of the function `f` must be evaluated.
    *
    * @param context the `Context` in which this `ExpressionMonoFunction` must be evaluated.
    * @return the `Context` to be used when evaluating the parameter.
    */
  def paramContext(context: CoreContext): CoreContext

  /**
    * Attempts to evaluate the given `Field` exactly using this `ExpressionMonoFunction`.
    * If the operation can be performed exactly, it returns the resulting `Field` wrapped
    * in an `Option`. If the operation cannot be performed exactly, it returns `None`.
    *
    * @param x the input parameter of type `Field` to be evaluated.
    * @return an `Option` containing the exact result as a `Field` if the evaluation succeeds, or `None` if it does not.
    */
  def applyExact(x: Field): Option[Field]

  /**
    * Evaluate this function on Field x.
    *
    * @param x the parameter to the function.
    * @return the result of f(x).
    */
  def apply(x: Field): Field = f(x)

  /**
    * Generate helpful debugging information about this ExpressionMonoFunction.
    *
    * @return a String.
    */
  override def toString: String = name
}

/**
  * The companion object for the ExpressionMonoFunction class.
  * Provides utility methods for working with ExpressionMonoFunction instances.
  */
object ExpressionMonoFunction {
  /**
    * Extractor method for `ExpressionMonoFunction`, enabling pattern matching.
    * TESTME ?
    *
    * @param arg the `ExpressionMonoFunction` instance from which components are extracted.
    * @return an `Option` containing a tuple of the function `Number => Number` and the name `String` of the `ExpressionMonoFunction`, or `None` if the input is null.
    */
  def unapply(arg: ExpressionMonoFunction): Option[(String, Field => Field)] =
    Some(arg.name, arg.f) // TESTME
}

/**
  * Represents a bi-functional operation on two `Field` arguments that returns a `Field` result.
  * Provides additional metadata such as the function's name, whether the function is exact,
  * and optional identity elements.
  *
  * CONSIDER changing `isExact` to a predicate based on two `Field` objects.
  *
  * @param f              the binary evaluation function to be applied to two `Field` arguments.
  * @param name           the name of the function, used for debugging and descriptive purposes.
  * @param isExact        a boolean indicating if the function is exact in all its computations.
  *                       Even if false, there may be special cases that are exact.
  * @param maybeIdentityL the optional left identity element for the function.
  *                       That's to say, `identityL f y` can be replaced by `y`.
  *                       If `None`, then the function has no identity value.
  * @param maybeIdentityR an optional right identity element for the function, if applicable.
  *                       That's to say, `x f identityR` can be replaced by `x`.
  *                       If `None`, then the function is commutative and the only identity
  *                       required is given by `identityL`.
  */
sealed abstract class ExpressionBiFunction(
                                              val name: String,
                                              val f: (Field, Field) => Field,
                                              val isExact: Boolean,
                                              val maybeIdentityL: Option[Field],
                                              val maybeIdentityR: Option[Field]
                                          ) extends ExpressionFunction[(Field, Field)] {

  /**
    * Indicates whether the binary operation represented by this instance commutes,
    * meaning the result remains unchanged for arbitrary swapping of inputs.
    * This is determined based on the absence of a right-hand identity (`maybeIdentityR`).
    */
  lazy val commutes: Boolean = maybeIdentityL.isDefined && maybeIdentityR.isEmpty

  /**
    * Applies a trivial binary function to the provided `Field` elements `a` and `b`.
    * Typically returns a default or neutral result without performing any meaningful operation.
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the trivial operation,
    *         or `None` to signify no computation or transformation.
    */
  def trivialEvaluation(a: Field, b: Field): Option[Field] = None

  /**
    * Evaluate this function on x.
    *
    * @param a the first parameter to the function.
    * @param b the second parameter to the function.
    * @return the result of f(x).
    */
  def apply(a: Field, b: Field): Field =
    f(a, b)

  /**
    * Alternative apply method which satisfies the type declaration.
    * Applies the binary function `f` to the given tuple of `Field` elements.
    *
    * @param ff a tuple containing two `Field` elements, representing the inputs to the binary function.
    * @return a `Field` that is the result of applying the binary function to the input tuple.
    */
  def apply(ff: (Field, Field)): Field =
    f.tupled(ff)

  /**
    * Defines the `Context` appropriate for evaluating the left-hand parameter of this function.
    *
    * @param context the `Context` typically based on the context for the evaluation of the whole function.
    * @return the left-hand `Context` of the binary function.
    */
  def leftContext(context: CoreContext): CoreContext

  /**
    * Defines the `Context` appropriate for evaluating the right-hand parameter of this function
    * based on the provided `Factor`.
    *
    * CONSIDER eliminating context as a parameter, or maybe changing it to the overall context
    * as used by leftContext.
    *
    * @param factor  the `Factor` used to determine the specific right-hand `Context`.
    * @param context the initial `Context` typically derived from the evaluation of the left-hand parameter.
    * @return the updated right-hand `Context` for use in further evaluations.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field]

  /**
    * Evaluates two expressions `x` and `y` in a given context and determines the resulting `Field` based on specific identity and evaluation rules.
    * Trivial identities are recognized and evaluated appropriately.
    *
    * @param x       the first expression to be evaluated.
    * @param y       the second expression to be evaluated.
    * @param context the evaluation context providing the necessary environment for resolving expressions.
    * @return an `Option[Field]` containing the result of the evaluation if successful, or `None` if evaluation fails.
    */
  def evaluate(x: Expression, y: Expression)(context: CoreContext): Option[Field] = (x.evaluateAsIs, y.evaluateAsIs) match {
    case (Some(a), _) if maybeIdentityL contains a =>
      y.evaluate(context)
    case (_, Some(b)) if maybeIdentityR contains b =>
      x.evaluate(context)
    case (Some(a), Some(b)) if trivialEvaluation(a, b).isDefined =>
      trivialEvaluation(a, b)
    case _ =>
      val xy = doEvaluate(x, y)(context)
      lazy val yx = FP.whenever(commutes)(doEvaluate(y, x)(context))
      context.qualifyingField(xy orElse yx)
  }

  /**
    * Evaluates two expressions `x` and `y` using their respective contexts, combines the evaluated results
    * through an exact binary operation, and returns the output if all operations are successful.
    *
    * This method performs a sequential evaluation where:
    * 1. The `x` expression is evaluated in the left-hand context to produce an intermediate result.
    * 2. The intermediate result is used to derive a right-hand context, in which the `y` expression is evaluated.
    * 3. If both expressions are successfully evaluated, their results are combined using a strict binary operation.
    *
    * @param x the first expression to be evaluated in the left-hand context.
    * @param y the second expression to be evaluated in the right-hand context derived from the result of `x`.
    * @return an `Option[Field]` containing the result of the exact binary operation on the evaluated results
    *         of `x` and `y`, or `None` if any step in the process fails.
    */
  private def doEvaluate(x: Expression, y: Expression)(context: CoreContext): Option[Field] =
    for {
      a <- x.evaluate(leftContext(context))
      f <- a.maybeFactor
      b <- y.evaluate(rightContext(f)(RestrictedContext(f)))
      z <- applyExact(a, b)
    } yield z

  /**
    * Evaluates two expressions as-is (without any simplification or conversion) and applies the function `f`
    * to the results if both evaluations are successful.
    *
    * @param x the first expression to be evaluated.
    * @param y the second expression to be evaluated.
    * @return an `Option[Field]` containing the result of applying the binary function to the
    *         evaluated results of `x` and `y`, or `None` if either evaluation fails.
    */
  def evaluateAsIs(x: Expression, y: Expression): Option[Field] =
    for (a <- x.evaluateAsIs; b <- y.evaluateAsIs) yield f(a, b)

  /**
    * Generate helpful debugging information about this ExpressionMonoFunction.
    *
    * @return a String.
    */
  override def toString: String = name
}

/**
  * Companion object for the `ExpressionBiFunction` class.
  *
  * Provides an extractor method to deconstruct `ExpressionBiFunction` instances
  * into their associated function and name.
  */
object ExpressionBiFunction {
  /**
    * Extracts the components of an `ExpressionBiFunction` instance.
    * TESTME ?? Not currently used.
    * CONSIDER returning other fields.
    *
    * @param f the binary function of type `((Field, Field)) => Field` to be matched and deconstructed.
    * @return an `Option` containing a tuple of the function `(Field, Field) => Field` and its associated name `String`
    *         if the input matches an `ExpressionBiFunction`, or `None` otherwise.
    */
  def unapply(f: ExpressionBiFunction): Option[((Field, Field) => Field, String, Option[Field], Option[Field])] = f match {
    case e: ExpressionBiFunction =>
      Some(e.f, e.name, e.maybeIdentityL, e.maybeIdentityR)
    case _ =>
      None // TESTME
  }
}

/**
  * Represents an arctangent operation as a binary function.
  * Calculates the angle (in radians) whose tangent is the quotient of the two provided fields.
  * If either input is not a real number, the result will be `NaN` encapsulated in a `Real`.
  *
  * - Operates on two `Field` values as the input.
  * - Uses the `atan` function to compute the angle between the two numbers.
  * - Returns a `Real` result if both inputs can be interpreted as numbers; otherwise, returns `Real(Number.NaN)`.
  *
  * Constraints:
  * - This operation is not commutative.
  * - May yield inexact results if the inputs are not exact.
  */
case object Atan extends ExpressionBiFunction("atan", Real.atan, false, None, None) {
  /**
    * Identifies and retrieves a restricted evaluation context suitable for left-hand operations.
    *
    * This method reduces the input `Context` by applying constraints, resulting in a
    * new `RestrictedContext` based on predefined evaluation criteria.
    *
    * @param context the input evaluation `Context` that defines the left-hand context for the operation.
    * @return a `RestrictedContext(PureNumber)` object that represents the constrained left-hand evaluation context.
    */
  def leftContext(context: CoreContext): CoreContext =
    RestrictedContext(PureNumber) or AnyRoot

  /**
    * Retrieves the right-hand evaluation context associated with this function.
    *
    * @param context the input evaluation `Context` that specifies the right-hand context for the operation.
    * @return the same `Context` object passed as input, representing the right-hand evaluation context.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext =
    RestrictedContext(PureNumber) or AnyRoot

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * TODO there are many cases which are unimplemented and will fail.
    * For example, other angles based pi/3.
    * TESTME
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    (a, b) match {
      case (x, Constants.zero) if x.signum > 0 =>
        Some(Real(Number.zeroR))
      case (x, Constants.zero) if x.signum < 0 =>
        Some(Constants.pi) // TESTME
      case (Constants.one, Constants.one) =>
        Some(Constants.piBy4)
      case (Constants.one, Constants.root3) =>
        Some(Constants.piBy3)
      case (Constants.zero, Constants.one) =>
        Some(Constants.piBy2)
      case (Real(ExactNumber(x, PureNumber)), Real(ExactNumber(y, PureNumber))) => // TESTME
        for {
          q <- Value.maybeRational(x)
          p <- Value.maybeRational(y)
          r = p / q
          // TODO test this--I have no idea if this is correct
          d = if (Value.signum(x) == Value.signum(y)) 1 else -1
          v <- Operations.doTransformValueMonadic(Value.fromRational(r))(MonadicOperationAtan(d).functions)
        } yield Real(ExactNumber(v, Radian))
      case _ =>
        None // TESTME
    }
}

/**
  * The `Log` object represents a mathematical logarithmic operation as a binary function.
  * It extends the `ExpressionBiFunction`, enabling evaluation of logarithmic expressions
  * with specific constraints and rules for exactness.
  *
  * This class supports restricted evaluation contexts and provides mechanisms
  * for exact computation or fallback to default behavior when exact evaluation is not possible.
  */
case object Log extends ExpressionBiFunction("log", Real.log, false, None, None) {
  /**
    * Identifies and retrieves a restricted evaluation context suitable for left-hand operations.
    *
    * This method reduces the input `Context` by applying constraints, resulting in a
    * new `RestrictedContext` based on predefined evaluation criteria.
    *
    * @param context the input evaluation `Context` that defines the left-hand context for the operation.
    * @return a `RestrictedContext(PureNumber)` object that represents the constrained left-hand evaluation context.
    */
  def leftContext(context: CoreContext): CoreContext =
    RestrictedContext(PureNumber) or AnyRoot

  /**
    * Retrieves the right-hand evaluation context associated with this function.
    *
    * @param context the input evaluation `Context` that specifies the right-hand context for the operation.
    * @return the same `Context` object passed as input, representing the right-hand evaluation context.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext =
    RestrictedContext(PureNumber)

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * TESTME
    *
    * @param a the field whose log we required, a `Field` instance.
    * @param b the base, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    (a, b) match {
      case _ if b <= Constants.one =>
        None // CONSIDER throwing an exception here instead
      case (Constants.one, _) =>
        Some(Constants.zero)
      case (Real(x@ExactNumber(_, Log2)), Constants.two) =>
        Some(Real(x.make(PureNumber)))
      case (Real(x@ExactNumber(_, Log10)), Constants.ten) =>
        Some(Real(x.make(PureNumber)))
      case (Real(x@ExactNumber(_, NatLog)), Constants.e) => // XXX not strictly necessary as this will be handled by the default case
        Some(Real(x.make(PureNumber)))
      case (Real(x@ExactNumber(_, Euler)), Constants.e) =>
        Some(ComplexPolar(Number.one, x.make(Radian).simplify))
      case _ if a == b =>
        Some(Constants.one)
      case _ =>
        Some(f(a, b))
    }
}

/**
  * Represents the natural logarithmic function as an ExpressionMonoFunction.
  *
  * Renaming this function as Ln (logarithmus naturalis).
  *
  * This object provides functionality to compute the natural logarithm (ln) of a given Number.
  * The underlying implementation utilizes the `log` method of the Number type.
  */
case object Ln extends ExpressionMonoFunction("ln", x => x.ln) {
  /**
    * Regardless of the value of `context`, the required `Context` for the parameter is `PureNumber`.
    *
    * @param context the initial `Context` to be modified or restricted.
    * @return a new `Context` object, which is a restricted version of the provided `context`.
    */
  def paramContext(context: CoreContext): CoreContext =
    AnyScalar or AnyLog // CONSIDER should we be allowing Log2 and Log10?  // TESTME

  /**
    * Applies an exact mapping transformation on the given `Field`.
    * The method matches the input `Field` to predefined constants
    * and returns the corresponding result wrapped in an `Option`.
    *
    * @param x the input `Field` to be evaluated.
    * @return an `Option` containing the resulting `Field` if the input matches a predefined constant;
    *         otherwise, `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.e =>
      Some(Constants.one)
    case Constants.one =>
      Some(Constants.zero)
    case Constants.zero =>
      Some(Constants.negInfinity)
    case Constants.minusOne =>
      Some(-ComplexPolar(Number.pi, Number.piBy2))
    case _ =>
      None
  }
}

/**
  * Represents a mathematical exponential function, exp(x), where e is the base of natural logarithms.
  * This case object extends ExpressionMonoFunction and applies the exp operation on a given number.
  * It defines the exponential operation for transformation or evaluation within expressions.
  */
case object Exp extends ExpressionMonoFunction("exp", x => x.exp) {
  /**
    * Ignores the provided `context` and returns `AnyScalar`.
    *
    * @param context ignored.
    * @return a new or modified `Context` after applying the associated operation
    */
  def paramContext(context: CoreContext): CoreContext =
    AnyScalar // TESTME

  /**
    * Computes the result of applying the exponential function to a specific `Field` value.
    * This method provides predefined results for certain input cases:
    * - Negative infinity maps to zero.
    * - Zero maps to one.
    * - One maps to the mathematical constant `e`.
    * - For all other inputs, no result is computed.
    *
    * @param x the input `Field` value on which the exact exponential operation is applied.
    * @return `Some(Field)` if the input matches a predefined case, or `None` otherwise.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.negInfinity =>
      Some(Constants.zero)
    case Constants.zero =>
      Some(Constants.one) // TESTME
    case Constants.one =>
      Some(Constants.e)
    case _ =>
      None
  }
}

/**
  * Negate is a specific implementation of the ExpressionMonoFunction that changes the sign of a numeric value.
  *
  * This object represents the mathematical negation operation ("-") applied to a numeric input.
  * It uses the `negate` method, which evaluates the negation of a given Number while handling specific cases
  * like imaginary numbers and converting non-pure factors to PureNumber form as necessary.
  *
  * The function is exact and operates lazily.
  */
case object Negate extends ExpressionMonoFunction("-", x => -x) {
  /**
    * Ignores the specified `context` and returns `AnyScalar`.
    *
    * @param context ignored.
    * @return a `Context` object derived from or related to the provided `context`.
    */
  def paramContext(context: CoreContext): CoreContext = AnyScalar // TESTME

  /**
    * Applies an exact mathematical operation to negate certain types of exact numeric fields.
    * This method specifically handles cases where the input is a `Real` containing an `ExactNumber`
    * with either a `PureNumber` or `Radian` factor.
    *
    * @param x the input field to which the exact operation is applied.
    *          Only fields matching predefined patterns are processed; others return `None`.
    * @return an `Option[Field]` containing the negated `Field` if the input matches the expected pattern,
    *         otherwise `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    // CONSIDER combining these cases by using `Scalar`
    case Real(ExactNumber(v, f@PureNumber)) =>
      Some(Real(ExactNumber(Value.negate(v), f)))
    case Real(ExactNumber(v, f@Radian)) =>
      Some(Real(ExactNumber(Value.negate(v), f)))
    case _ =>
      None
  }
}

/**
  * `Reciprocal` is an `ExpressionMonoFunction` representing the mathematical reciprocal operation.
  * The function takes a numeric input `x` and computes `1 / x`.
  * It is identified by the name "rec".
  * CONSIDER eliminating this and forcing any reciprocal to be power of negative one.
  *
  * The operation is performed lazily and adheres to the behavior defined in its parent class.
  */
case object Reciprocal extends ExpressionMonoFunction("rec", x => x.invert) {
  /**
    * Attempts to compute the reciprocal (exact inverse) of the given `Field`.
    * For specific cases of `Real` representations, such as `ExactNumber` with pure, logarithmic, or root factors,
    * the method determines the inverse or applies negation as appropriate.
    * Complex or unsupported `Field` cases are ignored, and `None` is returned.
    *
    * @param x the input `Field` to which the exact reciprocal operation should be applied.
    * @return an `Option[Field]` containing the exact reciprocal if it can be computed, otherwise `None`.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Real(ExactNumber(v, f@PureNumber)) =>
      Value.inverse(v) map (x =>
        // NOTE: experimental code. If it works well, we could use it elsewhere.
        Constants.pureConstants.getOrElse(x,
          Real(ExactNumber(x, f))))
    case Real(ExactNumber(v, f@Logarithmic(_))) =>
      Some(Real(ExactNumber(Value.negate(v), f))) // TESTME
    case Real(ExactNumber(v, f@NthRoot(_))) =>
      Value.inverse(v) map (x =>
        Real(ExactNumber(x, f)))
    case _ =>
      None
  }

  /**
    * Ignores the provided context and returns contexts that can be exactly inverted.
    *
    * @param context ignored.
    * @return a new context resulting from the combination of the `RestrictedContext` defined with `PureNumber`
    *         and the `AnyLog` condition using the logical "or" operation.
    */
  def paramContext(context: CoreContext): CoreContext =
    RestrictedContext(PureNumber) or AnyLog // TESTME
}

/**
  * Represents a sum operation as a binary function that adds two `Field` values.
  *
  * This object extends `ExpressionBiFunction` by defining its operation as addition (`add`)
  * with the corresponding symbol "+" and is flagged as not always exact (`isExact = false`).
  */
case object Sum extends ExpressionBiFunction("+", (x, y) => x add y, isExact = false, Some(Constants.zero), maybeIdentityR = None) {
  /**
    * Defines the `Context` appropriate for evaluating the left-hand parameter of this function.
    *
    * @param context the `Context` typically based on the context for the evaluation of the whole function.
    * @return the left-hand `Context` of the binary function.
    */
  def leftContext(context: CoreContext): CoreContext =
    context

  /**
    * Retrieves the right-hand evaluation `Context` appropriate for this function.
    *
    * @param context the `Context` typically based on the value of the left-hand parameter.
    * @return the right-hand `Context` of the binary function.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext =
    context

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    * In this case, the evaluation succeeds only if the factors of each parameter are compatible.
    * Otherwise, the result is `None`. TODO CHECK
    *
    * @param a the first operand, an exact `Field` instance.
    * @param b the second operand, an exact `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Some(a add b)
}

/**
  * Represents a specific implementation of the `ExpressionBiFunction` that performs multiplication between two `Field` values.
  *
  * This object embodies a binary operation where the function takes two inputs and computes their product using the `multiply` method
  * defined on `Field`. The operation is represented by the symbol "*".
  *
  * - The operation is marked as exact, ensuring the result is always precise when the inputs are exact.
  * - It inherits the commutative property from `ExpressionBiFunction`, as multiplication is commutative.
  */
case object Product extends ExpressionBiFunction("*", (x, y) => x multiply y, isExact = true, Some(Constants.one), maybeIdentityR = None) {
  /**
    * Evaluates two `Field` instances under certain trivial conditions and determines the result.
    *
    * This method returns `Some(Constants.zero)` if either of the input `Field` instances is
    * equal to `Constants.zero`. Otherwise, it returns `None`.
    *
    * @param a the first `Field` instance to evaluate.
    * @param b the second `Field` instance to evaluate.
    * @return an `Option[Field]` containing `Constants.zero` if trivial conditions are met;
    *         otherwise, `None`.
    */
  override def trivialEvaluation(a: Field, b: Field): Option[Field] = (a, b) match {
    case (Constants.zero, _) | (_, Constants.zero) =>
      Some(Constants.zero)
    case _ =>
      None
  }

  /**
    * Retrieves the left-hand evaluation context associated with this function.
    *
    * @return None.
    */
  def leftContext(context: CoreContext): CoreContext = context

  /**
    * Determines the right-hand evaluation context for a given `Context`.
    *
    * The method evaluates the provided context and returns a new `Context` that is derived
    * based on the specific rules for different types of contexts.
    *
    * CONSIDER reworking this method to take a Factor (from the left-hand parameter) and a Context (the overall context).
    * Otherwise, I don't think it's going to work properly.
    *
    * @param context the initial `Context` to be evaluated and transformed.
    * @return the resulting `Context` after applying the transformation logic.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext = context match {
    case AnyScalar | AnyContext =>
      context or RestrictedContext(PureNumber) // TESTME
    case AnyLog =>
      context // TESTME
    case AnyRoot =>
      context or RestrictedContext(PureNumber) // TESTME
    case r@RestrictedContext(SquareRoot) =>
      r or RestrictedContext(PureNumber)
    case r@RestrictedContext(Radian) =>
      r or RestrictedContext(PureNumber)
    case r@RestrictedContext(_) =>
      r
    case _ =>
      ImpossibleContext // TESTME
  }

  /**
    * Multiplies two Field instances under specific conditions and returns the result as an optional Field.
    *
    * The method checks the type and characteristics of the second operand `b` (the multiplier)
    * and applies an exact mathematical operation to the first operand `a` (the multiplicand)
    * if certain criteria are met. If no conditions are satisfied, it returns `None`.
    *
    * The final check on `isExact` should be redundant, but it's here to be safe.
    *
    * @param a the first operand, a Field instance serving as the multiplicand.
    * @param b the second operand, a Field instance serving as the multiplier. This operand is evaluated
    *          to determine the applicability of exact computations.
    * @return an `Option[Field]` containing the resulting Field if the operation is valid and applicable,
    *         or `None` if the conditions for exact multiplication are not met.
    */
  def applyExact(a: Field, b: Field): Option[Field] =
    Option.when(a.isExact && b.isExact)(a multiply b) filter (_.isExact)
}

/**
  * Represents the power operation as a binary function within expressions.
  * This operation raises the first operand to the power of the second operand.
  * It is not exact for all inputs and does not commute.
  *
  * TODO check on Constants.zero as identityL.
  *
  * Extends `ExpressionBiFunction` where the specific function is implemented
  * using the `power` method from the `Field` class.
  */
case object Power extends ExpressionBiFunction("∧", (x, y) => x.power(y), isExact = false, None, Some(Constants.one)) {
  /**
    * Evaluates two `Field` instances and determines a trivial result based on predefined conditions.
    * Specifically, it checks if the first `Field` instance is equivalent to the constant `zero`.
    *
    * @param a the first operand, a `Field` instance, which is evaluated to see if it's `zero`.
    * @param b the second operand, a `Field` instance, which is ignored in this implementation.
    * @return an `Option[Field]`, where `Some(a)` is returned if `a` is `zero`; otherwise, `None`.
    */
  override def trivialEvaluation(a: Field, b: Field): Option[Field] = a match {
    case Constants.zero =>
      Some(a) // TESTME
    case _ =>
      None
  }

  /**
    * Determines the left-hand evaluation context for a given input context.
    * This method evaluates the provided context to compute the appropriate left
    * evaluation context based on specific rules or conditions.
    *
    * @param context the input `Context` to evaluate.
    * @return None.
    */
  def leftContext(context: CoreContext): CoreContext = context

  /**
    * Determines the right-hand evaluation context for a given input context.
    * This method ignores the provided context and determines whether the `PureNumber` context
    * should be included or excluded based on the evaluation conditions.
    *
    * @param context the input context to evaluate.
    * @return the updated `Context` after applying the evaluation logic. Returns `None` if the input
    *         context matches `Some(PureNumber)`, otherwise returns `Some(PureNumber)`.
    */
  def rightContext(factor: Factor)(context: CoreContext): CoreContext =
    AnyScalar // ignore both parameters

  /**
    * Applies a binary operation to the provided `Field` elements `a` and `b`, with stricter evaluation rules,
    * and returns an optional result.
    * The evaluation succeeds only if the operation satisfies specific conditions
    * (e.g., exact representations or mathematical constraints).
    *
    * @param a the first operand, a `Field` instance.
    * @param b the second operand, a `Field` instance.
    * @return an `Option[Field]` containing the result of the operation if it can be computed exactly,
    *         or `None` if the operation fails to meet exactness requirements.
    */
  def applyExact(a: Field, b: Field): Option[Field] = (a, b) match {
    case (Real(x@ExactNumber(_, _)), Real(y@ExactNumber(_, PureNumber))) =>
      val result: Number = x.doPower(y)
      when(result.isExact)(Real(result))
    case _ =>
      None // TESTME
  }
}

/**
  * Represents a trigonometric function, either sine or cosine, that is derived from the abstract `ExpressionMonoFunction`.
  *
  * The `SineCos` class applies a specified trigonometric function (`sin` or `cos`) to an `Expression`.
  * The function to be applied is determined by the given `sine` boolean parameter.
  *
  * If `sine` is true, the sine (`sin`) function is applied, otherwise, the cosine (`cos`) function is applied.
  *
  * @param sine a boolean indicating whether the sine function should be used (`true` for sine, `false` for cosine).
  */
abstract class SineCos(sine: Boolean) extends ExpressionMonoFunction(if (sine) "sin" else "cos", x => if (sine) x.sin else x.cos) {
  /**
    * Regardless of the value of `context`, the required `Context` for the parameter is `Radian`.
    *
    * @param context ignored.
    * @return a new `RestrictedContext` instance configured with the `Radian` factor.
    */
  def paramContext(context: CoreContext): CoreContext =
    RestrictedContext(Radian) // TESTME

  /**
    * Applies the sine or cosine function to a given `Field` value if the value matches specific constants.
    * Returns an exact result for known trigonometric values of zero, π/2, π, and 3π/2.
    *
    * @param x the input `Field` value to be evaluated.
    * @return an `Option[Field]` containing the result of the sine or cosine function if the input matches a known constant,
    *         or `None` if the input does not match any predefined constants.
    */
  def applyExact(x: Field): Option[Field] = x match {
    case Constants.zero =>
      Some(if (sine) Constants.zero else Constants.one)
    case Constants.piBy2 =>
      Some(if (sine) Constants.one else Constants.zero)
    case Constants.pi =>
      Some(if (sine) Constants.zero else -Constants.one)
    case Constants.piBy2Times3 =>
      Some(if (sine) -Constants.one else Constants.zero) // TESTME
    case _ =>
      None // TESTME
  }
}

/**
  * Represents the sine trigonometric function.
  *
  * `Sine` is a case object of the `SineCos` abstract class with the `sine` parameter set to `true`,
  * which designates that the sine (`sin`) function is to be applied to expressions.
  *
  * It applies the sine function to a given mathematical expression and inherits all behavior
  * defined in the `SineCos` abstract class.
  */
case object Sine extends SineCos(true)

/**
  * Represents the cosine trigonometric function.
  *
  * `Cosine` is a specific instance of the `SineCos` class with `sine` set to `false`,
  * meaning it applies the cosine (`cos`) function to an `Expression`.
  */
case object Cosine extends SineCos(false)

