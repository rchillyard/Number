/*
 * Copyright (c) 2023-2025. Phasmid Software
 */

package com.phasmidsoftware.number.core.inner

import com.phasmidsoftware.number.core.inner.Modulo
import com.phasmidsoftware.number.core.misc.FP
import com.phasmidsoftware.number.core.misc.FP.*

import scala.util.*

/**
  * Represents an operation in a system. This trait serves as a base for different
  * types of operations that can be implemented. Being sealed, all implementations
  * must be defined in the same file, ensuring exhaustive pattern matching.
  *
  * Use this trait to model behaviors or actions that can be carried out, ensuring
  * type safety and enabling pattern matching on specific operations.
  */
sealed trait Operation

/**
  * Definitions of `MonadicOperation`s.
  * This module relates primarily to operations on `Value`.
  */
sealed trait MonadicOperation extends Operation {
  /**
    * Method to yield a set of functions which can be applied to Int, Rational, or Double values
    * for this MonadicOperation.
    *
    * @return a set of three functions: Int=>Try[Int], Rational=>Try[Rational], Double=>Try[Double].
    */
  val functions: MonadicFunctions

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * The general formula for a monadic operation is: x * dfByDx(x) / f(x)
    */
  val relativeFuzz: Double => Double

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int
}

/**
  * MonadicOperation to negate a Number.
  *
  * NOTE: not valid on NatLog-scaled values.
  */
case object MonadicOperationNegate extends MonadicOperation {
  /**
    * Contains monadic functions for negating numerical values of different types.
    *
    * The `functions` value is a tuple composed of three partially lifted functions:
    * - A function for negating integers, with potential exceptions handled via `Try`.
    * - A function for negating rational numbers, adhering to the Numeric type class's `negate` operation.
    * - A function for negating doubles, also using the `negate` operation from the Numeric type class.
    *
    * Each of the included functions uses the `tryF` helper to lift the respective negation operations
    * into functions that safely handle any potential runtime errors by encapsulating the result in a `Try`.
    */
  val functions: MonadicFunctions = {
    val fInt = tryF[Int, Int](math.negateExact)
    val fRational = tryF[Rational, Rational](implicitly[Numeric[Rational]].negate)
    val fDouble = tryF[Double, Double](implicitly[Numeric[Double]].negate)
    (fInt, fRational, fDouble)
  }

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double =
    _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0
}

/**
  * MonadicOperation to invert a Number.
  */
case object MonadicOperationInvert extends MonadicOperation {
  /**
    * Tuple of monadic functions used for inverting numbers of various types.
    *
    * This includes:
    * - A function to invert integers, returning `Success(1)` for input `1`, or a failure otherwise.
    * - A lifted function to invert rational numbers, wrapping the inversion operation in a `Try` context.
    * - A function to invert double precision floating-point numbers, computed as the reciprocal of the input.
    */
  val functions: MonadicFunctions = (invertInt, tryF[Rational, Rational](x => x.invert), invertDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * For any power that is not 0, the result is simply the power.
    */
  val relativeFuzz: Double => Double =
    _ => -1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0

  /**
    * Attempts to invert an integer.
    * Specifically, for the integer value `1`, it returns `Success(1)`, and for any other value, it returns a `Failure`.
    *
    * @param x the integer to be inverted
    * @return `Success(1)` if the input is `1`, or a `Failure` indicating that the inversion operation is not possible for the given value
    */
  private def invertInt(x: Int): Try[Int] = x match {
    case 1 =>
      Success(1)
    case _ =>
      Failure(OperationsException("can't invert Int"))
  }

  /**
    * A lazy implicit `Fractional[Double]` instance, used for performing fractional operations
    * on double precision floating-point numbers within the context of monadic operations.
    */
  private lazy val xf: Fractional[Double] = implicitly[Fractional[Double]]

  /**
    * Attempts to compute the inverse of a double-precision floating-point number.
    * The operation is wrapped in a `Try` to handle potential errors,
    * such as division by zero.
    *
    * @param x the double value to be inverted
    * @return a `Success` containing the inverse of the input if the operation is valid,
    *         or a `Failure` if an error occurs (e.g., division by zero)
    */
  private def invertDouble(x: Double): Try[Double] =
    Try(xf.div(xf.one, x))
}

/**
  * MonadicOperation to raise e (Euler's number) to the power of a Number.
  */
case object MonadicOperationExp extends MonadicOperation {
  /**
    * A tuple containing monadic functions for raising Euler's number (e)
    * to the power of a specific numeric type. The tuple includes the following:
    *
    * - A function for exponential computation with integers, which only allows
    * the result for `0` and fails otherwise.
    * - A function for exponential computation with rational numbers, which only
    * allows the result for negative infinity and fails otherwise.
    * - A function for exponential computation with doubles, based on the standard
    * mathematical exponential function.
    */
  val functions: MonadicFunctions = (
      expInt,
      expRat,
      expDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => x

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3

  /**
    * Computes the exponential of the given integer if the input is `0`.
    * Returns a `Success` with a value of `1` if the input is `0`, otherwise returns a `Failure`.
    *
    * @param x the integer input for which the exponential operation is attempted
    * @return a `Success` containing `1` if the input is `0`, otherwise a `Failure` indicating
    *         that the operation cannot be performed
    */
  private def expInt(x: Int): Try[Int] = x match {
    case 0 =>
      Success(1)
    case _ =>
      Failure(OperationsException("can't exp Int"))
  }

  /**
    * Function to compute the result of raising Euler's number (e) to the power of
    * a rational number. The function handles specific cases:
    *
    * - Returns `Rational.zero` (0) if the input is negative infinity.
    * - Fails for any other rational numbers and provides a relevant failure message.
    *
    * This function operates within a `Try` monad to handle potential failures.
    */
  private lazy val expRat: Rational => Try[Rational] = {
    case r if r.isInfinity && r.signum < 0 =>
      Success(Rational.zero)
    case r =>
      fail("can't do exp Rational=>Rational for non-zero parameter")(r)
  }

  /**
    * Computes the exponential of the given double value.
    *
    * @param x the double input for which the exponential operation is computed
    * @return a `Try` containing the result of the exponential computation, or a failure
    *         if the computation cannot be performed
    */
  private def expDouble(x: Double): Try[Double] =
    Try(Math.exp(x))
}

/**
  * MonadicOperation to yield the natural logarithm of a Number.
  * CONSIDER Renaming as MonadicOperationLn
  */
case object MonadicOperationLog extends MonadicOperation {
  /**
    * A tuple containing the logarithm computation functions for `Int`, `Rational`, and `Double` types.
    * These functions are used to compute the natural logarithm of the respective inputs,
    * each wrapped in a monadic structure (`Try`).
    */
  val functions: MonadicFunctions = (
      logInt,
      logRat,
      logDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double =
    x => 1 / math.log(x) // the reciprocal of the natural log of x

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3

  /**
    * Computes the natural logarithm of an integer. If the input is `1`, it returns `Success(0)`.
    * For any other input, it returns a `Failure` with a `OperationsException`.
    *
    * @param x the input integer to compute the natural logarithm for
    * @return a `Try[Int]` containing the result of the computation or an error if the input is invalid
    */
  private def logInt(x: Int): Try[Int] = x match {
    case 1 =>
      Success(0)
    case _ =>
      Failure(OperationsException("can't log Int"))
  }

  /**
    * Computes the natural logarithm of a `Rational` number wrapped in a `Try` monad.
    *
    * - Returns `Success(Rational.infinity.negate)` if the input is `Rational.zero`.
    * - Fails with an exception for other inputs, as logarithm computation for general `Rational` is undefined.
    */
  private lazy val logRat: Rational => Try[Rational] = {
    case Rational.zero =>
      Success(Rational.infinity.negate)
    case r =>
      fail("can't do log Rational=>Rational for parameter")(r)
  }

  /**
    * Computes the natural logarithm of a given double value.
    * If the input value is valid, returns a successful Try containing the natural logarithm.
    * If the input is invalid (e.g., less than or equal to zero), returns a failed Try.
    *
    * @param x the input double value for which the natural logarithm is to be computed
    * @return a Try wrapping the computed natural logarithm of the input value, or a Failure if the computation fails
    */
  private def logDouble(x: Double): Try[Double] =
    Try(Math.log(x))
}

/**
  * MonadicOperation to calculate the sine of a Number.
  * The number is a factor of pi, i.e. it is in radians.
  *
  * NOTE that this evaluator for sine is not as useful as the evaluator in the Number class.
  * That's because this type of operation does not change the factor.
  *
  * CONSIDER allow Monadic operations to change factor.
  *
  * See https://en.wikipedia.org/wiki/List_of_trigonometric_identities
  */
case object MonadicOperationSin extends MonadicOperation {

  /**
    * A collection of functions representing monadic operations for sine computation:
    * - `sinInt`: A function to compute sine for Integer values.
    * - A composite function combining `sinRatExact` for exact Rational computation
    * and `sinRatInexact` for approximate Rational computation, returning the first successful result.
    * - `sinDouble`: A function to compute sine for Double values.
    */
  val functions: MonadicFunctions = (sinInt, r => sinRatExact(r) orElse sinRatInexact(r), sinDouble)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = x => x / math.tan(x)

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3

  /**
    * A private value representing a partially lifted function that calculates the sine of an integral angle (in radians).
    *
    * This function considers that for any integral angle (in radians), the sine value is zero.
    * XXX any integral angle (in radians) results in a zero sine value.
    *
    * @return a Try[Int], where the sine value is always 0.
    */
  private lazy val sinInt: Int => Try[Int] = tryF(_ => 0)

  /**
    * A private tuple containing the BigInt values for two essential constants,
    * specifically `2` and `6`. These constants are used within the context of
    * monadic sine operation calculations.
    */
  private val (two, six) = (BigInt(2), BigInt(6))

  /**
    * NOTE that we declare this as a def so that we can invoke it for negative values.
    * Also NOTE that the modulo 4 and modulo 12 checks aren't necessary if the value has been modulated to -1 to 1.
    *
    * @param x a Rational.
    * @return a Try[Rational].
    */
  private def sinRatExact(x: Rational): Try[Rational] =
    if (x.isNegative)
      sinRatExact(x.negate).map(_.negate)
    else
      (x.n, x.d) match {
        case (n, `two`) if n.isValidInt =>
          n.toInt match {
            case t if t % 4 == 1 =>
              Success(Rational.one)
            case t if t % 4 == 3 =>
              Success(Rational.one.negate)
            case _ =>
              Failure(OperationsException("sine cannot be exact Rational"))
          }
        case (n, `six`) if n.isValidInt =>
          n.toInt match {
            case t if t % 12 == 1 || t % 12 == 5 =>
              Success(Rational.half)
            case t if t % 12 == 7 || t % 12 == 11 =>
              Success(Rational.half.negate)
            case _ =>
              Failure(OperationsException("sine cannot be exact Rational"))
          }
        case _ =>
          Failure(OperationsException("sine cannot be exact Rational"))
      }

  /**
    * A private field that defines a function to compute the sine of a `Rational` number,
    * returning a `Try[Rational]`. The function attempts to handle non-integer `Rational` values
    * by converting them to a `Double`, calculating the sine as a `Double`, and then converting
    * the result back to a `Rational` using exact representation. If the input is already a whole
    * number, the operation fails with a logic error since whole numbers are not expected input.
    *
    * This functionality is specific to the class `MonadicOperationSin` and assumes logical
    * integrity constraints within class methods for handling sine operations.
    *
    * @return a `Try[Rational]`, either the sine value as a `Rational` if successful,
    *         or a failure due to invalid conditions or calculation errors.
    */
  private val sinRatInexact: Rational => Try[Rational] = x =>
    if (!x.invert.isWhole)
      sinDouble(x.toDouble).flatMap(Rational.createExact)
    else
      Failure(OperationsException("MonadicOperationSin: logic error: whole Rational"))

  /**
    * Computes the sine of the input value multiplied by π.
    *
    * @param x a Double value representing the input for which the sine is to be calculated.
    * @return a Try[Double] containing the sine of the input, or a Throwable if an error occurs during computation.
    */
  private def sinDouble(x: Double): Try[Double] =
    Try(Math.sin(x * math.Pi))
}

/**
  * MonadicOperation to yield the arctangent of a Number.
  *
  * @param sign an Int which will distinguish between results in the four quadrants.
  */
case class MonadicOperationAtan(sign: Int) extends MonadicOperation {

  /**
    * A collection of monadic functions related to arctangent calculations.
    * This val consolidates specific implementations or handlers:
    * - A failure handler for invalid integer-based atan calls.
    * - A rational arctangent function.
    * - A double arctangent function.
    */
  val functions: MonadicFunctions =
    (fail("atan cannot be Int"), atanRat, atan)

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double =
    x => x / (1 + math.pow(x, 2)) / math.atan2(x, sign)

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 4

  /**
    * Adjusts the given angle encapsulated in a Try[Rational], potentially flipping it and normalizing its value.
    *
    * @param ry   The angle wrapped in a Try[Rational], which may be adjusted.
    * @param flip A Boolean value that determines whether the angle should be flipped (negated).
    * @return A Try[Rational] containing the modulated angle value, or a failure if the input computation fails.
    */
  def modulateAngle(ry: Try[Rational], flip: Boolean): Try[Rational] =
    ry map (r => if (flip) r.negate else r) map (r => if (sign < 0) Rational.one + r else r)

  /**
    * Defines a private function that computes the arctangent of a given Rational number
    * and adjusts the result based on the angle's quadrant.
    *
    * The process involves:
    *   1. Computing the absolute value of the input Rational and calculating its arctangent.
    *      2. Modulating the resulting angle to consider directionality (positive or negative)
    *      depending on the quadrant.
    *
    * @return A Try[Rational] containing the modulated arctangent of the input, or a Failure
    *         if the computation cannot be performed.
    */
  private lazy val atanRat: Rational => Try[Rational] =
    r => modulateAngle(doAtan(r.abs), r.signum < 0)

  /**
    * Computes the arctangent of a specified number, incorporating a directional sign for quadrant distinction.
    *
    * @param x A Double value for which the arctangent will be computed.
    * @return A Try[Double] containing the result of the calculation, or a failure if an exception occurs.
    */
  private def atan(x: Double): Try[Double] = Try {
    math.atan2(x, sign) / math.Pi
  } // CONSIDER use scale // TESTME

  /**
    * Computes the arctangent of a specified Rational value, returning a result based on certain predefined cases.
    *
    * @param r A Rational value for which the arctangent will be computed. Cases include:
    *          - Rational.infinity: Returns Success(Rational.half)
    *          - Rational.zero: Returns Success(Rational.zero)
    *          - Rational.one: Returns Success(Rational.one / 4)
    *          - Otherwise: Returns a Failure indicating that the operation cannot be performed on the given Rational
    * @return A Try[Rational] containing the computed result or a failure with a descriptive message.
    */
  private def doAtan(r: Rational) = r match {
    case Rational.infinity =>
      Success(Rational.half)
    case Rational.zero =>
      Success(Rational.zero)
    case Rational.one =>
      Success(Rational.one / 4)
    case _ =>
      Failure(OperationsException("atan cannot be Rational"))
  }
}

/**
  * This class represents a monadic operation that modulates a numeric input into a specified range
  * using modular arithmetic. It allows defining a flexible boundary behavior, which can either model
  * standard numeric ranges or angular (circular) ranges, with the ability to make the range inclusive.
  *
  * @param min                     the lower bound of the modulation range
  * @param max                     the upper bound of the modulation range
  * @param inclusive               if true, both min and max are valid; if false, max is excluded, unless boundaryBehavior specifies otherwise.
  * @param angularBoundaryBehavior when true, treats the boundary as circular (e.g., for angles),
  *                                otherwise treats it as standard numeric behavior
  */
case class MonadicOperationModulate(min: Int, max: Int, inclusive: Boolean, angularBoundaryBehavior: Boolean) extends MonadicOperation {

  private val intBoundaryBehavior = if (angularBoundaryBehavior) BoundaryBehavior.angle[Int] else BoundaryBehavior.number[Int]
  private val ratBoundaryBehavior = if (angularBoundaryBehavior) BoundaryBehavior.angle[Rational] else BoundaryBehavior.number[Rational]
  private val dblBoundaryBehavior = if (angularBoundaryBehavior) BoundaryBehavior.angle[Double] else BoundaryBehavior.number[Double]

  /**
    * Defines a set of functions for modulating a numeric input to a specified range,
    * leveraging the `modulate` method. Each function within the tuple is wrapped in
    * a `Try` for safe evaluation.
    *
    * The tuple contains:
    * - A modulation function for numeric inputs matching the provided minimum and maximum bounds as integers.
    * - A modulation function for numeric inputs with bounds converted to `Rational`.
    * - Another modulation function for numeric inputs matching the provided integer bounds.
    */
  val functions: MonadicFunctions = (
    tryF(z => modulate(z, min, max, inclusive, intBoundaryBehavior)),
    tryF(z => modulate(z, Rational(min), Rational(max), inclusive, ratBoundaryBehavior)),
    tryF(z => modulate(z, min, max, inclusive, dblBoundaryBehavior))
  )

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0

  /**
    * Modulates a value into a specified range [min, max] using modular arithmetic.
    * The result ensures the value falls within the inclusive range.
    * Optionally supports circular ranges where `min` wraps to `max`.
    *
    * @param value    the value to modulate
    * @param min      the minimum of the range (always inclusive)
    * @param max      the maximum of the range (inclusive according to `inclusive` parameter)
    * @param inclusive if true, both min and max are valid; if false, max is excluded, unless boundaryBehavior specifies otherwise.
    * @return the modulated value within the range [min, max]
    */
  def modulate[X](value: X, min: X, max: X, inclusive: Boolean, boundaryBehavior: BoundaryBehavior[X])(using Modulo[X]): X =
    Modulo.normalize(value, min, max, inclusive, boundaryBehavior)
}

/**
  * MonadicOperation to yield the square root of a Number.
  *
  * CONSIDER eliminating this and using power only.
  */
case object MonadicOperationSqrt extends MonadicOperation {
  /**
    * MonadicFunctions instance containing three functions related to square root operations:
    * - `sqrtInt`: Computes the square root of an integer, returned as a Try[Int].
    * - `sqrtRat`: Computes the square root of a rational number, returned as a Try[Rational].
    * - A partially lifted function that computes the square root of a given input using `math.sqrt`, returned as a Try[Double].
    */
  val functions: MonadicFunctions = (sqrtInt, sqrtRat, tryF(x => math.sqrt(x)))

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * For any power, the result is simply the power.
    */
  val relativeFuzz: Double => Double = _ => 0.5

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 3

  private lazy val sqrtInt: Int => Try[Int] = // CONSIDER not using squareRoots: there are other ways.
    x => toTryWithThrowable(Rational.squareRoots.get(x), OperationsException("Cannot create Int from Double"))

  private lazy val sqrtRat: Rational => Try[Rational] =
    x => FP.toTry(x.root(2), Failure(OperationsException("Cannot get exact square root")))

}

/**
  * This monadic operation is used to scale a Value by an Int.
  *
  * @param r the scale factor (a Rational).
  */
case class MonadicOperationScale(r: Rational) extends MonadicOperation {

  /**
    * A `MonadicFunctions` value containing a tuple of functions each responsible for scaling a specific numeric type
    * (`Int`, `Rational`, and `Double`) by a given scaling factor `r`.
    *
    * - `fInt`: Defined for scaling `Int` values by `r` when `r` is a whole number. If `r` is not a whole number, an error is returned.
    * - `fRational`: A function for scaling `Rational` values by the given `Rational` `r`.
    * - `fDouble`: A function for scaling `Double` values by the corresponding `Double` representation of `r`.
    */
  val functions: MonadicFunctions = {
    val fInt =
      if (r.isWhole)
        tryF[Int, Int](math.multiplyExact(_, r.toInt))
      else
        fail("can't do scale function Int=>Int")
    val fRational = tryF[Rational, Rational](_ * r)
    val fDouble = tryF[Double, Double](_ * c)
    (fInt, fRational, fDouble)
  }

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    */
  val relativeFuzz: Double => Double = _ => 1

  /**
    * Relative precision, as used by Fuzziness.createFuzz.
    */
  val fuzz: Int = 0

  private lazy val c: Double = r.toDouble
}

/**
  * MonadicOperation to apply a generic function to a Number.
  *
  */
case class MonadicOperationFunc(f: Double => Double, dfByDx: Double => Double) extends MonadicOperation {
  /**
    * A tuple of monadic functions representing different operations.
    *
    * The tuple contains:
    * - A function that always fails with the message "no apply".
    * - Another function that always fails with the message "no apply".
    * - A lifted version of the function `f`, wrapped to return a `Try` for potential failure handling.
    */
  val functions: MonadicFunctions = (fail("no apply"), fail("no apply"), tryF(f))

  /**
    * Function to yield the relative fuzz of the output Number, given the relative fuzz of the input Number.
    *
    * This is the general formula for a monadic operation. All others derive from this formula.
    */
  val relativeFuzz: Double => Double = x => x * dfByDx(x) / f(x)

  /**
    * An integer constant representing a default fuzz value.
    *
    * This value is used in the context of monadic operations
    * to represent a default or fallback fuzziness level.
    */
  val fuzz: Int = 1
}

/**
  * Trait to define a dyadic operation.
  */
sealed trait DyadicOperation extends Operation {
  /**
    * Represents the dyadic functions associated with the operation.
    */
  val functions: DyadicFunctions

  /**
    * Indicates whether the dyadic operation is performed using absolute values.
    */
  val absolute: Boolean
}

case object DyadicOperationPlus extends DyadicOperation {
  /**
    * Represents a tuple of functions for performing dyadic operations on various numeric types.
    * Each function is constructed using the `tryF` helper method to provide a safe execution context.
    *
    * The tuple contains:
    * - A function for performing an addition operation on `Int` values with overflow checks.
    * - A function for performing an addition operation on `Rational` values using their `Numeric` instance.
    * - A function for performing an addition operation on `Double` values using their `Numeric` instance.
    */
  val functions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.addExact)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].plus)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].plus)
    (fInt, fRational, fDouble)
  }

  /**
    * Indicates whether the operation is absolute.
    */
  val absolute: Boolean = true
}

/**
  * Represents the dyadic multiplication operation, where the operation is performed
  * for various numeric types (Int, Rational, Double) using predefined functions.
  *
  * This object provides the implementation of the multiplication operation
  * specific to each numeric type, with safeguards to handle exceptions and ensure
  * safety through `Try`.
  */
case object DyadicOperationTimes extends DyadicOperation {
  /**
    * Defines a tuple of dyadic functions where each function attempts to perform multiplication
    * for a specific numeric type and returns the result wrapped in a Try.
    *
    * - The first function handles multiplication for integers using safe arithmetic.
    * - The second function handles multiplication for Rational numbers using the implicit Numeric instance.
    * - The third function handles multiplication for Double values using the implicit Numeric instance.
    */
  val functions: DyadicFunctions = {
    val fInt = tryF[Int, Int, Int](math.multiplyExact)
    val fRational = tryF[Rational, Rational, Rational](implicitly[Numeric[Rational]].times)
    val fDouble = tryF[Double, Double, Double](implicitly[Numeric[Double]].times)
    (fInt, fRational, fDouble)
  }

  /**
    * Indicates whether the operation should treat values in absolute terms.
    * When set to `false`, the operation does not enforce absolute values.
    */
  val absolute: Boolean = false
}

/**
  * Represents a dyadic operation that performs power computations.
  *
  * This operation supports the power function for various numerical types including
  * integers, rational numbers, and double-precision floating-point numbers.
  */
case object DyadicOperationPower extends DyadicOperation {
  /**
    * Defines a collection of dyadic functions supporting power operations for
    * different numerical types.
    *
    * - `powerInt`: An operation that computes the power of integers.
    * - `fRational`: An operation that computes the power of rational numbers.
    * - `fDouble`: An operation that computes the power of double-precision floating-point numbers.
    *
    * Each function handles the corresponding type-specific power calculation, ensuring type safety
    * and proper handling of edge cases such as negative powers or precision limitations.
    */
  val functions: DyadicFunctions = {
    val fRational = (x: Rational, p: Rational) => x.power(p)
    val fDouble = tryF[Double, Double, Double]((x, p) => math.pow(x, p))
    (powerInt, fRational, fDouble)
  }

  /**
    * Indicates whether the operation requires absolute values for computations.
    * In this case, it is set to false, reflecting that the operation does not require
    * absolute value transformations during the power computation.
    */
  val absolute: Boolean = false

  /**
    * Computes the power of an integer raised to a non-negative integer exponent.
    *
    * The method computes the power `x∧p` where `x` is the base and `p` is the exponent.
    * If the exponent is negative, the operation fails with a `OperationsException`.
    * If the result exceeds the range of type `Int`, the operation fails with
    * a `RationalException` during precision narrowing.
    *
    * @param x the base value as an `Int`
    * @param p the exponent as an `Int`, must be non-negative
    * @return a `Success` containing the result as an `Int` if the computation succeeds,
    *         or a `Failure` with an appropriate exception if the operation fails
    */
  private def powerInt(x: Int, p: Int): Try[Int] =
    if (p >= 0)
      Rational.narrow(BigInt(x).pow(p), Int.MinValue, Int.MaxValue).map(_.toInt)
    else
      Failure(OperationsException("negative power (Int)"))
}


/**
  * Trait for the query functions: three functions, corresponding to the functions for Int, Rational and Double representations.
  */
trait QueryFunctions[T] {
  /**
    * Function that processes an integer input and returns a result of type `T` encapsulated in a `Try`.
    * This function is used for querying data by taking an integer representation.
    */
  val fInt: Int => Try[T]
  /**
    * Function to handle a query that takes a `Rational` input and returns a `Try[T]` as a result.
    */
  val fRat: Rational => Try[T]
  /**
    * Function to process a Double input and return a Try of type T.
    * Part of the query functions that operate on different numeric types.
    */
  val fDouble: Double => Try[T]
}

/**
  * Definitions of QueryOperations.
  */
sealed trait QueryOperation[T] {
  /**
    * Retrieves the `QueryFunctions` instance associated with this operation.
    * The returned `QueryFunctions` provides three functions for processing queries
    * based on different numeric input types: `Int`, `Rational`, and `Double`.
    *
    * @return the `QueryFunctions` instance containing query functions for handling
    *         operations with `Int`, `Rational`, and `Double` inputs.
    */
  def getFunctions: QueryFunctions[T]
}

/**
  * `QueryOperationIsZero` is a specific implementation of the `QueryOperation` trait
  * for operations that check whether a given numeric value is equal to zero.
  *
  * This case object provides functions for `Int`, `Rational`, and `Double` inputs,
  * determining if the numeric value is zero. The results of these functions are
  * encapsulated in a `Try` to capture potential exceptions during the operation.
  *
  * The behavior of the functions is defined as follows:
  * - For `Int`, it checks if the value is equal to 0.
  * - For `Rational`, it checks if the signum of the value is 0.
  * - For `Double`, it checks if the sign is 0 or -0, accounting for floating-point representation.
  *
  * This implementation ensures consistent behavior across different numeric types
  * while providing failure handling through the `Try` wrapper.
  */
case object QueryOperationIsZero extends QueryOperation[Boolean] {
  /**
    * Returns an instance of `BooleanQueryFunctions` containing functions that evaluate
    * whether the given numeric input is zero. These functions are encapsulated in a `Try`
    * to handle potential exceptions gracefully.
    *
    * @return `BooleanQueryFunctions` with three specific functions:
    *         - For `Int`: Checks if the value is equal to 0.
    *         - For `Rational`: Checks if the signum of the value is 0.
    *         - For `Double`: Checks if the sign is 0 or -0, accounting for floating-point representation.
    */
  def getFunctions: BooleanQueryFunctions = new QueryFunctions[Boolean] {
    val fInt: Int => Try[Boolean] = tryF[Int, Boolean](x => x == 0)
    val fRat: Rational => Try[Boolean] = tryF[Rational, Boolean](x => x.signum == 0)
    val fDouble: Double => Try[Boolean] = tryF[Double, Boolean](x => x.sign == 0 || x.sign == -0)
  }
}

/**
  * A query operation that determines whether a given value is infinite.
  * This object extends `QueryOperation[Boolean]` and provides a set of query
  * functions, implemented in `BooleanQueryFunctions`, to handle `Int`,
  * `Rational`, and `Double` input types.
  *
  * Specifically:
  * - For `Int` inputs, this operation always returns `false` since integers
  * cannot be infinite.
  * - For `Rational` inputs, it checks whether the rational number represents
  * infinity.
  * - For `Double` inputs, it checks for positive or negative infinity.
  */
case object QueryOperationIsInfinite extends QueryOperation[Boolean] {
  /**
    * Retrieves a set of query functions that operate on `Int`, `Rational`, and `Double` inputs
    * to produce a `Boolean` result, encapsulated in `BooleanQueryFunctions`.
    *
    * @return a `BooleanQueryFunctions` object containing the functions:
    *         - `fInt`: Always returns `false` for `Int` inputs, as integers cannot be infinite.
    *         - `fRat`: Checks whether a `Rational` input represents infinity.
    *         - `fDouble`: Checks if a `Double` input is positive or negative infinity.
    */
  def getFunctions: BooleanQueryFunctions = new QueryFunctions[Boolean] {
    val fInt: Int => Try[Boolean] = tryF[Int, Boolean](_ => false)
    val fRat: Rational => Try[Boolean] = tryF[Rational, Boolean](x => x.isInfinity)
    val fDouble: Double => Try[Boolean] = tryF[Double, Boolean](x => x == Double.PositiveInfinity || x == Double.NegativeInfinity)
  }
}

/**
  * `QueryOperationSignum` is a concrete implementation of `QueryOperation` for integer results.
  * It provides functionality to compute the signum of a numeric value (positive, negative, or zero) for
  * various numeric types: `Int`, `Rational`, and `Double`.
  *
  * The class defines a set of query functions encapsulated in an `IntQueryFunctions` instance.
  * These functions leverage the `math.signum` operation for `Int` and `Double`, and the
  * `.signum` method for `Rational`, with error handling provided through `Try`.
  *
  * Specifically:
  * - For `Int` inputs, it uses `math.signum` directly.
  * - For `Rational` inputs, it computes the signum using the `signum` method of `Rational`.
  * - For `Double` inputs, it uses `math.signum` and converts the result to an integer.
  */
case object QueryOperationSignum extends QueryOperation[Int] {
  /**
    * Returns a set of query functions that compute the signum of numeric values for `Int`, `Rational`,
    * and `Double` inputs, encapsulated in an `IntQueryFunctions` instance.
    *
    * These functions use the following logic:
    * - `fInt`: Computes the signum of an `Int` value using `math.signum`.
    * - `fRat`: Computes the signum of a `Rational` value using the `signum` method of `Rational`.
    * - `fDouble`: Computes the signum of a `Double` value using `math.signum` and converts the result to an `Int`.
    *
    * Each function captures the result in a `Try` to safely handle potential errors during computation.
    *
    * @return an instance of `IntQueryFunctions` containing the three signum computation functions.
    */
  def getFunctions: IntQueryFunctions = new QueryFunctions[Int] {
    val fInt: Int => Try[Int] = tryF[Int, Int](math.signum)
    val fRat: Rational => Try[Int] = tryF[Rational, Int](_.signum)
    val fDouble: Double => Try[Int] = tryF[Double, Int](math.signum(_).toInt)
  }
}

/**
  * Object providing operations to evaluate dyadic and monadic functions on values,
  * as well as query transformations.
  */
object Operations {

  /**
    * Composes two `Value` instances dyadically, using the provided set of dyadic functions
    * to determine the appropriate composition behavior based on their types.
    *
    * @param value     the first `Value` instance to compose.
    * @param other     the second `Value` instance to compose with the first.
    * @param functions a tuple of dyadic functions (`DyadicFunctions`) which include
    *                  functions for integer, rational, and double types, respectively.
    * @return an `Option[Value]` containing the result of the composition, or `None` if the operation fails.
    */
  def doComposeValueDyadic(value: Value, other: Value)(functions: DyadicFunctions): Option[Value] = {
    val (fInt, fRational, fDouble) = functions

    def tryDouble(xo: Option[Double]): Try[Value] = xo match {
      case Some(n) =>
        sequence(for (y <- Value.maybeDouble(other)) yield fDouble(n, y)) map Value.fromDouble
      case None =>
        Failure(OperationsException("number is invalid")) // NOTE this case is not observed in practice
    }

    def tryConvert[X](x: X, msg: String)(extract: Value => Option[X], func: (X, X) => Try[X], g: X => Value): Try[Value] =
      toTryWithThrowable(for (y <- extract(other)) yield func(x, y) map g, OperationsException(s"other is not a $msg")).flatten

    def tryRational(x: Rational): Try[Value] =
      tryConvert(x, "Rational")(v => Value.maybeRational(v), fRational, Value.fromRational)

    def tryInt(x: Int): Try[Value] =
      tryConvert(x, "Int")(v => Value.maybeInt(v), fInt, Value.fromInt)

    import com.phasmidsoftware.number.core.misc.Converters.*
    val xToZy1: Either[Option[Double], Rational] => Try[Value] = y => tryMap(y)(tryRational, tryDouble)

    tryMap(value)(tryInt, xToZy1).toOption
  }

  /**
    * Evaluate a monadic operator on this, using the various functions passed in.
    * The result is an Option[Value] rather than a Number, as in Number's doComposeMonadic.
    *
    * @param functions the tuple of four conversion functions.
    * @return an Option[Value] which is result of applying the appropriate function to the given value.
    */
  def doTransformValueMonadic(value: Value)(functions: MonadicFunctions): Option[Value] = {
    val (fInt, fRational, fDouble) = functions
    val xToZy0: Option[Double] => Try[Value] = {
      case Some(x) =>
        Try(Value.fromDouble(fDouble(x).toOption))
      case None =>
        Failure(new NoSuchElementException())
    }
    import com.phasmidsoftware.number.core.misc.Converters.*
    val xToZy1: Either[Option[Double], Rational] => Try[Value] = e => tryMap(e)(x => for (r <- fRational(x)) yield Value.fromRational(r), xToZy0)
    tryMap(value)(x => for (i <- fInt(x)) yield Value.fromInt(i), xToZy1).toOption
  }

  /**
    * Executes a query operation on a given `Value` using the provided `QueryFunctions`.
    * The query functions allow processing of the value based on its type (Int, Rational, or Double),
    * returning an optional result of type `T`.
    *
    * @param v         the `Value` to be queried.
    * @param functions an instance of `QueryFunctions[T]`, which provides type-specific functions
    *                  for querying integer, rational, and double values.
    * @return an `Option[T]` containing the result of applying the appropriate query function, or `None` if the query fails.
    */
  def doQuery[T](v: Value, functions: QueryFunctions[T]): Option[T] = {
    val (fInt, fRational, fDouble) = (functions.fInt, functions.fRat, functions.fDouble) // CONSIDER improve this (what's the problem?)
    val xToZy0: Option[Double] => Try[T] = {
      case Some(n) =>
        fDouble(n)
      case None =>
        Failure(new NoSuchElementException())
    }
    import com.phasmidsoftware.number.core.misc.Converters.*
    val xToZy1: Either[Option[Double], Rational] => Try[T] = y => tryMap(y)(x => fRational(x), xToZy0)
    tryMap(v)(x => fInt(x), xToZy1).toOption
  }
}

case class OperationsException(msg: String) extends Exception(msg)