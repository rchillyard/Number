import com.phasmidsoftware.number.applications.Approximation
import com.phasmidsoftware.number.core.Number

/**
  * This is a demonstration of Newton's method of approximation for his original polynomial x**3 + 6 x**2 + 10 x - 1.
  */
val newtonsPolynomial: Double => Double = x => x * x * x + 6 * x * x + 10 * x - 1
val newtonsDerivative: Double => Double = x => 3 * x * x + 12 * x + 10

Approximation.solve(0.9, newtonsPolynomial, newtonsDerivative)(Number.zero)

/**
  * This is a demonstration of Newton's method of approximation for the inverse square root problem.
  */
val x = 0.15625
val inverseSquareRoot: Double => Double = y => y * y - 1.0/x
val inverseDerivative: Double => Double = y => 2 * y

Approximation.solve(0.9, inverseSquareRoot, inverseDerivative)(Number(2.5))
