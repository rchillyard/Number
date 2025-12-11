import com.phasmidsoftware.number.core.Number.piBy2
import com.phasmidsoftware.number.core.applications.Approximation
import com.phasmidsoftware.number.core.expression.Expression
import com.phasmidsoftware.number.core.{Constants, Number, Real}

/**
  * This is a demonstration of Newton's method of approximation for his original polynomial:
  * x**3 + 6 x**2 + 10 x - 1.
  */
val newtonsPolynomial: Double => Double = x => x * x * x + 6 * x * x + 10 * x - 1
val newtonsDerivative: Double => Double = x => 3 * x * x + 12 * x + 10

Approximation.solve(0.9, newtonsPolynomial, newtonsDerivative)(Number.zero)

/**
  * This is a demonstration of Newton's method of approximation to discover the value of x such that:
  * x = cos(x).
  * The value should be 0.7390851332151607(27) if we use Newton,
  * or 0.7390851332151607(12) if we use Halley.
  *
  * Here, we use Halley's method (which includes the second derivative).
  */
val cosineFunction: Double => Double = x => math.cos(x) - x
val cosineDerivative: Double => Double = x => -math.sin(x) - 1
val cosineSecondDerivative: Double => Double = x => -math.cos(x)

Approximation.solve(0.9, cosineFunction, cosineDerivative, cosineSecondDerivative)(Number.zero)

/**
  * This is a demonstration of Newton's method of approximation for the inverse square root problem.
  */
val x = 0.15625
val inverseSquareRoot: Double => Double = y => y * y - 1.0 / x
val inverseDerivative: Double => Double = y => 2 * y

val result = Approximation.solve(0.9, inverseSquareRoot, inverseDerivative)(Number(2.5))

/**
  * This is a demonstration of how to calculate the length of Foucault's pendulum.
  */
val g = Expression(Real("9.81*"))
val t = Expression(Real("16.5*"))
val expression = g * ((t / Constants.twoPi) ∧ 2)
val length: Number = expression

/**
  * Some trigonometric identities.
  */
val iPi = Expression(Constants.i) * Constants.pi
val euler = Expression(Constants.e) ∧ iPi
val number = euler.asNumber
println(number)

piBy2.cos
piBy2.sin
piBy2.tan