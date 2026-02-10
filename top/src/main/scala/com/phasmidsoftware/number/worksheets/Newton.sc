//import com.phasmidsoftware.number.algebra.eager.{Angle, Eager}
//import com.phasmidsoftware.number.core.applications.Approximation
//import com.phasmidsoftware.number.expression.expr.{E, I, Pi}
//import com.phasmidsoftware.number.core.numerical.{Number, Real}
//import com.phasmidsoftware.number.expression.expr
//import com.phasmidsoftware.number.expression.expr.Expression
//
///**
//  * This is a demonstration of Newton's method of approximation for his original polynomial:
//  * x**3 + 6 x**2 + 10 x - 1.
//  */
//val newtonsPolynomial: Double => Double = x => x * x * x + 6 * x * x + 10 * x - 1
//val newtonsDerivative: Double => Double = x => 3 * x * x + 12 * x + 10
//
//Approximation.solve(0.9, newtonsPolynomial, newtonsDerivative)(Number.zero)
//
///**
//  * This is a demonstration of Newton's method of approximation to discover the value of x such that:
//  * x = cos(x).
//  * The value should be 0.7390851332151607(27) if we use Newton,
//  * or 0.7390851332151607(12) if we use Halley.
//  *
//  * Here, we use Halley's method (which includes the second derivative).
//  */
//val cosineFunction: Double => Double = x => math.cos(x) - x
//val cosineDerivative: Double => Double = x => -math.sin(x) - 1
//val cosineSecondDerivative: Double => Double = x => -math.cos(x)
//
//Approximation.solve(0.9, cosineFunction, cosineDerivative, cosineSecondDerivative)(Number.zero)
//
///**
//  * This is a demonstration of Newton's method of approximation for the inverse square root problem.
//  */
//val x = 0.15625
//val inverseSquareRoot: Double => Double = y => y * y - 1.0 / x
//val inverseDerivative: Double => Double = y => 2 * y
//
//val result = Approximation.solve(0.9, inverseSquareRoot, inverseDerivative)(Number(2.5))
//
///**
//  * This is a demonstration of how to calculate the length of Foucault's pendulum.
//  * NOTE that we do not use the carat (^) operator here because it is not defined for Expression.
//  */
//val g = Expression(Eager(Real("9.81*")))
//val t = Expression(Eager(Real("16.5*")))
//val expression = g * ((t / Angle.twoPi) ∧ 2)
//val length: Eager = expression.materialize
//length.render
//
///**
//  * Some trigonometric identities.
//  */
//val iPi = I * Pi
//val euler: expr.Expression = E ∧ iPi
//val number = euler.render
//
//Expression(Eager.piBy2).cos.render
//Expression(Eager.piBy2).sin.render
//Expression(Eager.piBy2).tan.render