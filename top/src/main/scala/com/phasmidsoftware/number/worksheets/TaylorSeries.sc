// This is an example of creating a Taylor series for a function (sine)
// about a particular a (0)
// We then evaluate the series at x = π/100 and compare with the math function value for the same x value.

//import com.phasmidsoftware.number.core.{Number, Real, Series, TaylorSeries}
//import scala.util.Try
//
//// Create Taylor Series about a = 0
//val a = Number.zeroR
//val taylorSeries: TaylorSeries = TaylorSeries.createSine(a)
//
//// Calculate the example x value (which is also the expected value of sin x)
//val x: Number = Number.pi.divide(Real(100)).asReal.get.x
//
//// Evaluate the Taylor series at x, resulting in a Series.
//val sineXseries: Series[Number] = taylorSeries.apply(x)
//
//// Evaluate the series by summing all the elements whose absolute value is > 1E-8
//val estimatedSineX: Try[Number] = sineXseries.evaluateToTolerance(1E-8)
//
//// Get the difference between x and estimatedSineX
//// We only require 25% confidence that the numbers are the same.
//val probablyTheSame = estimatedSineX.get.doSubtract(x).isProbablyZero(0.25)
//
//val mathSineX = Number.sin(x)
//// Now get the difference between estimatedSineX and math.sin(x)
//// Here, we require 90% confidence that the numbers are the same.
//val probablyTheSame2 = estimatedSineX.get.doSubtract(mathSineX).isProbablyZero(0.9)
//
