//// Introduction.sc
//// This is an introduction to the core module of the Number library.
//// Much of the code referenced here has been superseded by the other modules.
//
//import com.phasmidsoftware.number.core.expression.{Exp, L2, Transcendental}
//import com.phasmidsoftware.number.core.inner.Rational
//import com.phasmidsoftware.number.core.numerical.Number.NumberOps
//import com.phasmidsoftware.number.core.numerical.{Constants, ExactNumber, InfiniteSeries, Number, Real}
//import com.phasmidsoftware.number.expression.expr.E
//
//import scala.util.*
//
///**
//  * This should output the following: List(0, 1, 𝛑, 𝜀, 𝛗, √2, ∞)
//  */
//val constants: Seq[String] = Seq(zero, one, pi, e, phi, root2, i, infinity) map (_.render)
//println(constants)
//
///**
//  * This demonstrates Euler's identity.
//  * [[https://en.wikipedia.org/wiki/Euler%27s_identity]]
//  * THe output should be -1.
//  */
//val negOne = E ∧ Literal(iPi)
//println(negOne.render)
//
///**
//  * This demonstrates the "Basel" problem, another famous problem solved first by Euler.
//  * [[https://en.wikipedia.org/wiki/Basel_problem]]
//  * This should output the following: 3.140637100985938±0.030%
//  * NOTE that it converges slowly so it's probably not a good idea to reduce the tolerance much more.
//  */
//val basel: InfiniteSeries[Number] = InfiniteSeries(LazyList.from(1).map(x => Rational(x).invert.square), 0.001)
//val stringsBasel = basel.render(10)
//val xy = basel.evaluateToTolerance(0.000001)
//xy match {
//  case Success(x) =>
//    val pi = (ExactNumber(6).doMultiply(x))
//    val piString = pi.render
//    println(piString)
//  case Failure(exception) =>
//    println(s"error: $exception")
//}
//
///**
//  * The following should print i½𝛑
//  */
//val x = i.ln
//println(x.render)
//
///**
//  * This should first print "ln(2)" followed by "2"
//  */
//val y = L2
//println(y.render)
//val z: Transcendental = y.function(Exp)
//println(z.render)
//z.evaluateAsIs match {
//  case Some(a) => println(a.render)
//  case None =>
//}
