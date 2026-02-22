import com.phasmidsoftware.number.algebra.eager.Angle.𝛑
import com.phasmidsoftware.number.algebra.eager.{Eager, Real}
import com.phasmidsoftware.number.expression.expr.{Literal, Pi}
import com.phasmidsoftware.number.parse.ExpressionParser.*

import scala.util.Random

// The following are all ways of getting 𝛑 as an exact constant...
// 𝛑 as a (lazy) expression
Pi
// 𝛑 forced to be a Real (fuzzy) value.
// Note the "*" signifying "Here be dragons," (i.e., you're into the unknown)
Pi.fuzzy.render
// 𝛑 as an (eager) value
𝛑.render
// 𝛑 parsed from Latex
math"𝛑".render
// 𝛑 parsed from Latex and forced to be fuzzy
fuzzymath"𝛑".render
// 𝛑 parsed from Latex (note that the complaint about the escape character is not significant)
math"\pi".render
// 𝛑 parsed from Unicode
Eager("\uD835\uDED1").render

def getPoints(n: Int)(implicit r: Random): LazyList[(Double, Double)] = {
  def getCoordinate: Double = (r.nextDouble() - 0.5) * 2

  def sqr(x: Double) = x * x

  def radius(t: (Double, Double)): Double = scala.math.sqrt(sqr(t._1) + sqr(t._2))

  val xs = LazyList.continually(getCoordinate)
  val ys = LazyList.continually(getCoordinate)
  val zs = xs zip ys
  zs take n filter (radius(_) < 1)
}

def calculatePi(n: Int)(implicit r: Random): Double = {
  val points: List[(Double, Double)] = getPoints(n).toList
  4.0 * points.length / n
}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

implicit val r: Random = Random
val N = 10000000

val pif = for {p1 <- Future(calculatePi(N))
               p2 <- Future(calculatePi(N))
  //     p3 <- Future(calculatePi(N))
  //     p4 <- Future(calculatePi(N))
               } yield (p1 + p2) / 2 // (p1 + p2 + p3 + p4) / 4

import scala.language.postfixOps

println("Don't worry--we're waiting for 100 seconds")

Await.result(pif, 100 second)

"That's all folks"

