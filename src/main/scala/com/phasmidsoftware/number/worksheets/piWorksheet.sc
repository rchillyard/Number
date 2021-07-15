import scala.util.Random

def getPoints(n: Int)(implicit r: Random): LazyList[(Double, Double)] = {
  def getCoordinate: Double = (r.nextDouble() - 0.5) * 2

  def sqr(x: Double) = x * x

  def radius(t: (Double, Double)): Double = math.sqrt(sqr(t._1) + sqr(t._2))

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
Await.result(pif, 100 second)

"That's all folks"

