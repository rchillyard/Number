import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

def helloWorld: Future[String] = Future {
  Thread.sleep(5000)
  java.time.LocalDateTime.now.toString
}

Await.result(Future.sequence(Seq.fill(10)(helloWorld)), 20.second)


import scala.concurrent.ExecutionContext.Implicits.global
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

def calculatePi(n: Int)(implicit r: Random): Future[Double] = Future {
  val points: List[(Double, Double)] = getPoints(n).toList
  val result = 4.0 * points.length / n
  println(s"pi: $result")
  result
}

import com.phasmidsoftware.number.misc.Benchmark._
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

implicit val r: Random = Random
val N = 1000000
val p = 2

val (result, milliseconds) = 1.times {
  val xfs: Seq[Future[Double]] = Seq.fill(p)(calculatePi(N))
  val xs: Seq[Double] = Await.result(Future.sequence(xfs), 100 second)
  xs.sum / xs.length
}

println(s"result: $result, milliseconds: $milliseconds")

