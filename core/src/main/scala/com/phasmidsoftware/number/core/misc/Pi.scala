package com.phasmidsoftware.number.core.misc

import com.phasmidsoftware.number.core.misc.Benchmark.Repetitions
import com.phasmidsoftware.number.core.misc.Variance.rootSumSquares
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

object Pi extends App {

  import scala.util.Random

  def getPoints(n: Int)(implicit r: Random): LazyList[(Double, Double)] = {
    def getCoordinate: Double = (r.nextDouble() - 0.5) * 2

    def radius(t: (Double, Double)): Double =
      rootSumSquares(t._1, t._2)

    val xs = LazyList.continually(getCoordinate)
    val ys = LazyList.continually(getCoordinate)
    val zs = xs zip ys
    zs take n filter (radius(_) < 1)
  }

  def calculatePi(n: Int)(implicit r: Random): Double = {
    val points: List[(Double, Double)] = getPoints(n).toList
    val result = 4.0 * points.length / n
    System.err.println(s"pi: $result")
    result
  }

  import scala.concurrent.duration.DurationInt
  import scala.language.postfixOps

  implicit val r: Random = Random
  val N = 10_000_000
  val p = 2

  val (result, milliseconds) = 1.times {
    val xfs: Seq[Future[Double]] = Seq.fill(p)(Future(calculatePi(N)))
    val xs: Seq[Double] = Await.result(Future.sequence(xfs), 100 second)
    xs.sum / xs.length
  }

  println(s"result: $result, milliseconds: $milliseconds")

}
