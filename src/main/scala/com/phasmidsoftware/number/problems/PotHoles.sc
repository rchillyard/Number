import scala.annotation.tailrec

val potholes: Seq[Double] = Seq(3, 10, 6, 7, 13, 21)
val crews: Seq[Double] = Seq(2, 4, 8, 12, 17, 20)

def cost(ps: Seq[Double], cs: Seq[Double]): Double = {
  @tailrec
  def inner(r: Double, w: Seq[(Double, Double)]): Double =
    w match {
      case Nil => r
      case _ =>
        val xs = w map { case (a, b) => math.abs(a - b) }
        val (z, i) = (xs zipWithIndex).sortWith((x, _) => x._1 < x._2).head
        inner(r + z, (w take i) ++ (w drop i + 1))
    }

  inner(0, ps.sorted zip cs.sorted)
}

cost(potholes, crews)
cost(Seq(1), Seq(1))
cost(Nil, Nil)
