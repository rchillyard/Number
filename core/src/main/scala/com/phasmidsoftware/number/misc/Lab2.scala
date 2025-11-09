package com.phasmidsoftware.number.misc

object Lab2 extends App {

  // TODO create a lazy list with the values 1/x∧2

  val xs = LazyList from 1 map (x => 1.0 / x / x)

  // TODO get a list based on xs such that we ignore anything with value < 1E-6

  val ys = xs.takeWhile(_ >= 1E-6)

  println(ys.size)

  val pi = math.sqrt(6 * ys.sum)

  val error = math.abs(pi - math.Pi) / math.Pi

  println(s"pi = $pi, with error: $error")

  val teamA = Seq(1, 4, 2, 4)
  val teamB = Seq(3, 5)

  def counts(teamA: Seq[Int], teamB: Seq[Int]): Seq[Int] = {
    val z = for (a <- teamA; (b, _) <- teamB.zipWithIndex if a <= b) yield (a, b)
    println(z)
    val m: Map[Int, Int] = z.toMap
    val q: Map[Int, Map[Int, Int]] = m.groupBy { case (_, v) => v }
    println(q)
    (for ((k, v) <- q) yield v.size).toSeq
  }

  println(counts(teamA, teamB).toList)
}
