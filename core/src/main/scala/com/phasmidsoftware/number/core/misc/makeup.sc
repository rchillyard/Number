import scala.annotation.tailrec
// The following are immutable Map instances

val map1 = Map("a" -> "alpha", "b" -> "bravo", "c" -> "charlie", "d" -> "delta", "e" -> "echo", "f" -> "foxtrot", "g" -> "golf", "h" -> "hotel", "i" -> "india", "j" -> "juliet")
val map2 = Map("x" -> "xray", "y" -> "yankee", "c" -> "zulu")

// Create map3 which is a Map which includes all of map1 and, in addition, all of map2

val map3 = {
  @tailrec
  def inner(r: Seq[(String, String)], w: Seq[(String, String)]): Seq[(String, String)] =
    w match {
      case Nil => r
      case h :: t => inner(h +: r, t)
    }

  val x = inner(Nil, map1.toSeq)
  inner(x, map2.toSeq).toMap
}

// Define a method with the following signature which prints each key-value pair of a given mpa,
// one per line, ordered by key:  (a, alpha), (b, bravo), ...
// The sort methods are not available directly on Map, so you will have to
// convert the Map into a Seq and then sort according to the first element of a tuple.
// Recommendation: use sortBy

def showMap(m: Map[String, String]): Unit = m.toSeq.sortBy(_._1) foreach println

showMap(map3)

// Now, define a method which filters the elements of a given map
// such that only key-value pairs where the value contains an "e" are retained.

def keepEs(m: Map[String, String]): Map[String, String] =
  m.filter { case (_, v) => v.contains('e') }

showMap(keepEs(map3))

"hello"


// Show all your results.
