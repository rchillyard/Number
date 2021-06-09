import scala.LazyList._

val f: LazyList[BigDecimal] =
  BigDecimal(1) #:: f.map(x => 1.0 / (x * x))

LazyList.from(1) zip f take 1000 foreach println

val map1 = Map("a" -> "alpha", "b" -> "bravo", "c" -> "charlie", "d" -> "delta", "e" -> "echo", "f" -> "foxtrot", "g" -> "golf", "h" -> "hotel", "i" -> "india", "j" -> "juliet")
val map2 = Map("x" -> "xray", "y" -> "yankee", "c" -> "zulu")
def combine(a: Map[String, String], b: Map[String, String]): Map[String, String] = {
  (a /: b) {
    case (map, (k, v)) =>
      println(k + " " + v)
      map + (k -> (v + map.getOrElse(k, 0)))
    case _ =>
      println("null")
      null
  }
}
val map3 = combine(map1, map2)
