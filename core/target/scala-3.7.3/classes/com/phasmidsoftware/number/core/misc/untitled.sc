trait Z[T]

case class ZZ[T]() extends Z[T]

val zz = ZZ()

zz

val directory = Map("Alice" -> 3, "Bob" -> 1, "Carol" -> 2)
val names = List("Alice", "Bob", "Carol")
val phones = List(1, 2, 3)

def matchUp(name: String): Option[(String, Int)] = {
  for (n <- names.get(name))

    matchUp("Alice")
}
matchUp("Robin")
matchUp("Alice")
matchUp("")