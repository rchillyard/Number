val directory = Map("Alice" -> 3, "Bob" -> 1, "Carol" -> 2)
val names = List("Alice", "Bob", "Carol")
val phones = List(1, 2, 3)

def matchUp(name: String, phone: Int): Option[(String, Int)] = directory.find(d => d._1 == name && d._2 == phone)

matchUp("Alice", 3)
matchUp("Robin", 2)
matchUp("Alice", 0)
matchUp("", 0)
