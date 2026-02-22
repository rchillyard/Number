import scala.util.Random
// TODO a List of Double such that each element is the value of Random.nextDouble

def xs = List.fill(10)(Random.nextDouble)

// transform your list such that each element is between -1 and 1.
// zip two such lists together

def ys = xs map (_ * 2 - 1)

val zs = ys zip ys

// TODO define method such that it takes a (Double,Double) and returns the radius
// that's to say math.sqrt(sum of the squares).

def radius(x: Double, y: Double): Double = math.sqrt(x * x + y * y)

zs map radius

