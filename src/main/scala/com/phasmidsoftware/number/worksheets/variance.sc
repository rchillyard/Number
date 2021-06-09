class T[+A](a: A) {
  def get: A = a

  def +[B >: A](b: B): T[B] = new T(b)
  //  def unit(a: A): T[A] = new T(a)
}

class U[-A](a: A) {
  def unit(a: A): U[A] = new U(a)

  def +[B >: A](): U[B] = new U(a)

  override def toString: String = a.toString
}

trait F[-P, +R] {
  def apply(p: P): R
}

trait X[A] {
  def +[B >: A](b: B): X[B]

  def unit(x: A): X[A]
}

val u = new U(1).unit(2)
