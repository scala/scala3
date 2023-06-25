class A

class D[T >: A](v: T) {
  def getV(): T = v // ensure T is correctly inferred
}

object S {
  // J.getDS() : D[String] is inferred but not checked
  val dv: String = J.getDS().getV()
}
