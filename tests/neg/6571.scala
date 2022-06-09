object Test {
  class Inv[T]
  type M[t] = t match {
    case Inv[u] => u
  }
  def foo: M[Inv[Int] & Inv[String]] = "" // error
  def bar: M[Inv[String] & Inv[Int]] = 0 // error
}
