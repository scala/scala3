object test {
  case class Foo(i: Int, s: String)

  def f[A <: Tuple, B](v: A)(using m: scala.deriving.Mirror.ProductOf[B] { type MirroredElemTypes >: A }): B = m.fromTuple(v)

  f[(Any, String), Foo](("a", "b")) // error
}
