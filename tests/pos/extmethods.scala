trait That1[A]
class T[A, This <: That1[A]](val x: Int) extends AnyVal {
  self: This =>
  final def loop(x: This, cnt: Int): Int = loop(x, cnt + 1)
  def const[B](): Boolean = return true
}

class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
  def baz[B >: A](x: B): List[B] = ???
}

