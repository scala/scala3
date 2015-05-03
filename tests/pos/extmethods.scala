trait That1[A]
class T[A, This <: That1[A]](val x: Int) extends AnyVal {
  self: This =>
  var next: This = _
  final def loop(x: This, cnt: Int): Int = loop(x, cnt + 1)
  def const[B](): Boolean = return true
}

final class TraversableOnceOps[+A](val collection: TraversableOnce[A]) extends AnyVal {
  def reduceLeftOption[B >: A](op: (B, A) => B): Option[B] =
    if (collection.isEmpty) None else Some(collection.reduceLeft[B](op))
}

class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
  def baz[B >: A](x: B): List[B] = ???
}

