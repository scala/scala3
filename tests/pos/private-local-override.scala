sealed abstract class Tree[+A](
  final val key: A
)
final class RedTree[+A](key: A) extends Tree[A](key)
final class BlackTree[+A](key: A) extends Tree[A](key)
object RedTree {
  def unapply[A](t: RedTree[A]) = Some((t.key))
}
object BlackTree {
  def unapply[A](t: BlackTree[A]) = Some((t.key))
}
