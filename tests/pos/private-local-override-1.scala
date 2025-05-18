sealed abstract class Tree[+A](
  final val key: A
)
final class RedTree[+A](private val _key: A) extends Tree[A](_key)
final class BlackTree[+A](private val _key: A) extends Tree[A](_key)
object RedTree {
  def unapply[A](t: RedTree[A]) = Some((t.key))
}
object BlackTree {
  def unapply[A](t: BlackTree[A]) = Some((t.key))
}