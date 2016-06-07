
trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}
object Test {
  implicit def TraversableEqual[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X], A: Equal]: Equal[CC[A]] = error("")
}