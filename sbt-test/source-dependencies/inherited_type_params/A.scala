
trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}
object Test {
  implicit def TraversableEqual[CC[X] <: collection.IterableOps[X, CC, CC[X]] with Iterable[X], A: Equal]: Equal[CC[A]] = sys.error("")
}
