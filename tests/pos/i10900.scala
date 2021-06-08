import scala.collection.IterableOps
def foo[CC[A] <: IterableOps[A, CC, CC[A]], A](collection: CC[A]) =
  collection == collection
