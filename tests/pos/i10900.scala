import scala.collection.IterableOps
def foo[CC[A] <: IterableOps[A, CC, CC[A]], A](collection: CC[A]) =
  collection == collection

object Test1 {
  import scala.collection.IterableOps
  implicit class RichCollection[CC[A] <: IterableOps[A, CC, CC[A]], A](val collection: CC[A]) {
    def awm(update: CC[A] => CC[A]): CC[A] = {
      val newCollection = update(collection)
      if (newCollection == collection) collection else newCollection.awm(update)
    }
  }
}

object Test2 {
  import scala.collection.IterableOps
  implicit class RichCollection[CC[A] <: IterableOps[A, CC, CC[A]], A](val collection: CC[A]) {
    def awm(update: CC[A] => CC[A]): CC[A] = update(collection) match {
      case `collection` => collection
      case updated      => updated.awm(update)
    }
  }
}
