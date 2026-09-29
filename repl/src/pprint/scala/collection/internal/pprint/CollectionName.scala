package scala.collection.internal.pprint

// needs to be in a scala.* package to call Iterable.collectionClassName (which is private[scala])
object CollectionName {
  def get(iterable: scala.collection.Iterable[?]): String =
    iterable.collectionClassName
}
