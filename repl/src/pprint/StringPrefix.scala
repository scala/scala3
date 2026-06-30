package pprint

object StringPrefix{
  def apply(i: Iterable[?]) =
    scala.collection.internal.pprint.CollectionName.get(i)
}
