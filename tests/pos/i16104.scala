trait JsonVal

val value = ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, "k", "l", "m", "n", "o", "p", "q")
val tree: JsonVal = ???

def Case2 = {
  sealed trait Write[T]
  object WriteOf:
    final inline def tuple[T <: Tuple]: Write[T] = ???

  given EntryToJson: [T] => scala.Conversion[T, JsonStructureEntry[T]] = ???
  class JsonStructureEntry[T](t: T):
    def writeAs[X >: T](using Write[X]): util.Try[JsonVal] = ???

  value
  .writeAs(using WriteOf.tuple[String *: String *: String *: String *: String *: String *: String *: String *: String *: String *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: String *: String *: String *: String *: String *: String *: String *: EmptyTuple])
  .fold(_ => ???, _ == tree)
}