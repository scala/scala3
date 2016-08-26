// Currently takes a very long time (more than a minute) and then
// does not find an alternative.
object OverloadingShapeType {
  // comment out this, and the other alternative is chosen.
  def blerg(f: String): Unit = {}

  def blerg[M[X], T](l: M[T]): Unit = {}

  blerg(List(1)) // error: type mismatch; found   : List[Int] required: String
}
