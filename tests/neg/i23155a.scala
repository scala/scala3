import scala.NamedTuple
object Unpack_NT {
  (1, 2) match {
    case Unpack_NT(first, _) => first // error
  }
  def unapply(e: (Int, Int)): Some[NamedTuple.NamedTuple["x" *: "y" *: EmptyTuple, Int *: Int *: EmptyTuple]] = ???
}
