trait T[X]
object Test {
  def join(in: Seq[T[_]]): Int = ???
  def join[S](in: Seq[T[S]]): String = ???
  join(null: Seq[T[_]]) // error: ambiguous
}

object C {
  def join(in: Seq[List[_]]): Int = sys.error("TODO")
  def join[S](in: Seq[List[S]]): String = sys.error("TODO")

  join(Seq[List[Int]]()) // error: ambiguous
  //
  // ./a.scala:13: error: ambiguous reference to overloaded definition,
  // both method join in object C of type [S](in: Seq[List[S]])String
  // and  method join in object C of type (in: Seq[List[_]])Int
  // match argument types (Seq[List[Int]])
  //   join(Seq[List[Int]]())
  //   ^
  // one error found
}
