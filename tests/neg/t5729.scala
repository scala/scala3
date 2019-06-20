object C {
  def join(in: Seq[List[_]]): Int = 1
  def join[S](in: Seq[List[S]]): String = "x"

  val x= join(Seq[List[Int]]()) // error: ambiguous overload
  assert(x == "x")
}
