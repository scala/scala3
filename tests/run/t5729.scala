trait T[X]
object Test extends App {
  def join(in: Seq[T[_]]): Int = 1
  def join[S](in: Seq[T[S]]): String = "x"
  val x = join(null: Seq[T[_]])
  assert(x == 1)  // first alt chosen, since second requires a capture conversion in adapt
  C
}

object C {
  def join(in: Seq[List[_]]): Int = 1
  def join[S](in: Seq[List[S]]): String = "x"

  val x= join(Seq[List[Int]]()) // second alt chosen, since it is more specific
  assert(x == "x")
}
