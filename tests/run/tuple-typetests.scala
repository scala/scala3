object Test extends App {

  def nonEmpty(x: Any) = x match {
    case _: (h *: t) => true
    case _: EmptyTuple => false
  }

  println(nonEmpty(Tuple()))
  println(nonEmpty(1, 2,3))
}