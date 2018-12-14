object Test extends App {

  def nonEmpty(x: Any) = x match {
    case _: (h *: t) => true
    case () => false
  }

  println(nonEmpty(()))
  println(nonEmpty(1, 2,3))
}