case class One[T](fst: T)

object Test {
  def bad[T](e: One[T]) = e match {
    case foo: One[type A] =>
      val t: T = e.fst
      val nok: Nothing = t // error
  }
}
