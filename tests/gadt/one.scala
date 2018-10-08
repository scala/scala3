object one {
  case class One[T](fst: T)
  def bad[T](e: One[T]) = e match {
    case foo: One[a] =>
      val t: T = e.fst
      // val nok: Nothing = t // should not compile
      val ok: a = t // does compile
  }
}
