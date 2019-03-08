object i4075 {
  case class One[T](fst: T)
  def bad[T](e: One[T]) = e match {
    case foo: One[a] =>
      val nok: Nothing = foo.fst // error
      ???
  }
}
