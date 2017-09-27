object Bug {
  def foo[T](t1: List[T], t2: List[T]) = (t1, t2) match {
    case (Nil, Nil) => ()
    case (List(_), List(_)) => ()
  }
}

object Bug2 {
  sealed case class Foo(e: Option[Int])

  def loop(s: Foo, t: Foo): Nothing = (s,t) match {
    case (Foo(Some(_)), _) => ???
  }
}
