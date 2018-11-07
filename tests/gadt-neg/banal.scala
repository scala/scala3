object banal {
  final case class Box[A](a: A)

  sealed trait T[A]
  final case class StrLit(v: String) extends T[String]
  final case class IntLit(v: Int) extends T[Int]

  def evul[A](t: T[A]): A = t match {
    case StrLit(v) => (v: Any) // error
    case IntLit(v) => (??? : Nothing)
  }

  def noeval[A](t: T[A]): Box[A] = t match {
    case StrLit(v) => Box[Any](v) // error
    case IntLit(v) => Box[Nothing](???) // error
  }
}
