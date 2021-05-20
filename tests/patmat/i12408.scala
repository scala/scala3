class X[T] {
  sealed trait P

  case class A(id: Long)          extends P
  case class B(id: Long, x: Long) extends P
  case class C(id: Long)          extends P

  def m(p: P): Unit = p match {
    case B(_, x) =>
    case _       =>
  }

  def n(p: P): Unit = p match {
    case B(_, x) =>
  }

  def o(p: P): Unit = p match {
    case A(_)    =>
    case B(_, x) =>
    case C(_)    =>
    case _       =>
  }

}
