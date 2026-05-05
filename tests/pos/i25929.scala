sealed trait Ev[A, B]
case class Evidence[A]() extends Ev[Some[A], A]

sealed trait X[A]
case class Foo[A](x: A) extends X[Some[A]]

def test[A, B](m: X[A])(using ev: Ev[A, B]) = {
  ev match {
    case Evidence() =>
      m match {
        case Foo(_) =>
      }
  }
}
