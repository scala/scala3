sealed trait X[R]
case class Foo() extends X[Nothing]
case class Bar[A]() extends X[A]

def test[A](x: X[A]) = {
  x match {
    case Foo() =>
    case Bar() =>
  }
}

def call =
  test(Foo())
