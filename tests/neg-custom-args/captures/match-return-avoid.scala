import language.experimental.captureChecking

case class Foo()
case class Box[A](x: A)

def f(x: Foo^): Box[Foo^{}] =
  x match
    case y => Box(y) // error
