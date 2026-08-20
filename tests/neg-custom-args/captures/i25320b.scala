import language.experimental.captureChecking
case class Foo()
case class Box[A](x: A)
def fooLeak(x: Box[Foo^]): Box[Foo] = {
  x match {
    case Box(y) => Box(y)  // error
  }
}
