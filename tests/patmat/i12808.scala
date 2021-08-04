import scala.reflect.Typeable

case class Err1()
case class Err2()

def handleError[A: Typeable, B: Typeable](x: Either[A | B, Nothing]): Unit =
  x match // false alarm warning: It would fail on pattern case: Left(_)
  case Left(e: A)  => println("A")
  case Left(_: B) => println("B")
  case Right(_)    => println("Nothing")
