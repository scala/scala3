import language.experimental.captureChecking
sealed trait AbstractBox[A]
case class Box[A](x: A) extends AbstractBox[A]
def leak[A](x: Box[A^]): A =
  x match
    case Box(y) => y  // error
