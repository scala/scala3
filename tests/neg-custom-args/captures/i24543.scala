import scala.language.experimental.captureChecking

class Ref
case class Box[T](elem: T)

def h1[T <: Ref^](x: Box[T]): Unit =
  val y: (Ref^, Int) = (x.elem, 1) // error

def h2[T <: Ref^](x: List[T]): (Ref^, Int) = (x.head, 1) // error



