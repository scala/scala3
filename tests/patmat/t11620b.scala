sealed abstract class Length

object Length {
  case class Num(n: Int) extends Length
  case object StateColumn extends Length
}

import Length.*

case class Indent[T <: Length](length: T)

def withIndent[T <: Length](indent: => Indent[?]): Unit =
  indent match {
    case Indent(Num(0)) => println("this")
    case x              => println(x) // "unreachable"
  }