import scala.deriving.Mirror
import scala.compiletime.erasedValue

object Test:
  transparent inline def foo[T](using m: Mirror.Of[T]): Int =
    inline erasedValue[m.MirroredElemLabels] match
      case _: ("Child1" *: EmptyTuple) => 1
      case _: ("Child1" *: "Child2" *: EmptyTuple) => 2

  def main(args: Array[String]): Unit =
    assert(foo[Sum] == 2)

