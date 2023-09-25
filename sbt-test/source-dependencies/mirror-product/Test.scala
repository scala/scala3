import scala.deriving.Mirror
import scala.compiletime.erasedValue

transparent inline def foo[T](using m: Mirror.Of[T]): Int =
  inline erasedValue[m.MirroredElemTypes] match
    case _: (Int *: EmptyTuple) => 1
    case _: (Int *: String *: EmptyTuple) => 2

@main def Test =
  assert(foo[MyProduct] == 2)
