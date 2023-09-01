import scala.deriving.Mirror
import scala.compiletime.erasedValue

transparent inline def foo[T](using m: Mirror.Of[T]): Int =
  inline erasedValue[m.MirroredElemTypes] match
    case _: (Int *: EmptyTuple) => 1
    case _: (Int *: String *: EmptyTuple) => 2

@main def Test =
  assert(foo[MyProduct] == 2)
  assert(summon[Mirror.Of[WillGetDefault]].defaultArgument(0) == 1)
  assert(summon[Mirror.Of[WillChangeDefault]].defaultArgument(0) == 2)
