import scala.compiletime.*

object i5574 {
  class Box[F[_]]

  transparent inline def foo[T]: Any =
    inline erasedValue[T] match {
      case _: Box[f] =>
        type t[X] = f[X]
        23
    }

  foo[Box[List]]
}
