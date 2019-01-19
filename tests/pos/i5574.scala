import scala.compiletime._

object i5574 {
  class Box[F[_]]

  inline def foo[T] <: Any =
    inline erasedValue[T] match {
      case _: Box[f] =>
        type t = f
        23
    }

  foo[Box[List]]
}
