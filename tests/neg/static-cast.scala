import scala.compiletime.*

trait B
enum A { case A1, A2, A3 }

private inline def singletons[T, Elem <: Tuple]: Seq[T] =
  inline erasedValue[Elem] match
    case _: EmptyTuple => Seq.empty
    case _: (h *: t) => valueOf[h].as[T] +: singletons[T, t]

val _ = singletons[B, (A.A1.type, A.A2.type, A.A3.type)] // error
