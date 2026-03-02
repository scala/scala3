import scala.compiletime.{erasedValue, summonFrom}

object test {
  private inline def singletons[T, Elem <: Tuple]: Seq[T] =
    inline erasedValue[Elem] match {
      case _: EmptyTuple => Seq.empty
      case _: (h *: t) => valueOf[`h` & T] +: singletons[T, t]
    }

  enum A { case A1, A2, A3 }

  val _ = singletons[A, (A.A1.type, A.A2.type, A.A3.type)] // error
}
