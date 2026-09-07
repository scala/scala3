class Ev[-Tup <: Tuple]

object Ev:
  given [Tup <: Tuple] => Ev[Tup] => Ev[Tuple.Tail[Tup]] = new Ev

inline def realCons(x: Any, tup: Tuple): x.type *: tup.type =
  runtime.Tuples.cons(x, tup).asInstanceOf[x.type *: tup.type]

val ops: "a" *: "b" *: EmptyTuple = ???

given Ev[ops.type] = new Ev

transparent inline private def stage(bases: Tuple)(using Ev[bases.type]): Tuple =
  inline bases match
    case _: EmptyTuple => EmptyTuple
    case _: (h *: t) =>
      realCons(compiletime.constValue[h], stage(bases.tail))

inline def materialize: Unit =
  stage(stage(ops))(using new Ev)

@main def run(): Unit =
  materialize
