sealed abstract class Kyo[+T, -S]
opaque type <[+T, -S] >: T = T | Kyo[T, S]

abstract class Effect[+E]:
    type Command[_]

case class Recurse[Command[_], Result[_], E <: Effect[E], T, S, S2](
    h: ResultHandler[Command, Result, E, S],
    v: T < (E & S & S2)
)

abstract class ResultHandler[Command[_], Result[_], E <: Effect[E], S]:
  opaque type Handle[T, S2] >: (Result[T] < (S & S2)) = Result[T] < (S & S2) | Recurse[Command, Result, E, T, S, S2]

  def handle[T, S2](h: ResultHandler[Command, Result, E, S], v: T < (E & S & S2)): Handle[T, S2] = Recurse(h, v)
