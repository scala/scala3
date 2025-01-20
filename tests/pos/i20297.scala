sealed abstract class Kyo[+T, -S]
opaque type <[+T, -S] >: T = T | Kyo[T, S]

extension [T, S](v: T < S)
  inline def map[U, S2](inline f: T => U < S2): U < (S & S2) = ???

class Streams[V]
object Streams:
  def emitValue[V](v: V): Unit < Streams[V] = ???

opaque type Stream[+T, V, -S] = T < (Streams[V] & S)
object Stream:
  extension [T, V, S](s: Stream[T, V, S])
    def reemit[S2, V2](f: V => Unit < (Streams[V2] & S2)): Stream[T, V2, S & S2] = ???
    def filter[S2](f: V => Boolean < S2): Stream[T, V, S & S2] = reemit { v =>
      f(v).map {
        case false => ()
        case true  => Streams.emitValue(v)
      }
    }
