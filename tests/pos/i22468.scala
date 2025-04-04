import Result.*
opaque type Result[+E, +A] = Success[A] | Error[E]

object Result:
  opaque type Success[+A] = A
  sealed abstract class Error[+E]

  extension [E, A](self: Result[E, A])
    inline def transform[B]: B = ???
    def problem: Boolean = transform[Boolean]
