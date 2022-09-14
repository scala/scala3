object Scope:
  opaque type Uses[A, B] = A ?=> B // error
  opaque type UsesAlt = [A, B] =>> A ?=> B // error

  object Uses:
    def apply[A, B](fn: A ?=> B): Uses[A, B] = fn

import Scope.*
val uses =
  given Int = 1
  Uses[Int, String](i ?=> s"*$i*")

