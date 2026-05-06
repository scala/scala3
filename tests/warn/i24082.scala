//> using options -deprecation -Werror

import annotation.*

@deprecated
case object A {
  inline def use: Any = A
}

@nowarn
object test {
  A.use
}
