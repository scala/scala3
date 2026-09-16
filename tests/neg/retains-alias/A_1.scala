package a

import language.experimental.captureChecking

object A:
  type Catcher[+T] = PartialFunction[Throwable, T]
  class Catch[+T](val pf: Catcher[T]^)
