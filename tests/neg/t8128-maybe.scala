//> using options -Yexplicit-nulls
import language.experimental.errorHandling

import compiletime.Maybe
object G {
  def unapply(m: Any): Maybe[?, Unit] = Ok("")  // error

  type M[A, E] = Maybe[A, E]
  def foo: M[?, Unit] = Ok("") // error
}

