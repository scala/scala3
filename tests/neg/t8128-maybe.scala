//> using options -Yexplicit-nulls
import language.experimental.errorHandling

import compiletime.Maybe
object G {
  def unapply(m: Any): Maybe[?, Null] = Ok("")  // error

  type M[A, E] = Maybe[A, E]
  def foo: M[?, Null] = Ok("") // error
}

