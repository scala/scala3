//> using options -Yexplicit-nulls
package scala
import scala.util.boundary, boundary.Label
import annotation.experimental
import compiletime.Maybe
import scala.util.{Ok, Err}

@experimental
object maybe {

  type CanErr[E] = Label[Maybe[Nothing, E]]

  inline def apply[T, E](inline body: CanErr[E] ?=> T): Maybe[T, E] =
    boundary(Ok(body))

  inline def provided(inline cond: Boolean)(using CanErr[Unit]): Unit =
    if !cond then boundary.break(Err(()))

  inline def provided[E](inline cond: Boolean, inline e: E)(using CanErr[E]): Unit =
    if !cond then boundary.break(Err(e))
}

