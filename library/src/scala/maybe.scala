//> using options -Yexplicit-nulls
package scala
import scala.util.boundary, boundary.Label
import annotation.experimental
import compiletime.Maybe

@experimental
object maybe {

  /** The type of abort labels to be used in a maybe block */

  type CanErr[E] = Label[Maybe[Nothing, E]]

  /** Establish a maybe block with given `body` */
  inline def apply[T, E](inline body: CanErr[E] ?=> T): Maybe[T, E] =
    boundary(Ok(body))

  /** If `cond` does not hold, abort with `()` error. Typically used in a `maybe` block:
   *
   *     maybe:
   *       val y = str.toInt?
   *       provided(x >= 0)
   *       sqrt(y)
   */
  inline def provided(inline cond: Boolean)(using CanErr[Unit]): Unit =
    if !cond then boundary.break(Err(()))

  /** If `cond` does not hold, abort with given error `err`. Typically used in a `maybe` block:
   *
   *     maybe:
   *       val y = str.toInt?
   *       provided(x >= 0, "number may not be negative")
   *       sqrt(y)
   */
  inline def provided[E](inline cond: Boolean, inline err: E)(using CanErr[E]): Unit =
    if !cond then boundary.break(Err(err))
}

