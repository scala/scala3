//> using options -Yexplicit-nulls
package scala
import language.experimental.magic
import scala.util.boundary, boundary.{Label, break}

package object magic {

  type CanErr[E] = Label[Nothing ? E]

  inline def maybe[T, E](inline body: CanErr[E] ?=> T): T ? E =
    boundary(Ok(body))

  extension [T, E](x: T ? E)
    transparent inline def ? (using CanErr[E]): T = x match
      case Ok(y) => y
      case Err(e) => break(Err(e))

    def withErr[E1](e: E1): T ? E1 = x match
      case Ok(y) => Ok(y)
      case Err(_) => Err(e)

    def mapErr[E1](f: E => E1): T ? E1 = x match
      case Ok(y) => Ok(y)
      case Err(e) => Err(f(e))

  inline def provided(inline cond: Boolean)(using CanErr[Unit]): Unit =
    if !cond then boundary.break(Err(()))

  inline def provided[E](inline cond: Boolean, inline e: E)(using CanErr[E]): Unit =
    if !cond then boundary.break(Err(e))


}

