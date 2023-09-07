package dotty.tools.dotc.util

import scala.util.boundary

/** Return type that indicates that the method returns a T or aborts to the enclosing boundary with a `None` */
type optional[T] = boundary.Label[None.type] ?=> T

/** A prompt for `Option`, which establishes a boundary which `_.?` on `Option` can return */
object optional:
  inline def apply[T](inline body: optional[T]): Option[T] =
    boundary(Some(body))

  extension [T](r: Option[T])
    inline def ? (using label: boundary.Label[None.type]): T = r match
      case Some(x) => x
      case None => boundary.break(None)

  inline def break()(using label: boundary.Label[None.type]): Nothing =
    boundary.break(None)
