package scala.magic

import scala.magic.runtime.Valid
import annotation.experimental

@experimental
object Ok:
  inline def apply[T, E](x: T): Maybe[T, E] = {
    if x == null then new Valid(null)
    else if x.isInstanceOf[Valid] then new Valid(x)
    else x
  }.asInstanceOf[Maybe[T, E]]

  def unapply(x: Maybe[Any, Any]): x.type = x
