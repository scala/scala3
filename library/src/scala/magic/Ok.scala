package scala.magic

import scala.magic.runtime.{MaybeCase, Valid}
import scala.magic.compiletime.Maybe
import annotation.experimental

@experimental
object Ok:
  inline def apply[T](x: T): Maybe[T, Nothing] = {
    if x == null || x.isInstanceOf[MaybeCase] then new Valid(x)
    else x
  }.asInstanceOf[Maybe[T, Nothing]]

  def unapply(x: Maybe[Any, Any]): x.type = x
