package scala.magic

import scala.magic.runtime.Valid
import annotation.experimental

@experimental
object Ok:
  inline def apply[T](x: T): `$Maybe`[T] = {
    if x == null then new Valid(null)
    else if x.isInstanceOf[Valid] then new Valid(x)
    else x
  }.asInstanceOf[`$Maybe`[T]]

  def unapply(x: `$Maybe`[Any]): x.type = x
