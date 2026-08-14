package scala.magic

import scala.magic.runtime.Valid
import annotation.experimental

/** Under experimental.magic, a trait backing maybe types `T?` */
@experimental
trait `$Maybe`[+T] extends Any, Matchable:
  def isEmpty: Boolean = (this: Any) == null
  def get: T = (this: Any) match
    case x: Valid => x.elem.asInstanceOf[T]
    case x => x.asInstanceOf[T]



