package scala.magic

import language.experimental.magic
import scala.magic.runtime
import annotation.experimental

@experimental
object Err:
  inline def apply[E](e: E): Maybe[Nothing, E] =
    if e == () then null
    else new runtime.Fail(e).asInstanceOf[Maybe[Nothing, E]]

  def unapply[E](x: Maybe[Any, E]): E? = x match
    case null => Ok(().asInstanceOf[E])
    case x: runtime.Fail[E] => Ok(x.elem)
    case _ => null

