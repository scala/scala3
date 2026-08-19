package scala.magic

import language.experimental.magic
import scala.magic.runtime
import annotation.experimental

@experimental
object Err:
  def apply[E](e: E): Maybe[Nothing, E] =
    (if e == () then null else new runtime.Fail(e))
      .asInstanceOf[Maybe[Nothing, E]]

  def unapply[E](x: Maybe[Any, E]): Maybe[E, Nothing] = ???

