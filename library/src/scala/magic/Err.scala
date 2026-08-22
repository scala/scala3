package scala.magic

import language.experimental.magic
import scala.magic.runtime
import scala.magic.compiletime.Maybe
import annotation.experimental

@experimental
object Err:

  /** `inline` neded since nonbootrapped 3.9.0 compiler
   *  uses a different erasure for Maybe than boostrapped
   *  3.10.0 compiler.
   */
  inline def apply[E](e: E): Maybe[Nothing, E] =
    (if e == () then null else new runtime.Fail(e))
      .asInstanceOf[Maybe[Nothing, E]]

  def unapply[E](x: Maybe[Any, E]): Maybe[E, Nothing] = ???

