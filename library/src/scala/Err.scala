package scala

import compiletime.Maybe
import annotation.experimental

@experimental
object Err:

  // `inline` needed since nonbootrapped 3.9.0 compiler
  //  uses a different erasure for Maybe than boostrapped
  //  3.10.0 compiler. `inline` is also benefical since it can
  //  remove the condition and the `if` when the argument is known.
  inline def apply[E](e: E): Maybe[Nothing, E] =
    (if e == () then null else new runtime.Fail(e))
      .asInstanceOf[Maybe[Nothing, E]]

  /** Wll be replaced by compiler with specialized code */
  def unapply[E](x: Maybe[Any, E]): Maybe[E, Nothing] = x.runtimeChecked match
    case Err(e) => Ok(e)


