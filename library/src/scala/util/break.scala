package scala.util

/** This object has two apply methods that abort the current computation
 *  up to an enclosing `boundary` call.
 */
object break:

  /** Abort current computation and instead return `value` as the value of
   *  the enclosing `boundary` call that created `label`.
   */
  transparent inline def apply[T](value: T)(using l: boundary.Label[T]): Nothing =
    l.break(value)

  /** Abort current computation and instead continue after the `boundary` call that
   *  created `label`.
   */
  transparent inline def apply()(using l: boundary.Label[Unit]): Nothing =
    apply(())

end break