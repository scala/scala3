package scala.util

/** This object has two apply methods that abort the current computation
 *  up to an enclosing `boundary` call.
 */
object break:

  /** Abort current computation and instead return `value` as the value of
   *  the enclosing `boundary` call that created `label`.
   */
  def apply[T](value: T)(using label: boundary.Label[T]): Nothing =
    label.break(value)

  /** Abort current computation and instead continue after the `boundary` call that
   *  created `label`.
   */
  def apply()(using label: boundary.Label[Unit]): Nothing =
    label.break(())

end break
