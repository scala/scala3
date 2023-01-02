package scala.util
import control.ControlException

/** A boundary that can be exited by `break` calls.
 *  `boundary` and `break` represent a unified and superior alternative for the
 *  `scala.util.control.NonLocalReturns` and `scala.util.control.Breaks` APIs.
 *  The main differences are:
 *
 *    - Unified names: `boundary` to establish a scope, `break` to leave it.
 *      `break` can optionally return a value.
 *    - Integration with exceptions. `break`s are logically non-fatal exceptions.
 *      The `Break` exception class extends `ControlException` which is a regular
 *      `RuntimeException`, optimized so that stack trace generation is suppressed.
 *    - Better performance: breaks to enclosing scopes in the same method can
 *      be rwritten to jumps.
 */
object boundary:

  /** User code should call `break.apply` instead of throwing this exception
   *  directly.
   */
  class Break[T](val label: Label[T], val value: T) extends ControlException

  /** Labels are targets indicating which boundary will be exited by a `break`.
   */
  class Label[T]:
    transparent inline def break(value: T): Nothing = throw Break(this, value)

  /** Run `body` with freshly generated label as implicit argument. Catch any
   *  breaks associated with that label and return their results instead of
   *  `body`'s result.
   */
  inline def apply[T <: R, R](inline body: Label[T] ?=> R): R =
    val local = Label[T]()
    try body(using local)
    catch case ex: Break[T] @unchecked =>
      if ex.label eq local then ex.value
      else throw ex

end boundary
