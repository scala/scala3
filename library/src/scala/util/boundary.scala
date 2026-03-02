package scala.util

import language.experimental.captureChecking
import scala.annotation.implicitNotFound

/** A boundary that can be exited by `break` calls.
 *  `boundary` and `break` represent a unified and superior alternative for the
 *  `scala.util.control.NonLocalReturns` and `scala.util.control.Breaks` APIs.
 *  The main differences are:
 *
 *    - Unified names: `boundary` to establish a scope, `break` to leave it.
 *      `break` can optionally return a value.
 *    - Integration with exceptions. `break`s are logically non-fatal exceptions.
 *      The `Break` exception class extends `RuntimeException` and is optimized so
 *      that stack trace generation is suppressed.
 *    - Better performance: breaks to enclosing scopes in the same method can
 *      be rewritten to jumps.
 *
 *  Example usage:
 *
 *  ```scala
 *  import scala.util.boundary, boundary.break
 *
 *  def firstIndex[T](xs: List[T], elem: T): Int =
 *   boundary:
 *     for (x, i) <- xs.zipWithIndex do
 *       if x == elem then break(i)
 *     -1
 *  ```
 */
object boundary:

  /** User code should call `break.apply` instead of throwing this exception
   *  directly.
   *
   *  Note that it is **capability unsafe** to access `label` from a `Break`.
   *  This field will be marked private in a future release.
   */
  final class Break[T] private[boundary](val label: Label[T]^{}, val value: T)
  extends RuntimeException(
    /*message*/ null, /*cause*/ null, /*enableSuppression=*/ false, /*writableStackTrace*/ false):
    /** Compares the given [[Label]] to the one this [[Break]] was constructed with. */
    def isSameLabelAs(other: Label[T]) = label eq other

  object Break:
    import caps.unsafe.unsafeAssumePure
    def apply[T](label: Label[T], value: T) =
      // SAFETY: labels cannot leak from [[Break]], and is only used for equality comparison.
      new Break(label.unsafeAssumePure, value)

  /** Labels are targets indicating which boundary will be exited by a `break`. */
  @implicitNotFound("explain=A Label is generated from an enclosing `scala.util.boundary` call.\nMaybe that boundary is missing?")
  final class Label[-T] extends caps.Control

  /** Abort current computation and instead return `value` as the value of
   *  the enclosing `boundary` call that created `label`.
   */
  def break[T](value: T)(using label: Label[T]): Nothing =
    throw Break(label, value)

  /** Abort current computation and instead continue after the `boundary` call that
   *  created `label`.
   */
  def break()(using label: Label[Unit]): Nothing =
    throw Break(label, ())

  /** Run `body` with freshly generated label as implicit argument. Catch any
   *  breaks associated with that label and return their results instead of
   *  `body`'s result.
   */
  inline def apply[T](inline body: Label[T] ?=> T): T =
    val local = Label[T]()
    try body(using local)
    catch case ex: Break[T] @unchecked =>
      if ex.isSameLabelAs(local) then ex.value
      else throw ex

end boundary
