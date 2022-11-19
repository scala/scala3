package dotty.tools
package dotc
package sbt

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import xsbti.api

/** Create and hold thunks. A thunk is a (potentially) unevaluated value
 *  that may be evaluated once.
 */
private[sbt] trait ThunkHolder {
  private val thunks = new ListBuffer[api.Lazy[?]]

  /** Force all unevaluated thunks to prevent space leaks. */
  @tailrec protected final def forceThunks(): Unit = if (!thunks.isEmpty) {
    val toForce = thunks.toList
    thunks.clear()
    toForce.foreach(_.get())
    // Forcing thunks may create new thunks
    forceThunks()
  }

  /** Store the by-name parameter `s` in a `Lazy` container without evaluating it.
   *  It will be forced by the next call to `forceThunks()`
   */
  def lzy[T <: AnyRef](t: => T): api.Lazy[T] = {
    val l = api.SafeLazy.apply(() => t).nn
    thunks += l
    l
  }
}
