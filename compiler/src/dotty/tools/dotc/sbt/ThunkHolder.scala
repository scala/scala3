package dotty.tools.dotc
package sbt

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import xsbti.api

/** Create and hold thunks. A thunk is a (potentially) unevaluated value
 *  that may be evaluated once.
 */
private[sbt] trait ThunkHolder {
  private[this] val thunks = new ListBuffer[api.Lazy[_]]

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
    val l = SafeLazy(() => t)
    thunks += l
    l
  }

  /** Store the parameter `s` in a `Lazy` container, since `s` is not by-name, there
   *  is nothing to force.
   *
   *  TODO: Get rid of this method. It is only needed because some xsbti.api classes
   *  take lazy arguments when they could be strict, but this can be fixed in sbt,
   *  see https://github.com/sbt/zinc/issues/114
   */
  def strict2lzy[T <: AnyRef](t: T): api.Lazy[T] =
    SafeLazy.strict(t)
}

// TODO: Use xsbti.SafeLazy once https://github.com/sbt/zinc/issues/113 is fixed
private object SafeLazy {
  def apply[T <: AnyRef](eval: () => T): xsbti.api.Lazy[T] =
    new Impl(eval)

  def strict[T <: AnyRef](value: T): xsbti.api.Lazy[T] =
    new Strict(value)

  private[this] final class Impl[T <: AnyRef](private[this] var eval: () => T) extends xsbti.api.AbstractLazy[T] {
    private[this] lazy val _t = {
      val t = eval()
      eval = null // clear the reference, ensuring the only memory we hold onto is the result
      t
    }
    def get: T = _t
  }

  private[this] final class Strict[T <: AnyRef](val get: T) extends xsbti.api.Lazy[T] with java.io.Serializable
}
