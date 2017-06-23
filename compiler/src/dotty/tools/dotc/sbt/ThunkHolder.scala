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
    val l = SafeLazyWrapper(() => t)
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
    SafeLazyWrapper.strict(t)
}

/** Wrapper around SafeLazy implementations.
 *
 *  `xsbti.SafeLazy` is part of sbt but it is not part of the `interface` jar
 *  that dotty depends on, therefore we can only access it by reflection,
 *  and this will only succeed when dotty is run by sbt (otherwise
 *  `xsbti.SafeLazy` won't be on the classpath at all).
 *
 *  For testing purposes, we still want to be able to run the sbt phases outside
 *  of sbt, using `-Yforce-sbt-phases` and `-Ydump-sbt-inc`, therefore we
 *  provide a copy of SafeLazy in `dotty.tools.dotc.sbt.SafeLazy` that we use
 *  when `xsbti.SafeLazy` is unavailable.
 *
 *  This raises a question: why bother with `xsbti.SafeLazy` if we have our own
 *  version anyway? Because sbt uses Java serialization to persist the output of
 *  the incremental compilation analysis when sbt is stopped and restarted. If
 *  we used `dotty.tools.dotc.sbt.SafeLazy` with sbt, deserialization would fail
 *  and every restart of sbt would require a full recompilation.
 *
 *  Note: this won't be needed once we switch to zinc 1.0 where `SafeLazy` becomes
 *  part of the `interface` jar, see https://github.com/sbt/zinc/issues/113
 */
private object SafeLazyWrapper {

  @sharable private[this] val safeLazy =
    try {
      Class.forName("xsbti.SafeLazy")
    } catch {
      case e: ClassNotFoundException =>
        null
    }

  @sharable private[this] val safeLazyApply =
    if (safeLazy != null)
      safeLazy.getMethod("apply", classOf[xsbti.F0[_]])
    else
      null
  @sharable private[this] val safeLazyStrict =
    if (safeLazy != null)
      safeLazy.getMethod("strict", classOf[Object])
    else
      null

  def apply[T <: AnyRef](eval: () => T): xsbti.api.Lazy[T] =
    if (safeLazyApply != null)
      safeLazyApply
        .invoke(null, new xsbti.F0[T] { def apply() = eval() })
        .asInstanceOf[xsbti.api.Lazy[T]]
    else
      SafeLazy(eval)

  def strict[T <: AnyRef](value: T): xsbti.api.Lazy[T] =
    if (safeLazyStrict != null)
      safeLazyStrict
        .invoke(null, value)
        .asInstanceOf[xsbti.api.Lazy[T]]
    else
      SafeLazy.strict(value)
}

// Adapted from https://github.com/sbt/sbt/blob/0.13/compile/api/src/main/scala/xsbti/SafeLazy.scala
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
    def get(): T = _t
  }

  private[this] final class Strict[T <: AnyRef](val get: T) extends xsbti.api.Lazy[T] with java.io.Serializable
}
