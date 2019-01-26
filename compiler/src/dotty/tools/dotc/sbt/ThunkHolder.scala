/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
    val l = api.SafeLazy.apply(() => t)
    thunks += l
    l
  }
}
