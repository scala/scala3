/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import scala.language.`2.13`
import language.experimental.captureChecking

/**
 * Base class for [[LazyListIterable]] to split out code that uses concurrency utilities that are not available on Scala.js.
 */
abstract class LazyListIterableBase[+A] private[immutable] (initialTail: (AnyRef | Null)^) extends Iterable[A] with Serializable {
  /** See [[LazyListIterable._head]] for the possible states of this field. */
  @volatile private var _tail: AnyRef^{this} | Null /* () => LazyListIterable[A] | Thread | InRace | LazyListIterable[A] | Null */ =
    initialTail

  private[immutable] def rawTail: AnyRef^{this} | Null = _tail

  private[immutable] def setRawTail(value: AnyRef^{this}): Unit = _tail = value

  private[immutable] def makeTailUpdater: LazyListIterableBase.TailUpdater = LazyListIterableBase.TailUpdater()
}

private[immutable] object LazyListIterableBase {
  import caps.unsafe.unsafeAssumePure

  final class TailUpdater {
    @inline def compareAndSet(ll: LazyListIterableBase[?]^, expected: AnyRef^{ll}, value: AnyRef^{ll}): Boolean =
      if (ll._tail eq expected) { ll._tail = unsafeAssumePure(value); true } else false
    @inline def getAndSet(ll: LazyListIterableBase[?]^, value: (AnyRef | Null)^{ll}): (AnyRef | Null)^{ll} = {
      val old = ll._tail
      ll._tail = value.asInstanceOf[AnyRef | Null]
      old
    }
  }

  def isCurrentThread(t: Thread^): Boolean = true

  def InRace(t: Thread^): InRace = throw new Exception("unreachable")

  final class InRace private[LazyListIterableBase] () {
    throw new Exception("unreachable")

    def owner: Thread = throw new Exception("unreachable")
    def await(): Unit = ()
    def countDown(): Unit = ()
  }
}
