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

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import java.util.concurrent.locks.AbstractQueuedSynchronizer

/**
 * Base class for [[LazyListIterable]] to split out code that uses concurrency utilities that are not
 * available on Scala.js. This way, Scala.js does not need to override all of LazyListIterable.
 *
 * This class cannot be a trait because `AtomicReferenceFieldUpdater.newUpdater` checks if the caller
 * class has access to the corresponding field. So it needs to be called in the class where the field is
 * declared (fields are always private in Scala).
 */
abstract class LazyListIterableBase[+A] private[immutable] (initialTail: (AnyRef | Null)^) extends Iterable[A] with Serializable {
  /** See [[LazyListIterable._head]] for the possible states of this field. */
  @volatile private var _tail: AnyRef^{this} | Null /* () => LazyListIterable[A] | Thread | InRace | LazyListIterable[A] | Null */ =
    initialTail

  private[immutable] def rawTail: AnyRef^{this} | Null = _tail

  private[immutable] def setRawTail(value: AnyRef^{this}): Unit = _tail = value

  @noinline private[immutable] def makeTailUpdater: LazyListIterableBase.TailUpdater =
    new LazyListIterableBase.TailUpdater(AtomicReferenceFieldUpdater.newUpdater(classOf[LazyListIterableBase[?]], classOf[AnyRef], "_tail"))
}

private[immutable] object LazyListIterableBase {
  import caps.unsafe.unsafeAssumePure

  final class TailUpdater(u: AtomicReferenceFieldUpdater[LazyListIterableBase[?], AnyRef]) {
    def compareAndSet(ll: LazyListIterableBase[?]^, expected: AnyRef^{ll}, value: AnyRef^{ll}): Boolean =
      u.compareAndSet(unsafeAssumePure(ll), unsafeAssumePure(expected), unsafeAssumePure(value))
    def getAndSet(ll: LazyListIterableBase[?]^, value: (AnyRef | Null)^{ll}): (AnyRef | Null)^{ll} =
      u.getAndSet(unsafeAssumePure(ll), value.asInstanceOf[AnyRef | Null])
  }

  // this utility is constant `true` on Scala.js -> enables DCE in LazyListIterable
  def isCurrentThread(t: Thread^): Boolean = t eq Thread.currentThread
  // also for Scala.js
  def InRace(t: Thread^): InRace = new InRace(unsafeAssumePure(t))

  final class InRace private[LazyListIterableBase] (val owner: Thread) {
    // Implements a one-time latch
    final private class Sync extends AbstractQueuedSynchronizer {
      override def tryAcquireShared(unused: Int): Int = if (getState == 0) -1 else 1
      override def tryReleaseShared(unused: Int): Boolean = getState == 0 && compareAndSetState(0, 1)
    }

    private val sync = new Sync()

    def await(): Unit = sync.acquireShared(0)
    def countDown(): Unit = sync.releaseShared(0)
  }
}
