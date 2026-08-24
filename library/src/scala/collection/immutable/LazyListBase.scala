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

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import java.util.concurrent.locks.AbstractQueuedSynchronizer

/**
 * Base class for [[LazyList]] to split out code that uses concurrency utilities that are not available
 * on Scala.js. This way, Scala.js does not need to override all of LazyList.
 *
 * This class cannot be a trait because `AtomicReferenceFieldUpdater.newUpdater` checks if the caller
 * class has access to the corresponding field. So it needs to be called in the class where the field is
 * declared (fields are always private in Scala).
 */
abstract class LazyListBase[+A] private[immutable] (initialTail: AnyRef | Null) extends AbstractSeq[A] with Serializable {
  /** See [[LazyList._head]] for the possible states of this field. */
  @volatile private var _tail: AnyRef | Null /* () => LazyList[A] | Thread | InRace | LazyList[A] | Null */ = initialTail

  private[immutable] def rawTail: AnyRef | Null = _tail

  private[immutable] def setRawTail(value: AnyRef): Unit = _tail = value

  @noinline private[immutable] def makeTailUpdater: LazyListBase.TailUpdater =
    new LazyListBase.TailUpdater(AtomicReferenceFieldUpdater.newUpdater(classOf[LazyListBase[?]], classOf[AnyRef], "_tail"))
}

private[immutable] object LazyListBase {
  /** Atomic accessor for the `_tail` field of [[LazyListBase]] instances.
   *
   *  [[LazyList]] uses a single shared instance (created through `makeTailUpdater`) to perform
   *  the atomic state transitions of `_tail` during lazy state evaluation. On Scala.js this
   *  class is replaced by a variant that uses plain, non-atomic field accesses.
   *
   *  @param u the field updater for `_tail`, created by `LazyListBase.makeTailUpdater`
   */
  final class TailUpdater(u: AtomicReferenceFieldUpdater[LazyListBase[?], AnyRef]) {
    /** Atomically sets the `_tail` field of `ll` to `value` if it currently holds `expected`.
     *
     *  @param ll the lazy list whose `_tail` field to update
     *  @param expected the value `_tail` is expected to hold, compared by reference
     *  @param value the new value for `_tail`
     *  @return `true` if the field was updated, `false` if it did not hold `expected`
     */
    def compareAndSet(ll: LazyListBase[?], expected: AnyRef, value: AnyRef): Boolean = u.compareAndSet(ll, expected, value)
    /** Atomically sets the `_tail` field of `ll` to `value`.
     *
     *  @param ll the lazy list whose `_tail` field to update
     *  @param value the new value for `_tail`
     *  @return the value `_tail` held before the update
     */
    def getAndSet(ll: LazyListBase[?], value: AnyRef | Null): AnyRef | Null = u.getAndSet(ll, value)
  }

  // this utility is constant `true` on Scala.js -> enables DCE in LazyList
  /** Returns `true` if `t` is the calling thread.
   *
   *  [[LazyList]] calls this on a `Thread` (or an `InRace` owner) found in the `_tail` field to
   *  detect a lazy state whose evaluation re-enters itself: if the thread recorded as evaluating
   *  the state is the current thread, the state depends on its own result and evaluation fails
   *  instead of deadlocking. On Scala.js this method is constant `true`, which lets dead code
   *  elimination drop the thread-coordination branches in [[LazyList]].
   *
   *  @param t the thread to compare against the current thread
   */
  def isCurrentThread(t: Thread): Boolean = t eq Thread.currentThread
  // also for Scala.js
  /** Returns a new, unreleased [[InRace]] latch owned by `t`.
   *
   *  Called by [[LazyList]] when a thread finds another thread's marker in the `_tail` field,
   *  meaning that thread is currently evaluating the lazy state. The returned latch is then
   *  installed in `_tail` so that all threads losing the race can wait for the owner to finish.
   *
   *  @param t the thread currently evaluating the lazy state, recorded as the latch's owner
   */
  def InRace(t: Thread): InRace = new InRace(t)

  /** A one-shot latch coordinating threads that race to evaluate the same lazy state.
   *
   *  When a thread finds that another thread (the `owner`) is already evaluating a
   *  [[LazyList]]'s state, it installs an `InRace` in the `_tail` field and blocks in `await`
   *  until the owner completes the evaluation and calls `countDown`. [[LazyList]] also consults
   *  `owner` to detect self-referential evaluation: if the thread that finds the `InRace` is its
   *  owner, the lazy state depends on its own result.
   */
  final class InRace private[LazyListBase] (val owner: Thread) {
    // Implements a one-time latch
    final private class Sync extends AbstractQueuedSynchronizer {
      /** Returns -1 (acquisition fails, so the caller blocks) while the latch has not been
       *  released (state 0), and 1 (acquisition succeeds) once `countDown` has released it.
       *
       *  @param unused required by the `AbstractQueuedSynchronizer` contract; never used
       */
      override def tryAcquireShared(unused: Int): Int = if (getState == 0) -1 else 1
      /** Releases the latch by atomically moving the synchronizer state from 0 to 1.
       *
       *  @param unused required by the `AbstractQueuedSynchronizer` contract; never used
       *  @return `true` on the call that performs the transition, so that blocked threads are
       *          woken; `false` if the latch was already released
       */
      override def tryReleaseShared(unused: Int): Boolean = getState == 0 && compareAndSetState(0, 1)
    }

    private val sync = new Sync()

    /** Blocks the calling thread until `countDown` is called; returns immediately if the latch
     *  is already released.
     */
    def await(): Unit = sync.acquireShared(0)
    /** Releases the latch, waking all threads blocked in `await`; subsequent calls have no effect. */
    def countDown(): Unit = sync.releaseShared(0)
  }
}
