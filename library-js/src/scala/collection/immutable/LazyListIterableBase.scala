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

  /** Non-atomic stand-in for the JVM `TailUpdater`.
   *
   *  Provides the same interface for updating the `_tail` field of [[LazyListIterableBase]]
   *  instances, but with plain reads and writes: Scala.js has only one thread, so no atomicity
   *  is needed.
   */
  final class TailUpdater {
    /** Sets the `_tail` field of `ll` to `value` if it currently holds `expected`.
     *
     *  Unlike the JVM version, the check and the write are not atomic, which is sufficient in
     *  the single-threaded Scala.js runtime.
     *
     *  @param ll the lazy list whose `_tail` field to update
     *  @param expected the value `_tail` is expected to hold, compared by reference
     *  @param value the new value for `_tail`
     *  @return `true` if the field was updated, `false` if it did not hold `expected`
     */
    @inline def compareAndSet(ll: LazyListIterableBase[?]^, expected: AnyRef^{ll}, value: AnyRef^{ll}): Boolean =
      if (ll._tail eq expected) { ll._tail = unsafeAssumePure(value); true } else false
    /** Sets the `_tail` field of `ll` to `value`.
     *
     *  Unlike the JVM version, the read and the write are not atomic, which is sufficient in
     *  the single-threaded Scala.js runtime.
     *
     *  @param ll the lazy list whose `_tail` field to update
     *  @param value the new value for `_tail`
     *  @return the value `_tail` held before the update
     */
    @inline def getAndSet(ll: LazyListIterableBase[?]^, value: (AnyRef | Null)^{ll}): (AnyRef | Null)^{ll} = {
      val old = ll._tail
      ll._tail = value.asInstanceOf[AnyRef | Null]
      old
    }
  }

  /** Returns `true` always, without inspecting `t`.
   *
   *  On the JVM this method tests whether `t` is the calling thread; [[LazyListIterable]] uses
   *  it to detect a lazy state whose evaluation re-enters itself. Scala.js has only one thread,
   *  so a `Thread` found in the `_tail` field mid-evaluation can only be the current thread.
   *  Being constant `true` also lets dead code elimination drop the thread-coordination
   *  branches in [[LazyListIterable]].
   *
   *  @param t never used
   */
  def isCurrentThread(t: Thread^): Boolean = true

  /** Always throws; never called on Scala.js.
   *
   *  With `isCurrentThread` constant `true`, the branch in [[LazyListIterable]] that would
   *  create an `InRace` is preceded by a self-reference error and is eliminated as dead code.
   *
   *  @param t never used
   *  @throws Exception always
   */
  def InRace(t: Thread^): InRace = throw new Exception("unreachable")

  /** Unconstructible stand-in for the JVM `InRace` latch.
   *
   *  On the JVM, `InRace` coordinates threads racing to evaluate the same lazy state. Scala.js
   *  has only one thread, so such a race cannot occur: the constructor throws to guarantee that
   *  no instance is ever created, and the class exists only so that [[LazyListIterable]]
   *  compiles unchanged.
   */
  final class InRace private[LazyListIterableBase] () {
    throw new Exception("unreachable")

    /** Always throws; can never be called, as `InRace` cannot be constructed.
     *
     *  @throws Exception always
     */
    def owner: Thread = throw new Exception("unreachable")
    /** Does nothing; can never be called, as `InRace` cannot be constructed. */
    def await(): Unit = ()
    /** Does nothing; can never be called, as `InRace` cannot be constructed. */
    def countDown(): Unit = ()
  }
}
