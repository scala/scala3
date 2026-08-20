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

/**
 * Base class for [[LazyList]] to split out code that uses concurrency utilities that are not available on Scala.js.
 */
abstract class LazyListBase[+A] private[immutable] (initialTail: AnyRef | Null) extends AbstractSeq[A] with Serializable {
  /** See [[LazyList._head]] for the possible states of this field. */
  @volatile private var _tail: AnyRef | Null /* () => LazyList[A] | Thread | InRace | LazyList[A] | Null */ = initialTail

  private[immutable] def rawTail: AnyRef | Null = _tail

  private[immutable] def setRawTail(value: AnyRef): Unit = _tail = value

  private[immutable] def makeTailUpdater: LazyListBase.TailUpdater = LazyListBase.TailUpdater()
}

private[immutable] object LazyListBase {
  final class TailUpdater {
    @inline def compareAndSet(ll: LazyListBase[?], expected: AnyRef, value: AnyRef): Boolean =
      if (ll._tail eq expected) { ll._tail = value; true } else false

    @inline def getAndSet(ll: LazyListBase[?], value: AnyRef | Null): AnyRef | Null = {
      val old = ll._tail
      ll._tail = value
      old
    }
  }

  def isCurrentThread(t: Thread): Boolean = true

  def InRace(t: Thread): InRace = throw new Exception("unreachable")

  final class InRace private[LazyListBase] () {
    throw new Exception("unreachable")

    def owner: Thread = throw new Exception("unreachable")
    def await(): Unit = ()
    def countDown(): Unit = ()
  }
}
