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

package scala.collection.concurrent

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.concurrent.atomic.AtomicIntegerFieldUpdater
import scala.annotation.static

private[concurrent] object CNodeBase {
  @static
  final val updater: AtomicIntegerFieldUpdater[CNodeBase[?, ?]] =
    AtomicIntegerFieldUpdater.newUpdater(classOf[CNodeBase[?, ?]], "csize")
}

private[concurrent] abstract class CNodeBase[K, V] extends MainNode[K, V] {

  @volatile var csize: Int = -1

  def CAS_SIZE(oldval: Int, nval: Int): Boolean = {
    CNodeBase.updater.compareAndSet(this, oldval, nval)
  }

  def WRITE_SIZE(nval: Int): Unit = {
    CNodeBase.updater.set(this, nval)
  }

  def READ_SIZE(): Int = {
    CNodeBase.updater.get(this)
  }

}
