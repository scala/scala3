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

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater
import scala.annotation.static

private[concurrent] object MainNode {
  @static val updater: AtomicReferenceFieldUpdater[MainNode[?, ?], MainNode[?, ?]] =
    AtomicReferenceFieldUpdater.newUpdater(classOf[MainNode[?, ?]], classOf[MainNode[?, ?]], "prev")
}

private[concurrent] abstract class MainNode[K, V] extends BasicNode {

  @volatile var prev: MainNode[K, V] = null

  def cachedSize(ct: Object): Int

  // standard contract
  def knownSize(): Int

  def CAS_PREV(oldval: MainNode[K, V], nval: MainNode[K, V]): Boolean = {
    MainNode.updater.compareAndSet(this, oldval, nval)
  }

  def WRITE_PREV(nval: MainNode[K, V]): Unit = {
    MainNode.updater.set(this, nval)
  }

  // do we need this? unclear in the javadocs...
  // apparently not - volatile reads are supposed to be safe
  // regardless of whether there are concurrent ARFU updates
  @deprecated
  def READ_PREV(): MainNode[K, V] = {
    MainNode.updater.get(this).asInstanceOf[MainNode[K, V]]
  }
}
