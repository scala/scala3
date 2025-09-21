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

private[concurrent] object INodeBase {
  @static
  final val updater: AtomicReferenceFieldUpdater[INodeBase[?, ?], MainNode[?, ?]] =
    AtomicReferenceFieldUpdater.newUpdater(classOf[INodeBase[?, ?]], classOf[MainNode[?, ?]], "mainnode")

  @static final val RESTART: Object = new Object()

  @static final val NO_SUCH_ELEMENT_SENTINEL: Object = new Object()
}

private[concurrent] abstract class INodeBase[K, V](val gen: Gen) extends BasicNode {

  @volatile var mainnode: MainNode[K, V]  = null

  def prev(): BasicNode = null
}
