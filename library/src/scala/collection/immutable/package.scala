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

package scala.collection

import scala.language.`2.13`
import language.experimental.captureChecking

package object immutable {
  type StringOps = scala.collection.StringOps
  /** Alias for the [[scala.collection.StringOps]] companion object, so that the name
   *  resolves under `scala.collection.immutable` alongside the type alias above.
   */
  val StringOps = scala.collection.StringOps
  type StringView = scala.collection.StringView
  /** Alias for the [[scala.collection.StringView]] companion object, so that the name
   *  resolves under `scala.collection.immutable` alongside the type alias above.
   */
  val StringView = scala.collection.StringView

  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+X] = Iterable[X]
  /** Alias for the [[Iterable]] companion object, kept so that code written against
   *  the old name can still call factory methods such as `Traversable(1, 2, 3)`.
   */
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable

  @deprecated("Use Map instead of DefaultMap", "2.13.0")
  type DefaultMap[K, +V] = scala.collection.immutable.Map[K, V]
}
