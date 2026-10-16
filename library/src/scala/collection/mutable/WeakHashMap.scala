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

package scala
package collection
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.nowarn
import scala.collection.convert.JavaCollectionWrappers.{JMapWrapper, JMapWrapperLike}

/** A hash map with references to entries which are weakly reachable. Entries are
 *  removed from this map when the key is no longer (strongly) referenced. This class wraps
 *  `java.util.WeakHashMap`.
 *
 *  @tparam K      type of keys contained in this map
 *  @tparam V      type of values associated with the keys
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#weak-hash-maps)
 *  section on `Weak Hash Maps` for more information.
 *
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@SerialVersionUID(3L)
class WeakHashMap[K, V] extends JMapWrapper[K, V](new java.util.WeakHashMap)
    with JMapWrapperLike[K, V, WeakHashMap, WeakHashMap[K, V]]
    with MapFactoryDefaults[K, V, WeakHashMap, Iterable] {
  /** Returns a new empty `WeakHashMap` of the same key and value types. */
  override def empty = new WeakHashMap[K, V]
  /** Returns the companion object `WeakHashMap`, which builds maps of this kind. */
  override def mapFactory: MapFactory[WeakHashMap] = WeakHashMap
  /** Returns `"WeakHashMap"`, the name used in this map's string representation. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "WeakHashMap"
}

/** $factoryInfo
 *  @define Coll `WeakHashMap`
 *  @define coll weak hash map
 */
@SerialVersionUID(3L)
object WeakHashMap extends MapFactory[WeakHashMap] {
  /** Creates a new empty `WeakHashMap`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @return a new empty `WeakHashMap`
   */
  def empty[K, V]: WeakHashMap[K,V] = new WeakHashMap[K, V]
  /** Creates a new `WeakHashMap` from a collection of key/value pairs.
   *
   *  If several pairs share a key, the last one wins. Like any `WeakHashMap`,
   *  the result holds its keys weakly, so entries whose keys are no longer
   *  strongly referenced elsewhere may disappear from it.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the key/value pairs to initialize the map with
   *  @return a new `WeakHashMap` containing the pairs of `it`
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): WeakHashMap[K,V] = Growable.from(empty[K, V], it)
  /** Creates a new empty builder for a `WeakHashMap`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @return a new builder that produces a `WeakHashMap` from the key/value pairs added to it
   */
  def newBuilder[K, V]: Builder[(K, V), WeakHashMap[K,V]] = new GrowableBuilder(WeakHashMap.empty[K, V])
}

