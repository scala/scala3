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
package immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.annotation.{nowarn, tailrec}

/** This class implements immutable maps using a vector/map-based data structure, which preserves insertion order.
  *
  *  Unlike `ListMap`, `VectorMap` has amortized effectively constant lookup at the expense
  *  of using extra memory and generally lower performance for other operations
  *
  *  @tparam K      the type of the keys contained in this vector map.
  *  @tparam V      the type of the values associated with the keys in this vector map.
  *
  * @define coll immutable vector map
  * @define Coll `immutable.VectorMap`
  */
final class VectorMap[K, +V] private (
    private[immutable] val fields: Vector[Any], // K | Tombstone | Null
    private[immutable] val underlying: Map[K, (Int, V)],
    dropped: Int)
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with StrictOptimizedMapOps[K, V, VectorMap, VectorMap[K, V]]
    with MapFactoryDefaults[K, V, VectorMap, Iterable] {

  import VectorMap._

  /** The name of this collection class, used as the prefix in its `toString` representation. */
  override protected def className: String = "VectorMap"

  private[immutable] def this(fields: Vector[K], underlying: Map[K, (Int, V)]) = this(fields, underlying, 0)

  /** The number of key-value pairs in this map. */
  override val size = underlying.size

  /** Returns the size of this map: the size is always known. */
  override def knownSize: Int = size

  /** Returns `true` if this map contains no key-value pairs, `false` otherwise. */
  override def isEmpty: Boolean = size == 0

  /** Returns a `VectorMap` with `key` bound to `value`.
   *
   *  If `key` is already present, the new value replaces the old one at the key's
   *  existing position in insertion order; otherwise the new binding is appended
   *  after all existing bindings.
   *
   *  @tparam V1 the value type of the returned map, a supertype of `V`
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return a `VectorMap` containing the bindings of this map and `key -> value`
   */
  def updated[V1 >: V](key: K, value: V1): VectorMap[K, V1] = {
    underlying.get(key) match {
      case Some((slot, _)) =>
        new VectorMap(fields, underlying.updated[(Int, V1)](key, (slot, value)), dropped)
      case None =>
        new VectorMap(fields :+ key, underlying.updated[(Int, V1)](key, (fields.length + dropped, value)), dropped)
    }
  }

  /** Returns the same map with a given default function, wrapping this map without copying it.
   *
   *  The default is only used by `apply` and `default`; methods like `get`, `contains`
   *  and iteration are unaffected.
   *
   *  @tparam V1 the type of the values returned by the default function, a supertype of `V`
   *  @param d the function mapping keys to values, used for non-present keys
   *  @return a wrapper of this map with a default function
   */
  override def withDefault[V1 >: V](d: K -> V1): Map[K, V1] =
    new Map.WithDefault(this, d)

  /** Returns the same map with a given default value, wrapping this map without copying it.
   *
   *  The default is only used by `apply` and `default`; methods like `get`, `contains`
   *  and iteration are unaffected.
   *
   *  @tparam V1 the type of the default value, a supertype of `V`
   *  @param d the value returned for non-present keys
   *  @return a wrapper of this map with a default value
   */
  override def withDefaultValue[V1 >: V](d: V1): Map[K, V1] =
    new Map.WithDefault[K, V1](this, _ => d)

  /** Returns the value associated with `key`, if present.
   *
   *  @param key the key to look up
   *  @return `Some` of the value bound to `key`, or `None` if `key` is not in this map
   */
  def get(key: K): Option[V] = underlying.get(key) match {
    case Some(v) => Some(v._2)
    case None    => None
  }

  @tailrec
  private def nextValidField(slot: Int): (Int, K) = {
    if (slot >= fields.size) (-1, null.asInstanceOf[K])
    else fields(slot) match {
      case Tombstone(distance) => nextValidField(slot + distance)
      case k /*: K | Null */   => (slot, k.asInstanceOf[K])
    }
  }

  /** Returns an iterator over the key-value pairs of this map, in insertion order.
   *
   *  The iterator traverses the underlying key vector. Slots of removed keys are not
   *  reclaimed but marked with tombstones, which the iterator skips (a run of adjacent
   *  tombstones is skipped in a single step), so after many removals a full traversal
   *  can cost more than the current size of the map suggests.
   */
  def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
    private val fieldsLength = fields.length
    private var slot = -1
    private var key: K = null.asInstanceOf[K]

    private def advance(): Unit = {
      val nextSlot = slot + 1
      if (nextSlot >= fieldsLength) {
        slot = fieldsLength
        key = null.asInstanceOf[K]
      } else {
        nextValidField(nextSlot) match {
          case (-1, _) =>
            slot = fieldsLength
            key = null.asInstanceOf[K]
          case (s, k) =>
            slot = s
            key = k
        }
      }
    }

    advance()

    override def hasNext: Boolean = slot < fieldsLength

    override def next(): (K, V) =
      if (!hasNext) Iterator.empty.next()
      else {
        val result = (key, underlying(key)._2)
        advance()
        result
      }
  }

  // No-Op overrides to allow for more efficient steppers in a minor release.
  // Refining the return type to `S with EfficientSplit` is binary compatible.

  /** Returns a [[Stepper]] for the key-value pairs of this map, delegating to the
   *  inherited implementation.
   *
   *  This override exists only so that the return type can later be refined to
   *  `S with EfficientSplit` in a binary compatible way.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a `Stepper` over the key-value pairs of this map
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[(K, V), S]): S = super.stepper(using shape)

  /** Returns a [[Stepper]] for the keys of this map, delegating to the inherited
   *  implementation.
   *
   *  This override exists only so that the return type can later be refined to
   *  `S with EfficientSplit` in a binary compatible way.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a `Stepper` over the keys of this map
   */
  override def keyStepper[S <: Stepper[?]](implicit shape: StepperShape[K, S]): S = super.keyStepper(using shape)

  /** Returns a [[Stepper]] for the values of this map, delegating to the inherited
   *  implementation.
   *
   *  This override exists only so that the return type can later be refined to
   *  `S with EfficientSplit` in a binary compatible way.
   *
   *  @tparam S the type of the returned `Stepper`, determined by the implicit `StepperShape`
   *  @param shape the `StepperShape` that determines the concrete `Stepper` subtype to return
   *  @return a `Stepper` over the values of this map
   */
  override def valueStepper[S <: Stepper[?]](implicit shape: StepperShape[V, S]): S = super.valueStepper(using shape)


  /** Returns a `VectorMap` without a binding for `key`.
   *
   *  An empty map yields the shared empty `VectorMap` without `key` being looked up at
   *  all, so an empty map that is not that instance is replaced by it. Otherwise, if
   *  `key` is absent this map itself is returned, and if it is the only key the shared
   *  empty `VectorMap` is. In the remaining case the key's slot in the underlying vector is not
   *  reclaimed but replaced by a tombstone, and adjacent tombstones are linked so that
   *  iteration can skip a whole run of removed slots in one step. The relative order
   *  of the remaining bindings is unchanged.
   *
   *  @param key the key to remove
   */
  def removed(key: K): VectorMap[K, V] = {
    if (isEmpty) empty
    else {
      var fs = fields
      val sz = fs.size
      underlying.get(key) match {
        case Some(_) if size == 1 => empty
        case Some((slot, _)) =>
          val s = slot - dropped

          // Calculate next of kin
          val next =
            if (s < sz - 1) fs(s + 1) match {
              case Tombstone(d) => s + d + 1
              case _ => s + 1
            } else s + 1

          fs = fs.updated(s, Tombstone(next - s))

          // Calculate first index of preceding tombstone sequence
          val first =
            if (s > 0) {
              fs(s - 1) match {
                case Tombstone(d) if d < 0 => if (s + d >= 0) s + d else 0
                case Tombstone(d) if d == 1 => s - 1
                case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
                case _ => s
              }
            }else s
          fs = fs.updated(first, Tombstone(next - first))

          // Calculate last index of succeeding tombstone sequence
          val last = next - 1
          if (last != first) {
            fs = fs.updated(last, Tombstone(first - 1 - last))
          }
          new VectorMap(fs, underlying - key, dropped)
        case _ =>
          this
      }
    }
  }

  /** Returns the [[VectorMap$ VectorMap]] companion object as the factory for maps of this kind. */
  override def mapFactory: MapFactory[VectorMap] = VectorMap

  /** Returns `true` if this map has a binding for `key`, `false` otherwise.
   *
   *  This is a lookup in the underlying hash map, so it takes amortized effectively
   *  constant time.
   *
   *  @param key the key to test
   */
  override def contains(key: K): Boolean = underlying.contains(key)

  /** Returns the first key-value pair of this map, in insertion order.
   *
   *  @throws NoSuchElementException if this map is empty
   */
  override def head: (K, V) = iterator.next()

  /** Returns the last key-value pair of this map, in insertion order.
   *
   *  Found from the end of the underlying vector, consulting at most one tombstone,
   *  so this takes effectively constant time.
   *
   *  @throws UnsupportedOperationException if this map is empty
   */
  override def last: (K, V) = {
    if (isEmpty) throw new UnsupportedOperationException("empty.last")
    val lastSlot = fields.length - 1
    val last = fields.last match {
      case Tombstone(d) if d < 0 => fields(lastSlot + d).asInstanceOf[K]
      case Tombstone(d) if d == 1 => fields(lastSlot - 1).asInstanceOf[K]
      case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
      case k => k.asInstanceOf[K]
    }
    (last, underlying(last)._2)
  }

  /** Returns `Some` of the last key-value pair of this map, in insertion order, or
   *  `None` if this map is empty.
   */
  override def lastOption: Option[(K, V)] = {
    if (isEmpty) None
    else Some(last)
  }

  /** Returns a `VectorMap` with all bindings of this map except the first, in insertion order.
   *
   *  @throws UnsupportedOperationException if this map is empty
   */
  override def tail: VectorMap[K, V] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.tail")
    val (slot, key) = nextValidField(0)
    new VectorMap(fields.drop(slot + 1), underlying - key, dropped + slot + 1)
  }

  /** Returns a `VectorMap` with all bindings of this map except the last, in insertion order.
   *
   *  @throws UnsupportedOperationException if this map is empty
   */
  override def init: VectorMap[K, V] = {
    if (isEmpty) throw new UnsupportedOperationException("empty.init")
    val lastSlot = fields.size - 1
    val (slot, key) = fields.last match {
      case Tombstone(d) if d < 0 => (lastSlot + d, fields(lastSlot + d).asInstanceOf[K])
      case Tombstone(d) if d == 1 => (lastSlot - 1, fields(lastSlot - 1).asInstanceOf[K])
      case Tombstone(d) => throw new IllegalStateException("tombstone indicate wrong position: " + d)
      case k => (lastSlot, k.asInstanceOf[K])
    }
    new VectorMap(fields.dropRight(fields.size - slot), underlying - key, dropped)
  }

  /** A [[Vector]] of the keys contained by this map.
   *
   *  @return  a [[Vector]] of the keys contained by this map.
   */
  @nowarn("msg=overriding method keys")
  override def keys: Vector[K] = keysIterator.toVector

  /** Returns the values of this map as an `Iterable`, in the insertion order of their keys.
   *
   *  The returned collection is a lazy wrapper: its elements are computed from this
   *  map on each traversal.
   */
  override def values: Iterable[V] = new Iterable[V] with IterableFactoryDefaults[V, Iterable] {
    override def iterator: Iterator[V] = keysIterator.map(underlying(_)._2)
  }
}

object VectorMap extends MapFactory[VectorMap] {
  //Class to mark deleted slots in 'fields'.
  //When one or more consecutive slots are deleted, the 'distance' of the first 'Tombstone'
  // represents the distance to the location of the next undeleted slot (or the last slot in 'fields' +1 if it does not exist).
  //When two or more consecutive slots are deleted, the 'distance' of the trailing 'Tombstone'
  // represents the distance to the location of the previous undeleted slot ( or -1 if it does not exist) multiplied by -1.
  //For other deleted slots, it simply indicates that they have been deleted.
  private[VectorMap] final case class Tombstone(distance: Int)

  private final val EmptyMap: VectorMap[Nothing, Nothing] =
    new VectorMap[Nothing, Nothing](Vector.empty[Nothing], HashMap.empty[Nothing, (Int, Nothing)])

  /** An empty [[VectorMap]].
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return the empty `VectorMap` (a single cached instance)
   */
  def empty[K, V]: VectorMap[K, V] = EmptyMap.asInstanceOf[VectorMap[K, V]]

  /** Returns a [[VectorMap]] containing the key-value pairs of `it`, in its iteration order.
   *
   *  If `it` is already a `VectorMap`, it is returned unchanged. If a key occurs more
   *  than once in `it`, its first occurrence determines its position and its last
   *  occurrence determines its value.
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @param it the source collection of key-value pairs
   *  @return a `VectorMap[K, V]` with the bindings of `it`
   */
  def from[K, V](it: collection.IterableOnce[(K, V)]^): VectorMap[K, V] =
    (it: @unchecked) match {
      case vm: VectorMap[K, V] => vm
      case _                   => (newBuilder[K, V] ++= it).result()
    }

  /** Returns a new builder for a [[VectorMap]].
   *
   *  @tparam K the type of the keys
   *  @tparam V the type of the values
   *  @return a `Builder` that accepts key-value pairs and produces a `VectorMap[K, V]`
   */
  def newBuilder[K, V]: mutable.Builder[(K, V), VectorMap[K, V]] = new VectorMapBuilder[K, V]
}

private[immutable] final class VectorMapBuilder[K, V] extends mutable.Builder[(K, V), VectorMap[K, V]] {
  private val vectorBuilder = new VectorBuilder[K]
  private val mapBuilder = new MapBuilderImpl[K, (Int, V)]
  @annotation.stableNull
  private var aliased: VectorMap[K, V] | Null = null

  override def clear(): Unit = {
    vectorBuilder.clear()
    mapBuilder.clear()
    aliased = null
  }

  override def result(): VectorMap[K, V] = {
    if (aliased eq null) {
      aliased = new VectorMap(vectorBuilder.result(), mapBuilder.result())
    }
    aliased
  }
  def addOne(key: K, value: V): this.type = {
    if (aliased ne null) {
      aliased = aliased.updated(key, value)
    } else {
      mapBuilder.getOrElse(key, null) match {
        case (slot, _) =>
          mapBuilder.addOne(key, (slot, value))
        case null =>
          val vectorSize = vectorBuilder.size
          vectorBuilder.addOne(key)
          mapBuilder.addOne(key, (vectorSize, value))
      }
    }
    this
  }

  override def addOne(elem: (K, V)): this.type = addOne(elem._1, elem._2)
}
