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
package immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import java.lang.IllegalStateException

import scala.collection.generic.{BitOperations, DefaultSerializationProxy}
import scala.collection.mutable.{Builder, ImmutableBuilder, ListBuffer}
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import scala.language.implicitConversions

/** Utility class for long maps. */
private[immutable] object LongMapUtils extends BitOperations.Long {
  /** Returns a mask with a single bit set at the highest position where `i` and `j`
   *  differ, or zero if they are equal.
   *
   *  @param i the first prefix
   *  @param j the second prefix
   */
  def branchMask(i: Long, j: Long) = highestOneBit(i ^ j)

  /** Joins two maps with differing prefixes under a new `Bin` node.
   *
   *  The branching bit of the new node is the highest bit at which `p1` and `p2`
   *  differ; the map whose prefix has a zero at that bit becomes the left subtree.
   *
   *  @tparam T the type of the values
   *  @param p1 the prefix of `t1`
   *  @param t1 the first map
   *  @param p2 the prefix of `t2`
   *  @param t2 the second map
   *  @return a `Bin` node with `t1` and `t2` as subtrees, ordered by the branching bit
   */
  def join[T](p1: Long, t1: LongMap[T], p2: Long, t2: LongMap[T]): LongMap[T] = {
    val m = branchMask(p1, p2)
    val p = mask(p1, m)
    if (zero(p1, m)) LongMap.Bin(p, m, t1, t2)
    else LongMap.Bin(p, m, t2, t1)
  }

  /** Builds a `Bin` node from the given subtrees, collapsing empty ones.
   *
   *  If either subtree is `Nil`, returns the other subtree unchanged, maintaining
   *  the invariant that `Nil` never occurs inside a non-empty map.
   *
   *  @tparam T the type of the values
   *  @param prefix the bits all keys under the node have in common above the branching bit
   *  @param mask the mask with only the branching bit set
   *  @param left the subtree of keys with a zero at the branching bit
   *  @param right the subtree of keys with a one at the branching bit
   *  @return `left` if `right` is empty, `right` if `left` is empty, otherwise a new
   *          `Bin` node with the given prefix, mask and subtrees
   */
  def bin[T](prefix: Long, mask: Long, left: LongMap[T], right: LongMap[T]): LongMap[T] = (left, right) match {
    case (left, LongMap.Nil) => left
    case (LongMap.Nil, right) => right
    case (left, right) => LongMap.Bin(prefix, mask, left, right)
  }
}

import LongMapUtils.{Long => _, _}

/** A companion object for long maps.
 *
 *  @define Coll  `LongMap`
 */
object LongMap {
  /** Returns the empty map, a single instance shared between all value types.
   *
   *  @tparam T the type of the values
   */
  def empty[T]: LongMap[T]  = LongMap.Nil
  /** Returns a map containing only the given key/value binding.
   *
   *  @tparam T the type of the value
   *  @param key the key of the single binding
   *  @param value the value associated with `key`
   */
  def singleton[T](key: Long, value: T): LongMap[T] = LongMap.Tip(key, value)
  /** Returns a map containing the given key/value pairs.
   *
   *  If a key occurs more than once in `elems`, the last binding for that key is
   *  retained.
   *
   *  @tparam T the type of the values
   *  @param elems the key/value pairs of the map
   */
  def apply[T](elems: (Long, T)*): LongMap[T] =
    elems.foldLeft(empty[T])((x, y) => x.updated(y._1, y._2))

  /** Returns a map containing the key/value pairs of the given collection.
   *
   *  If a key occurs more than once in `coll`, the last binding for that key is
   *  retained.
   *
   *  @tparam V the type of the values
   *  @param coll the collection of key/value pairs
   */
  def from[V](coll: IterableOnce[(Long, V)]^): LongMap[V] =
    newBuilder[V].addAll(coll).result()

  /** Returns a new builder that accumulates key/value pairs into a `LongMap`.
   *
   *  If a key is added more than once, the last binding for that key is retained.
   *
   *  @tparam V the type of the values
   */
  def newBuilder[V]: Builder[(Long, V), LongMap[V]] =
    new ImmutableBuilder[(Long, V), LongMap[V]](empty) {
      def addOne(elem: (Long, V)): this.type = { elems = elems + elem; this }
    }

  private[immutable] case object Nil extends LongMap[Nothing] {
    // Important, don't remove this! See IntMap for explanation.
    /** Compares this empty map with `that` for equality.
     *
     *  `Nil` equals itself and is unequal to every other `LongMap`, since the only
     *  empty `LongMap` is this object. For anything else it falls back to the
     *  structural equality of maps, so it equals any other empty `Map`.
     *
     *  @param that the value to compare with
     */
    override def equals(that : Any) = that match {
      case _: this.type  => true
      case _: LongMap[?] => false // The only empty LongMaps are eq Nil
      case _             => super.equals(that)
    }
  }

  private[immutable] case class Tip[+T](key: Long, value: T) extends LongMap[T] {
    /** Returns a `Tip` with this node's key and the given value, reusing this node
     *  when possible.
     *
     *  If `s` is the same reference as the current value, returns this node (cast
     *  to the new value type) to preserve sharing; otherwise creates a new `Tip`.
     *
     *  @tparam S the type of the new value
     *  @param s the new value
     */
    def withValue[S](s: S) =
      if (s.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) this.asInstanceOf[LongMap.Tip[S]]
      else LongMap.Tip(key, s)
  }

  private[immutable] case class Bin[+T](prefix: Long, mask: Long, left: LongMap[T], right: LongMap[T]) extends LongMap[T] {
    /** Returns a `Bin` with this node's prefix and mask and the given subtrees,
     *  reusing this node when possible.
     *
     *  @tparam S the type of the values in the subtrees
     *  @param left the new left subtree
     *  @param right the new right subtree
     *  @return this node (cast to the new value type) if both subtrees are the same
     *          references as this node's, to preserve sharing; otherwise a new `Bin`
     */
    def bin[S](left: LongMap[S], right: LongMap[S]): LongMap[S] = {
      if ((this.left eq left) && (this.right eq right)) this.asInstanceOf[LongMap.Bin[S]]
      else LongMap.Bin[S](prefix, mask, left, right)
    }
  }

  /** Implicitly converts the `LongMap` object to a `Factory`, so that it can be
   *  passed where a factory of long-keyed pairs is expected.
   *
   *  @tparam V the type of the values
   *  @param dummy the `LongMap` companion object; never used
   *  @return a `Factory` building a `LongMap[V]` from `(Long, V)` pairs
   */
  implicit def toFactory[V](dummy: LongMap.type): Factory[(Long, V), LongMap[V]] = ToFactory.asInstanceOf[Factory[(Long, V), LongMap[V]]]

  @SerialVersionUID(3L)
  private object ToFactory extends Factory[(Long, AnyRef), LongMap[AnyRef]] with Serializable {
    /** Returns a `LongMap` containing the key/value pairs of `it`.
     *
     *  @param it the collection of key/value pairs
     */
    def fromSpecific(it: IterableOnce[(Long, AnyRef)]^): LongMap[AnyRef] = LongMap.from[AnyRef](it)
    /** Returns a new builder that accumulates key/value pairs into a `LongMap`. */
    def newBuilder: Builder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  /** Implicitly converts the `LongMap` object to a `BuildFrom`, so that it can be
   *  passed where a `BuildFrom` producing a long-keyed map is expected.
   *
   *  @tparam V the type of the values
   *  @param factory the `LongMap` companion object; never used
   *  @return a `BuildFrom` building a `LongMap[V]` from `(Long, V)` pairs, whatever
   *          the source collection
   */
  implicit def toBuildFrom[V](factory: LongMap.type): BuildFrom[Any, (Long, V), LongMap[V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (Long, V), LongMap[V]]]
  private object ToBuildFrom extends BuildFrom[Any, (Long, AnyRef), LongMap[AnyRef]] {
    /** Returns a `LongMap` containing the key/value pairs of `it`.
     *
     *  @param from the source collection; never used
     *  @param it the collection of key/value pairs
     */
    def fromSpecific(from: Any)(it: IterableOnce[(Long, AnyRef)]^) = LongMap.from(it)
    /** Returns a new builder that accumulates key/value pairs into a `LongMap`.
     *
     *  @param from the source collection; never used
     */
    def newBuilder(from: Any) = LongMap.newBuilder[AnyRef]
  }

  /** Returns an implicit `Factory` building a `LongMap[V]` from `(Long, V)` pairs.
   *
   *  @tparam V the type of the values
   */
  implicit def iterableFactory[V]: Factory[(Long, V), LongMap[V]] = toFactory(this)
  /** Returns an implicit `BuildFrom` building a `LongMap[V]` from `(Long, V)` pairs,
   *  for transformation methods whose source collection is a `LongMap`.
   *
   *  @tparam V the type of the values
   */
  implicit def buildFromLongMap[V]: BuildFrom[LongMap[?], (Long, V), LongMap[V]] = toBuildFrom(this)
}

// Iterator over a non-empty LongMap.
private[immutable] abstract class LongMapIterator[V, T](it: LongMap[V]) extends AbstractIterator[T] {

  // Basically this uses a simple stack to emulate conversion over the tree. However
  // because we know that Longs are only 64 bits we can have at most 64 LongMap.Bins and
  // one LongMap.Tip sitting on the tree at any point. Therefore we know the maximum stack
  // depth is 65
  /** The current stack depth: the index of the next free slot in `buffer`. */
  var index = 0
  /** The stack of subtrees still to be traversed; `buffer(index - 1)` is the top. */
  var buffer = new Array[AnyRef](65)

  /** Removes and returns the top subtree of the stack. Must not be called when the
   *  stack is empty.
   */
  def pop() = {
    index -= 1
    buffer(index).asInstanceOf[LongMap[V]]
  }

  /** Pushes a subtree onto the stack.
   *
   *  @param x the subtree still to be traversed
   */
  def push(x: LongMap[V]): Unit = {
    buffer(index) = x.asInstanceOf[AnyRef]
    index += 1
  }
  push(it)

  /** What value do we assign to a tip?
   *
   *  @param tip the leaf node to extract a value from
   *  @return the element of type `T` (e.g. the key, the value, or the key-value pair) extracted from `tip`
   */
  def valueOf(tip: LongMap.Tip[V]): T

  /** Returns `true` if the stack is non-empty, that is, if elements remain. */
  def hasNext = index != 0
  /** Returns the next element, in unsigned order of the keys.
   *
   *  Pops the top subtree and descends along left children, pushing each right
   *  sibling, until a `Tip` is reached, whose element is returned; a `Bin` whose
   *  left child is a `Tip` is handled directly, pushing only its right child.
   *  Must not be called when `hasNext` is false.
   *
   *  @throws IllegalStateException if a `Nil` occurs inside a subtree, which never
   *          happens for a well-formed map
   */
  @tailrec
  final def next(): T =
    pop() match {
      case LongMap.Bin(_,_, t@LongMap.Tip(_, _), right) => {
        push(right)
        valueOf(t)
      }
      case LongMap.Bin(_, _, left, right) => {
        push(right)
        push(left)
        next()
      }
      case t@LongMap.Tip(_, _) => valueOf(t)
      // This should never happen. We don't allow LongMap.Nil in subtrees of the LongMap
      // and don't return an LongMapIterator for LongMap.Nil.
      case LongMap.Nil => throw new IllegalStateException("Empty maps not allowed as subtrees")
    }
}

private[immutable] class LongMapEntryIterator[V](it: LongMap[V]) extends LongMapIterator[V, (Long, V)](it){
  /** Returns the key/value pair stored in `tip`.
   *
   *  @param tip the leaf node
   */
  def valueOf(tip: LongMap.Tip[V]) = (tip.key, tip.value)
}

private[immutable] class LongMapValueIterator[V](it: LongMap[V]) extends LongMapIterator[V, V](it){
  /** Returns the value stored in `tip`.
   *
   *  @param tip the leaf node
   */
  def valueOf(tip: LongMap.Tip[V]) = tip.value
}

private[immutable] class LongMapKeyIterator[V](it: LongMap[V]) extends LongMapIterator[V, Long](it){
  /** Returns the key stored in `tip`.
   *
   *  @param tip the leaf node
   */
  def valueOf(tip: LongMap.Tip[V]) = tip.key
}

/** Specialised immutable map structure for long keys, based on
 *  [Fast Mergeable Long Maps](https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.37.5452)
 *  by Okasaki and Gill. Essentially a trie based on binary digits of the integers.
 *
 *  Note: This class is as of 2.8 largely superseded by HashMap.
 *
 *  @tparam T      type of the values associated with the long keys.
 *
 *  @define Coll `immutable.LongMap`
 *  @define coll immutable long integer map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed abstract class LongMap[+T] extends AbstractMap[Long, T]
  with StrictOptimizedMapOps[Long, T, Map, LongMap[T]]
  with Serializable {

  /** Returns a `LongMap` containing the key/value pairs of `coll`.
   *
   *  If a key occurs more than once in `coll`, the last binding for that key is
   *  retained.
   *
   *  @param coll the collection of key/value pairs
   */
  override protected def fromSpecific(coll: (scala.collection.IterableOnce[(Long, T)]^) @uncheckedVariance): LongMap[T] = {
    //TODO should this be the default implementation of this method in StrictOptimizedIterableOps?
    val b = newSpecificBuilder
    b.sizeHint(coll)
    b.addAll(coll)
    b.result()
  }
  /** Returns a new builder that accumulates key/value pairs into a `LongMap`. */
  override protected def newSpecificBuilder: Builder[(Long, T), LongMap[T]] @uncheckedVariance =
    new ImmutableBuilder[(Long, T), LongMap[T]](empty) {
      def addOne(elem: (Long, T)): this.type = { elems = elems + elem; this }
    }

  /** Returns the empty `LongMap`, a single shared instance. */
  override def empty: LongMap[T] = LongMap.Nil

  /** Returns a list of the key/value pairs of this map, in unsigned order of the keys. */
  override def toList = {
    val buffer = new ListBuffer[(Long, T)]
    foreach(buffer += _)
    buffer.toList
  }

  /** Iterator over key, value pairs of the map in unsigned order of the keys.
   *
   *  @return an iterator over pairs of long keys and corresponding values.
   */
  def iterator: Iterator[(Long, T)] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapEntryIterator(this)
  }

  /** Loops over the key, value pairs of the map in unsigned order of the keys.
   *
   *  @tparam U the return type of the function `f`, used only for side effects
   *  @param f the function applied to each key-value pair in the map
   */
  override final def foreach[U](f: ((Long, T)) => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreach(f); right.foreach(f) }
    case LongMap.Tip(key, value) => f((key, value))
    case LongMap.Nil =>
  }

  /** Loops over the key, value pairs of the map in unsigned order of the keys,
   *  passing the key and value as two separate arguments.
   *
   *  @tparam U the return type of the function `f`, used only for side effects
   *  @param f the function applied to each key and value in the map
   */
  override final def foreachEntry[U](f: (Long, T) => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachEntry(f); right.foreachEntry(f) }
    case LongMap.Tip(key, value) => f(key, value)
    case LongMap.Nil =>
  }

  /** Returns an iterator over the keys of this map, in unsigned order. */
  override def keysIterator: Iterator[Long] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapKeyIterator(this)
  }

  /** Loop over the keys of the map. The same as keys.foreach(f), but may
   *  be more efficient.
   *
   *  @tparam U the return type of the function `f`, used only for side effects
   *  @param f The loop body
   */
  final def foreachKey[U](f: Long => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachKey(f); right.foreachKey(f) }
    case LongMap.Tip(key, _) => f(key)
    case LongMap.Nil =>
  }

  /** Returns an iterator over the values of this map, in unsigned order of the
   *  corresponding keys.
   */
  override def valuesIterator: Iterator[T] = this match {
    case LongMap.Nil => Iterator.empty
    case _ => new LongMapValueIterator(this)
  }

  /** Loop over the values of the map. The same as values.foreach(f), but may
   *  be more efficient.
   *
   *  @tparam U the return type of the function `f`, used only for side effects
   *  @param f The loop body
   */
  final def foreachValue[U](f: T => U): Unit = this match {
    case LongMap.Bin(_, _, left, right) => { left.foreachValue(f); right.foreachValue(f) }
    case LongMap.Tip(_, value) => f(value)
    case LongMap.Nil =>
  }

  /** The name `"LongMap"`, used as the prefix in the string representation of this map. */
  override protected def className = "LongMap"

  /** Returns `true` if this map contains no bindings. The only empty `LongMap` is
   *  the shared `LongMap.Nil` instance, so this is a reference comparison.
   */
  override def isEmpty = this eq LongMap.Nil
  /** Returns 0 if this map is empty, otherwise -1, since computing the size
   *  requires traversing the whole tree.
   */
  override def knownSize: Int = if (isEmpty) 0 else super.knownSize
  /** Returns a map containing only the key/value pairs of this map that satisfy
   *  the predicate `f`.
   *
   *  Preserves sharing where possible: unchanged subtrees are reused, and if no
   *  binding is removed the result is this map itself.
   *
   *  @param f the predicate applied to each key/value pair
   */
  override def filter(f: ((Long, T)) => Boolean): LongMap[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) => {
      val (newleft, newright) = (left.filter(f), right.filter(f))
      if ((left eq newleft) && (right eq newright)) this
      else bin(prefix, mask, newleft, newright)
    }
    case LongMap.Tip(key, value) =>
      if (f((key, value))) this
      else LongMap.Nil
    case LongMap.Nil => LongMap.Nil
  }

  /** Returns a map with the same keys, where each value is replaced by the result
   *  of applying `f` to the key and its current value.
   *
   *  Unlike `map`, keys and tree structure are unchanged; only values are
   *  recomputed. Subtrees whose values are unchanged (by reference) are reused.
   *
   *  @tparam S the type of the values in the resulting map
   *  @param f the function computing the new value for each binding
   */
  override def transform[S](f: (Long, T) => S): LongMap[S] = this match {
    case b@LongMap.Bin(prefix, mask, left, right) => b.bin(left.transform(f), right.transform(f))
    case t@LongMap.Tip(key, value) => t.withValue(f(key, value))
    case LongMap.Nil => LongMap.Nil
  }

  /** Returns the number of bindings in this map, counted by traversing the whole
   *  tree, in time linear in the size of the map.
   */
  final override def size: Int = this match {
    case LongMap.Nil => 0
    case LongMap.Tip(_, _) => 1
    case LongMap.Bin(_, _, left, right) => left.size + right.size
  }

  /** Optionally returns the value associated with the given key.
   *
   *  @param key the key to look up
   *  @return `Some(value)` if this map binds `key` to `value`, `None` otherwise
   */
  @tailrec
  final def get(key: Long): Option[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left.get(key) else right.get(key)
    case LongMap.Tip(key2, value) => if (key == key2) Some(value) else None
    case LongMap.Nil => None
  }

  /** Returns the value associated with the given key, or `default` if the key is
   *  not present.
   *
   *  @tparam S the type of the result, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the value returned if `key` is not present; evaluated only in
   *                 that case
   */
  @tailrec
  final override def getOrElse[S >: T](key: Long, default: => S): S = this match {
    case LongMap.Nil => default
    case LongMap.Tip(key2, value) => if (key == key2) value else default
    case LongMap.Bin(prefix, mask, left, right) =>
      if (zero(key, mask)) left.getOrElse(key, default) else right.getOrElse(key, default)
  }

  /** Returns the value associated with the given key.
   *
   *  @param key the key to look up
   *  @return the value bound to `key`
   *  @throws IllegalArgumentException if `key` is not present; note that this
   *          differs from the `NoSuchElementException` thrown by most map
   *          implementations
   */
  @tailrec
  final override def apply(key: Long): T = this match {
    case LongMap.Bin(prefix, mask, left, right) => if (zero(key, mask)) left(key) else right(key)
    case LongMap.Tip(key2, value) => if (key == key2) value else throw new IllegalArgumentException("Key not found")
    case LongMap.Nil => throw new IllegalArgumentException("key not found")
  }

  /** Returns a map with the given key/value pair added, replacing any existing
   *  binding for that key.
   *
   *  @tparam S the type of the values in the resulting map, a supertype of this
   *            map's value type
   *  @param kv the key/value pair to add
   *  @return a map containing the bindings of this map and the binding `kv`
   */
  override def + [S >: T] (kv: (Long, S)): LongMap[S] = updated(kv._1, kv._2)

  /** Returns a map with `key` bound to `value`, replacing any existing binding
   *  for `key`.
   *
   *  @tparam S the type of the values in the resulting map, a supertype of this
   *            map's value type
   *  @param key the key to add or update
   *  @param value the value to associate with `key`
   *  @return a map containing the bindings of this map and the binding of `key`
   *          to `value`
   */
  override def updated[S >: T](key: Long, value: S): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, LongMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) LongMap.Bin(prefix, mask, left.updated(key, value), right)
      else LongMap.Bin(prefix, mask, left, right.updated(key, value))
    case LongMap.Tip(key2, value2) =>
      if (key == key2) LongMap.Tip(key, value)
      else join(key, LongMap.Tip(key, value), key2, this)
    case LongMap.Nil => LongMap.Tip(key, value)
  }

  /** Updates the map, using the provided function to resolve conflicts if the key is already present.
   *
   *  Equivalent to
   *  ```scala sc-name:updateWithExampleContext sc-hidden
   *  val map = LongMap(1L -> "one", 2L -> "two")
   *  val key = 2L
   *  val value = "deux"
   *  val f = (oldValue: String, newValue: String) => oldValue + "-" + newValue
   *  ```
   *  ```scala sc-compile-with:updateWithExampleContext
   *   map.get(key) match {
   *     case None => map.updated(key, value)
   *     case Some(oldvalue) => map.updated(key, f(oldvalue, value))
   *   }
   *  ```
   *
   *  @tparam S     The supertype of values in this `LongMap`.
   *  @param key    The key to update.
   *  @param value  The value to use if there is no conflict.
   *  @param f      The function used to resolve conflicts.
   *  @return       The updated map.
   */
  def updateWith[S >: T](key: Long, value: S, f: (T, S) => S): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) join(key, LongMap.Tip(key, value), prefix, this)
      else if (zero(key, mask)) LongMap.Bin(prefix, mask, left.updateWith(key, value, f), right)
      else LongMap.Bin(prefix, mask, left, right.updateWith(key, value, f))
    case LongMap.Tip(key2, value2) =>
      if (key == key2) LongMap.Tip(key, f(value2, value))
      else join(key, LongMap.Tip(key, value), key2, this)
    case LongMap.Nil => LongMap.Tip(key, value)
  }

  /** Returns a map without any binding for the given key.
   *
   *  @param key the key to remove
   *  @return a map containing the bindings of this map except for `key`
   */
  def removed(key: Long): LongMap[T] = this match {
    case LongMap.Bin(prefix, mask, left, right) =>
      if (!hasMatch(key, prefix, mask)) this
      else if (zero(key, mask)) bin(prefix, mask, left - key, right)
      else bin(prefix, mask, left, right - key)
    case LongMap.Tip(key2, _) =>
      if (key == key2) LongMap.Nil
      else this
    case LongMap.Nil => LongMap.Nil
  }

  /** A combined transform and filter function. Returns an `LongMap` such that
   *  for each `(key, value)` mapping in this map, if `f(key, value) == None`
   *  the map contains no mapping for key, and if `f(key, value)`.
   *
   *  @tparam S    The type of the values in the resulting `LongMap`.
   *  @param f     The transforming function.
   *  @return      The modified map.
   */
  def modifyOrRemove[S](f: (Long, T) => Option[S]): LongMap[S] = this match {
    case LongMap.Bin(prefix, mask, left, right) => {
      val newleft = left.modifyOrRemove(f)
      val newright = right.modifyOrRemove(f)
      if ((left eq newleft) && (right eq newright)) this.asInstanceOf[LongMap[S]]
      else bin(prefix, mask, newleft, newright)
    }
    case LongMap.Tip(key, value) => f(key, value) match {
      case None => LongMap.Nil
      case Some(value2) =>
        //hack to preserve sharing
        if (value.asInstanceOf[AnyRef] eq value2.asInstanceOf[AnyRef]) this.asInstanceOf[LongMap[S]]
        else LongMap.Tip(key, value2)
    }
    case LongMap.Nil => LongMap.Nil
  }

  /** Forms a union map with that map, using the combining function to resolve conflicts.
   *
   *  @tparam S      The type of values in `that`, a supertype of values in `this`.
   *  @param that    The map to form a union with.
   *  @param f       The function used to resolve conflicts between two mappings.
   *  @return        Union of `this` and `that`, with identical key conflicts resolved using the function `f`.
   */
  def unionWith[S >: T](that: LongMap[S], f: (Long, S, S) => S): LongMap[S] = (this, that) match{
    case (LongMap.Bin(p1, m1, l1, r1), that@(LongMap.Bin(p2, m2, l2, r2))) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) join(p1, this, p2, that)
        else if (zero(p2, m1)) LongMap.Bin(p1, m1, l1.unionWith(that, f), r1)
        else LongMap.Bin(p1, m1, l1, r1.unionWith(that, f))
      } else if (shorter(m2, m1)){
        if (!hasMatch(p1, p2, m2)) join(p1, this, p2, that)
        else if (zero(p1, m2)) LongMap.Bin(p2, m2, this.unionWith(l2, f), r2)
        else LongMap.Bin(p2, m2, l2, this.unionWith(r2, f))
      }
      else {
        if (p1 == p2) LongMap.Bin(p1, m1, l1.unionWith(l2,f), r1.unionWith(r2, f))
        else join(p1, this, p2, that)
      }
    case (LongMap.Tip(key, value), x) => x.updateWith(key, value, (x, y) => f(key, y, x))
    case (x, LongMap.Tip(key, value)) => x.updateWith[S](key, value, (x, y) => f(key, x, y))
    case (LongMap.Nil, x) => x
    case (x, LongMap.Nil) => x
  }

  /** Forms the intersection of these two maps with a combining function. The
   *  resulting map is a map that has only keys present in both maps and has
   *  values produced from the original mappings by combining them with `f`.
   *
   *  @tparam S      The type of values in `that`.
   *  @tparam R      The type of values in the resulting `LongMap`.
   *  @param that    The map to intersect with.
   *  @param f       The combining function.
   *  @return        Intersection of `this` and `that`, with values for identical keys produced by function `f`.
   */
  def intersectionWith[S, R](that: LongMap[S], f: (Long, T, S) => R): LongMap[R] = (this, that) match {
    case (LongMap.Bin(p1, m1, l1, r1), that@LongMap.Bin(p2, m2, l2, r2)) =>
      if (shorter(m1, m2)) {
        if (!hasMatch(p2, p1, m1)) LongMap.Nil
        else if (zero(p2, m1)) l1.intersectionWith(that, f)
        else r1.intersectionWith(that, f)
      } else if (m1 == m2) bin(p1, m1, l1.intersectionWith(l2, f), r1.intersectionWith(r2, f))
      else {
        if (!hasMatch(p1, p2, m2)) LongMap.Nil
        else if (zero(p1, m2)) this.intersectionWith(l2, f)
        else this.intersectionWith(r2, f)
      }
    case (LongMap.Tip(key, value), that) => that.get(key) match {
      case None => LongMap.Nil
      case Some(value2) => LongMap.Tip(key, f(key, value, value2))
    }
    case (_, LongMap.Tip(key, value)) => this.get(key) match {
      case None => LongMap.Nil
      case Some(value2) => LongMap.Tip(key, f(key, value2, value))
    }
    case (_, _) => LongMap.Nil
  }

  /** Left biased intersection. Returns the map that has all the same mappings as this but only for keys
   *  which are present in the other map.
   *
   *  @tparam R      The type of values in `that`.
   *  @param that    The map to intersect with.
   *  @return        A map with all the keys both in `this` and `that`, mapped to corresponding values from `this`.
   */
  def intersection[R](that: LongMap[R]): LongMap[T] =
    this.intersectionWith(that, (key: Long, value: T, value2: R) => value)

  /** Returns a map containing the bindings of this map and of `that`.
   *
   *  If a key is present in both maps, the value from `that` is retained.
   *
   *  @tparam S the type of the values in the resulting map, a supertype of this
   *            map's value type
   *  @param that the map to form a union with
   */
  def ++[S >: T](that: LongMap[S]) =
    this.unionWith[S](that, (key, x, y) => y)

  /** Returns the lowest key of this map in unsigned order.
   *
   *  @throws IllegalStateException if this map is empty
   */
  @tailrec
  final def firstKey: Long = this match {
    case LongMap.Bin(_, _, l, r) => l.firstKey
    case LongMap.Tip(k, v) => k
    case LongMap.Nil => throw new IllegalStateException("Empty set")
  }

  /** Returns the highest key of this map in unsigned order.
   *
   *  @throws IllegalStateException if this map is empty
   */
  @tailrec
  final def lastKey: Long = this match {
    case LongMap.Bin(_, _, l, r) => r.lastKey
    case LongMap.Tip(k , v) => k
    case LongMap.Nil => throw new IllegalStateException("Empty set")
  }

  /** Returns a map built from the results of applying `f` to each key/value pair
   *  of this map.
   *
   *  Unlike `transform`, `f` may change the keys, so the result is rebuilt from
   *  the transformed pairs. If `f` produces the same key for different pairs,
   *  bindings produced later (in unsigned order of the original keys) overwrite
   *  earlier ones.
   *
   *  @tparam V2 the type of the values in the resulting map
   *  @param f the function applied to each key/value pair
   *  @return a `LongMap` containing the transformed pairs
   */
  def map[V2](f: ((Long, T)) => (Long, V2)): LongMap[V2] = LongMap.from(new View.Map(coll, f))

  /** Returns a map built by applying `f` to each key/value pair of this map and
   *  collecting all the pairs it produces.
   *
   *  If the same key is produced more than once, bindings produced later
   *  overwrite earlier ones.
   *
   *  @tparam V2 the type of the values in the resulting map
   *  @param f the function returning a collection of key/value pairs for each
   *           pair of this map
   *  @return a `LongMap` containing all the pairs produced by `f`
   */
  def flatMap[V2](f: ((Long, T)) => IterableOnce[(Long, V2)]^): LongMap[V2] = LongMap.from(new View.FlatMap(coll, f))

  /** Returns a map containing the bindings of this map and of `that`.
   *
   *  Bindings from `that` overwrite bindings of this map with the same key.
   *
   *  @tparam V1 the type of the values in the resulting map, a supertype of this
   *             map's value type
   *  @param that the collection of key/value pairs to add
   *  @return a `LongMap` containing the combined bindings
   */
  override def concat[V1 >: T](that: scala.collection.IterableOnce[(Long, V1)]^): LongMap[V1] =
    super.concat(that).asInstanceOf[LongMap[V1]] // Already has correct type but not declared as such

  /** Alias for `concat`: returns a map containing the bindings of this map and
   *  of `that`, where bindings from `that` overwrite bindings of this map with
   *  the same key.
   *
   *  @tparam V1 the type of the values in the resulting map, a supertype of this
   *             map's value type
   *  @param that the collection of key/value pairs to add
   *  @return a `LongMap` containing the combined bindings
   */
  override def ++ [V1 >: T](that: scala.collection.IterableOnce[(Long, V1)]^): LongMap[V1] = concat(that)

  /** Returns a map built from the key/value pairs on which `pf` is defined,
   *  transformed by `pf`.
   *
   *  If `pf` produces the same key for different pairs, bindings produced later
   *  overwrite earlier ones.
   *
   *  @tparam V2 the type of the values in the resulting map
   *  @param pf the partial function applied to each pair on which it is defined
   *  @return a `LongMap` containing the transformed pairs
   */
  def collect[V2](pf: PartialFunction[(Long, T), (Long, V2)]): LongMap[V2] =
    strictOptimizedCollect(LongMap.newBuilder[V2], pf)

  /** Replaces this map with a serialization proxy during Java serialization. */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(LongMap.toFactory[T](LongMap), this)
}
