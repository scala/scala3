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
import scala.annotation.meta.companionClass
import scala.annotation.nowarn
import scala.collection.generic.DefaultSerializationProxy
import scala.language.implicitConversions

/** This class implements mutable maps with `AnyRef` keys based on a hash table with open addressing.
 *
 *  Basic map operations on single entries, including `contains` and `get`,
 *  are typically significantly faster with `AnyRefMap` than [[HashMap]].
 *  Note that numbers and characters are not handled specially in AnyRefMap;
 *  only plain `equals` and `hashCode` are used in comparisons.
 *
 *  Methods that traverse or regenerate the map, including `foreach` and `map`,
 *  are not in general faster than with `HashMap`.  The methods `foreachKey`,
 *  `foreachValue`, `mapValuesNow`, and `transformValues` are, however, faster
 *  than alternative ways to achieve the same functionality.
 *
 *  Maps with open addressing may become less efficient at lookup after
 *  repeated addition/removal of elements.  Although `AnyRefMap` makes a
 *  decent attempt to remain efficient regardless,  calling `repack`
 *  on a map that will no longer have elements removed but will be
 *  used heavily may save both time and storage space.
 *
 *  This map is not intended to contain more than 2<sup>29</sup> entries (approximately
 *  500 million).  The maximum capacity is 2<sup>30</sup>, but performance will degrade
 *  rapidly as 2<sup>30</sup> is approached.
 */
@(deprecated @companionClass)("Use `scala.collection.mutable.HashMap` instead for better performance.", since = "2.13.16")
class AnyRefMap[K <: AnyRef, V] private[collection] (defaultEntry: K -> V, initialBufferSize: Int, initBlank: Boolean)
  extends AbstractMap[K, V]
    with MapOps[K, V, Map, AnyRefMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, AnyRefMap[K, V]]
    with Serializable {

  import AnyRefMap._
  /** Creates a new empty `AnyRefMap` whose `apply` throws `NoSuchElementException` for missing keys. */
  def this() = this(AnyRefMap.exceptionDefault, 16, initBlank = true)

  /** Creates a new `AnyRefMap` that returns default values according to a supplied key-value mapping.
   *
   *  @param defaultEntry the function mapping keys to default values
   */
  def this(defaultEntry: K -> V) = this(defaultEntry, 16, initBlank = true)

  /** Creates a new `AnyRefMap` with an initial buffer of specified size.
   *
   *  An `AnyRefMap` can typically contain half as many elements as its buffer size
   *  before it requires resizing.
   *
   *  @param initialBufferSize the initial size of the internal buffer; the map can hold about half this many elements before resizing
   */
  def this(initialBufferSize: Int) = this(AnyRefMap.exceptionDefault, initialBufferSize, initBlank = true)

  /** Creates a new `AnyRefMap` with specified default values and initial buffer size.
   *
   *  @param defaultEntry the function mapping keys to default values
   *  @param initialBufferSize the initial size of the internal buffer; the map can hold about half this many elements before resizing
   */
  def this(defaultEntry: K -> V, initialBufferSize: Int) = this(defaultEntry, initialBufferSize, initBlank = true)

  private var mask = 0
  private var _size = 0
  private var _vacant = 0
  private var _hashes: Array[Int] = compiletime.uninitialized
  private var _keys: Array[AnyRef | Null] = compiletime.uninitialized
  private var _values: Array[AnyRef | Null] = compiletime.uninitialized

  if (initBlank) defaultInitialize(initialBufferSize)

  private def defaultInitialize(n: Int): Unit = {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _hashes = new Array[Int](mask+1)
    _keys = new Array[AnyRef | Null](mask+1)
    _values = new Array[AnyRef | Null](mask+1)
  }

  private[collection] def initializeTo(
    m: Int, sz: Int, vc: Int, hz: Array[Int], kz: Array[AnyRef | Null], vz: Array[AnyRef | Null]
  ): Unit = {
    mask = m; _size = sz; _vacant = vc; _hashes = hz; _keys = kz; _values = vz
  }

  /** Builds a new `AnyRefMap` containing the key/value pairs of the given collection.
   *
   *  If several pairs share a key, the last one wins.
   *
   *  @param coll the key/value pairs to build the map from
   *  @return a new `AnyRefMap` containing the pairs of `coll`
   */
  override protected def fromSpecific(coll: scala.collection.IterableOnce[(K, V)]^): AnyRefMap[K,V] = {
    var sz = coll.knownSize
    if(sz < 0) sz = 4
    val arm = new AnyRefMap[K, V](sz * 2)
    coll.iterator.foreach{ case (k,v) => arm(k) = v }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }
  /** Returns a new empty builder producing an `AnyRefMap` of the same key and value types. */
  override protected def newSpecificBuilder: Builder[(K, V), AnyRefMap[K,V]] = new AnyRefMapBuilder

  /** Returns the number of key/value pairs in this map. */
  override def size: Int = _size
  /** Returns `size`; the number of key/value pairs is always known. */
  override def knownSize: Int = size
  /** Returns `true` if this map contains no key/value pairs. */
  override def isEmpty: Boolean = _size == 0
  /** Returns a new empty `AnyRefMap` with the same default-value function as this map. */
  override def empty: AnyRefMap[K,V] = new AnyRefMap(defaultEntry)

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def hashOf(key: K): Int = {
    // Note: this method must not return 0 or Int.MinValue, as these indicate no element
    if (key eq null) 0x41081989
    else {
      val h = key.hashCode
      // Part of the MurmurHash3 32 bit finalizer
      val i = (h ^ (h >>> 16)) * 0x85EBCA6B
      val j = (i ^ (i >>> 13)) & 0x7FFFFFFF
      if (j==0) 0x41081989 else j
    }
  }

  private def seekEntry(h: Int, k: AnyRef): Int = {
    var e = h & mask
    var x = 0
    var g = 0
    val hashes = _hashes
    val keys = _keys
    while ({ g = hashes(e); g != 0}) {
      if (g == h && { val q = keys(e); (q eq k) || ((q ne null) && (q.equals(k))) }) return e
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    e | MissingBit
  }

  @`inline` private def seekEntryOrOpen(h: Int, k: AnyRef): Int = {
    var e = h & mask
    var x = 0
    var g = 0
    var o = -1
    while ({ g = _hashes(e); g != 0}) {
      if (g == h && { val q = _keys(e); (q eq k) || ((q ne null) && (q.equals(k))) }) return e
      else if (o == -1 && g+g == 0) o = e
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (o >= 0) o | MissVacant else e | MissingBit
  }

  /** Tests whether a key is present in this map.
   *
   *  @param key the key to look up; may be `null`
   *  @return `true` if `key` has an associated value, `false` otherwise
   */
  override def contains(key: K): Boolean = seekEntry(hashOf(key), key) >= 0

  /** Optionally returns the value associated with a key.
   *
   *  @param key the key to look up
   *  @return `Some` of the value associated with `key`, or `None` if `key` is not present
   */
  override def get(key: K): Option[V] = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) None else Some(_values(i).asInstanceOf[V])
  }

  /** Returns the value associated with a key, or a computed alternative if the key is not present.
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the alternative result; evaluated only if `key` is not present
   *  @return the value associated with `key`, or `default` if `key` is not present
   */
  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) default else _values(i).asInstanceOf[V]
  }

  /** Returns the value associated with a key, computing and storing a value if the key is not present.
   *
   *  If `key` is not present, evaluates `defaultValue`, adds the resulting entry to
   *  this map, and returns the result. `defaultValue` may itself query or modify this
   *  map; the entry for `key` is added after it completes.
   *
   *  @param key the key to look up
   *  @param defaultValue the value to store and return if `key` is not present; evaluated at most once
   *  @return the value associated with `key`, either pre-existing or newly stored
   */
  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    val h = hashOf(key)
    var i = seekEntryOrOpen(h, key)
    if (i < 0) {
      val value = {
        val ohs = _hashes
        val j = i & IndexMask
        val oh = ohs(j)
        val ans = defaultValue
        // Evaluating `defaultValue` may change the map
        //   - repack: the array is different
        //   - element added at `j`: since `i < 0`, the key was missing and `oh` is either 0 or MinValue.
        //     If `defaultValue` added an element at `j` then `_hashes(j)` must be different now.
        //     (`hashOf` never returns 0 or MinValue.)
        if (ohs.ne(_hashes) || oh != _hashes(j)) {
          i = seekEntryOrOpen(h, key)
          if (i >= 0) _size -= 1
        }
        ans
      }
      _size += 1
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key.asInstanceOf[AnyRef]
      _values(j) = value.asInstanceOf[AnyRef]
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
      value
    }
    else _values(i).asInstanceOf[V]
  }

  /** Retrieves the value associated with a key, or the default for that type if none exists
   *  (null for AnyRef, 0 for floats and integers).
   *
   *  Note: this is the fastest way to retrieve a value that may or
   *  may not exist, if the default null/zero is acceptable.  For key/value
   *  pairs that do exist, `apply` (i.e. `map(key)`) is equally fast.
   *
   *  @param key the key to look up
   *  @return the value associated with `key`, or `null` if the key is not present
   */
  def getOrNull(key: K): V | Null = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) null else _values(i).asInstanceOf[V]
  }

  /** Retrieves the value associated with a key.
   *  If the key does not exist in the map, the `defaultEntry` for that key
   *  will be returned instead; an exception will be thrown if no
   *  `defaultEntry` was supplied.
   *
   *  @param key the key to look up
   *  @return the value associated with `key`, or the result of `defaultEntry(key)` if the key is not present
   */
  override def apply(key: K): V = {
    val i = seekEntry(hashOf(key), key)
    if (i < 0) defaultEntry(key) else _values(i).asInstanceOf[V]
  }

  /** Defers to defaultEntry to find a default value for the key.  Throws an
   *  exception if no other default behavior was specified.
   *
   *  @param key the key to look up a default value for
   *  @return the default value supplied by `defaultEntry` for `key`, or throws if no `defaultEntry` was provided
   */
  override def default(key: K): V = defaultEntry(key)

  private def repack(newMask: Int): Unit = {
    val oh = _hashes
    val ok = _keys
    val ov = _values
    mask = newMask
    _hashes = new Array[Int](mask+1)
    _keys = new Array[AnyRef | Null](mask+1)
    _values = new Array[AnyRef | Null](mask+1)
    _vacant = 0
    var i = 0
    while (i < oh.length) {
      val h = oh(i)
      if (h+h != 0) {
        var e = h & mask
        var x = 0
        while (_hashes(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
        _hashes(e) = h
        _keys(e) = ok(i)
        _values(e) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `AnyRefMap` for maximum efficiency of lookup.
   *
   *  For maps that undergo a complex creation process with both addition and
   *  removal of keys, and then are used heavily with no further removal of
   *  elements, calling `repack` after the end of the creation can result in
   *  improved performance.  Repacking takes time proportional to the number
   *  of entries in the map.
   */
  def repack(): Unit = {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8*_size < m) m = m >>> 1
    repack(m)
  }

  /** Adds a key/value pair to this map, returning any value previously associated with the key.
   *
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return `Some` of the value previously associated with `key`, or `None` if `key` was not present
   */
  override def put(key: K, value: V): Option[V] = {
    val h = hashOf(key)
    val i = seekEntryOrOpen(h, key)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
      None
    }
    else {
      val ans = Some(_values(i).asInstanceOf[V])
      _hashes(i) = h
      _values(i) = value.asInstanceOf[AnyRef]
      ans
    }
  }

  /** Updates the map to include a new key-value pair.
   *
   *  This is the fastest way to add an entry to an `AnyRefMap`.
   *
   *  @param key the key to update
   *  @param value the new value to associate with `key`
   */
  override def update(key: K, value: V): Unit = {
    val h = hashOf(key)
    val i = seekEntryOrOpen(h, key)
    if (i < 0) {
      val j = i & IndexMask
      _hashes(j) = h
      _keys(j) = key
      _values(j) = value.asInstanceOf[AnyRef]
      _size += 1
      if ((i & VacantBit) != 0) _vacant -= 1
      else if (imbalanced) repack()
    }
    else {
      _hashes(i) = h
      _values(i) = value.asInstanceOf[AnyRef]
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  @deprecated("Use `addOne` or `update` instead; infix operations with an operand of multiple args will be deprecated", "2.13.3")
  def +=(key: K, value: V): this.type = { update(key, value); this }

  /** Adds a new key/value pair to this map and returns the map.
   *
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return this map after the entry has been added
   */
  @inline final def addOne(key: K, value: V): this.type = { update(key, value); this }

  /** Adds a key/value pair to this map and returns the map.
   *
   *  @param kv the key/value pair to add; any existing value for the same key is overwritten
   *  @return this map after the entry has been added
   */
  @inline override final def addOne(kv: (K, V)): this.type = { update(kv._1, kv._2); this }

  /** Removes a key from this map, and returns the map.
   *
   *  If `key` is present, its entry is removed and its slot is marked vacant;
   *  vacant slots are reclaimed on a later `repack`. Does nothing if `key` is
   *  not present.
   *
   *  @param key the key to remove
   *  @return this map after the removal
   */
  def subtractOne(key: K): this.type = {
    val i = seekEntry(hashOf(key), key)
    if (i >= 0) {
      _size -= 1
      _vacant += 1
      _hashes(i) = Int.MinValue
      _keys(i) = null
      _values(i) = null
    }
    this
  }

  /** Returns an iterator over the key/value pairs of this map. */
  def iterator: Iterator[(K, V)] = new AnyRefMapIterator[(K, V)] {
    protected def nextResult(k: K, v: V) = (k, v)
  }
  /** Returns an iterator over the keys of this map. */
  override def keysIterator: Iterator[K] = new AnyRefMapIterator[K] {
    protected def nextResult(k: K, v: V) = k
  }
  /** Returns an iterator over the values of this map. */
  override def valuesIterator: Iterator[V] = new AnyRefMapIterator[V] {
    protected def nextResult(k: K, v: V) = v
  }

  private abstract class AnyRefMapIterator[A] extends AbstractIterator[A] {
    private val hz = _hashes
    private val kz = _keys
    private val vz = _values

    private var index = 0

    /** Returns `true` if at least one entry remains, advancing past empty and vacated slots. */
    def hasNext: Boolean = index < hz.length && {
      var h = hz(index)
      while (h+h == 0) {
        index += 1
        if (index >= hz.length) return false
        h = hz(index)
      }
      true
    }

    /** Returns the result computed by `nextResult` for the next entry, and advances the iterator.
     *
     *  @return the result for the next entry
     *  @throws NoSuchElementException if no entries remain
     */
    def next(): A = {
      if (hasNext) {
        val ans = nextResult(kz(index).asInstanceOf[K], vz(index).asInstanceOf[V])
        index += 1
        ans
      }
      else throw new NoSuchElementException("next")
    }

    /** Computes this iterator's result for one entry.
     *
     *  @param k the entry's key
     *  @param v the entry's value
     *  @return the result to produce for the entry `k -> v`
     */
    protected def nextResult(k: K, v: V): A
  }


  /** Applies a function to each key/value pair of this map.
   *
   *  @tparam U the result type of the function; the results are discarded
   *  @param f the function to apply to each key/value pair
   */
  override def foreach[U](f: ((K,V)) => U): Unit = {
    var i = 0
    var e = _size
    while (e > 0) {
      while(i < _hashes.length && { val h = _hashes(i); h+h == 0 && i < _hashes.length}) i += 1
      if (i < _hashes.length) {
        f((_keys(i).asInstanceOf[K], _values(i).asInstanceOf[V]))
        i += 1
        e -= 1
      }
      else return
    }
  }

  /** Applies a function to each key/value pair of this map, passing key and value as separate arguments.
   *
   *  Unlike `foreach`, does not allocate a tuple per entry.
   *
   *  @tparam U the result type of the function; the results are discarded
   *  @param f the function to apply to each key and value
   */
  override def foreachEntry[U](f: (K,V) => U): Unit = {
    var i = 0
    var e = _size
    while (e > 0) {
      while(i < _hashes.length && { val h = _hashes(i); h+h == 0 && i < _hashes.length}) i += 1
      if (i < _hashes.length) {
        f(_keys(i).asInstanceOf[K], _values(i).asInstanceOf[V])
        i += 1
        e -= 1
      }
      else return
    }
  }

  /** Returns a copy of this map with the same entries, default-value function, and internal layout.
   *
   *  Later changes to the copy do not affect this map, and vice versa.
   */
  override def clone(): AnyRefMap[K, V] = {
    val hz = java.util.Arrays.copyOf(_hashes, _hashes.length)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = java.util.Arrays.copyOf(_values, _values.length)
    val arm = new AnyRefMap[K, V](defaultEntry, 1, initBlank = false)
    arm.initializeTo(mask, _size, _vacant, hz, kz, vz)
    arm
  }

  /** Returns a new `AnyRefMap` containing the entries of this map and one additional key/value pair.
   *
   *  This map is not modified.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param kv the key/value pair to add; it overrides any entry of this map with the same key
   *  @return a new `AnyRefMap` with the entries of this map plus `kv`
   */
  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def + [V1 >: V](kv: (K, V1)): AnyRefMap[K, V1] = AnyRefMap.from(new View.Appended(this, kv))

  /** Returns a new `AnyRefMap` containing the entries of this map and two or more additional key/value pairs.
   *
   *  This map is not modified. When keys coincide, later pairs override earlier
   *  ones and entries of this map.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param elem1 the first key/value pair to add
   *  @param elem2 the second key/value pair to add
   *  @param elems the remaining key/value pairs to add, if any
   *  @return a new `AnyRefMap` with the entries of this map plus all the given pairs
   */
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (K, V1), elem2: (K, V1), elems: (K, V1)*): AnyRefMap[K, V1]^{} = {
    // An empty capture annotation is needed in the result type to satisfy the overriding checker.
    val m = this + elem1 + elem2
    if(elems.isEmpty) m else m.concat(elems)
  }

  /** Returns a new `AnyRefMap` containing the entries of this map together with those of another collection.
   *
   *  This map is not modified. When keys coincide, pairs of `xs` override
   *  entries of this map, and later pairs of `xs` override earlier ones.
   *
   *  @tparam V2 the value type of the resulting map, a supertype of this map's value type
   *  @param xs the key/value pairs to add
   *  @return a new `AnyRefMap` with the entries of this map and of `xs`
   */
  override def concat[V2 >: V](xs: scala.collection.IterableOnce[(K, V2)]^): AnyRefMap[K, V2] = {
    val arm = clone().asInstanceOf[AnyRefMap[K, V2]]
    xs.iterator.foreach(kv => arm += kv)
    arm
  }

  /** Returns a new `AnyRefMap` containing the entries of this map together with those of another collection.
   *
   *  Alias for `concat`; this map is not modified.
   *
   *  @tparam V2 the value type of the resulting map, a supertype of this map's value type
   *  @param xs the key/value pairs to add
   *  @return a new `AnyRefMap` with the entries of this map and of `xs`
   */
  override def ++[V2 >: V](xs: scala.collection.IterableOnce[(K, V2)]^): AnyRefMap[K, V2] = concat(xs)

  /** Returns a copy of this map with one key/value pair added or replaced.
   *
   *  This map is not modified.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return a clone of this map with `key` mapped to `value`
   */
  @deprecated("Use m.clone().addOne(k,v) instead of m.updated(k, v)", "2.13.0")
  override def updated[V1 >: V](key: K, value: V1): AnyRefMap[K, V1] =
    clone().asInstanceOf[AnyRefMap[K, V1]].addOne(key, value)

  private def foreachElement[A,B](elems: Array[AnyRef | Null], f: A => B): Unit = {
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        f(elems(i).asInstanceOf[A])
      }
      i += 1
    }
  }

  /** Applies a function to all keys of this map.
   *
   *  @tparam A the result type of the function
   *  @param f the function to apply to each key
   */
  def foreachKey[A](f: K => A): Unit = foreachElement[K,A](_keys, f)

  /** Applies a function to all values of this map.
   *
   *  @tparam A the result type of the function
   *  @param f the function to apply to each value
   */
  def foreachValue[A](f: V => A): Unit = foreachElement[V,A](_values, f)

  /** Creates a new `AnyRefMap` with different values.
   *  Unlike `mapValues`, this method generates a new
   *  collection immediately.
   *
   *  @tparam V1 the new value type
   *  @param f the transformation function to apply to each value
   *  @return a new `AnyRefMap` with the same keys and values transformed by `f`
   */
  def mapValuesNow[V1](f: V => V1): AnyRefMap[K, V1] = {
    val arm = new AnyRefMap[K,V1](AnyRefMap.exceptionDefault, 1, initBlank = false)
    val hz = java.util.Arrays.copyOf(_hashes, _hashes.length)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = new Array[AnyRef | Null](_values.length)
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        vz(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    arm.initializeTo(mask, _size, _vacant, hz, kz, vz)
    arm
  }

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  @deprecated("Use transformValuesInPlace instead of transformValues", "2.13.0")
  @`inline` final def transformValues(f: V => V): this.type = transformValuesInPlace(f)

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   *
   *  @param f the transformation function to apply to each value
   *  @return this map after each stored value has been replaced by `f` applied to it
   */
  def transformValuesInPlace(f: V => V): this.type = {
    var i,j = 0
    while (i < _hashes.length & j < _size) {
      val h = _hashes(i)
      if (h+h != 0) {
        j += 1
        _values(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef]
      }
      i += 1
    }
    this
  }

  // The implicit dummy parameter is necessary to distinguish these methods from the base methods they overload (not override).
  // Previously, in Scala 2, f took `K with AnyRef` scala/bug#11035
  /** An overload of `map` which produces an `AnyRefMap`.
   *
   *  @tparam K2 the key type of the resulting map, must be a subtype of `AnyRef`
   *  @tparam V2 the value type of the resulting map
   *  @param f the function mapping each key-value pair to a new key-value pair; the resulting key must be an `AnyRef`
   *  @param dummy implicit parameter used to distinguish this overload from the inherited version after erasure
   *  @return a new `AnyRefMap` containing the key-value pairs produced by applying `f` to each entry of this map
   */
  def map[K2 <: AnyRef, V2](f: ((K, V)) => (K2, V2))(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    AnyRefMap.from(new View.Map(this, f))
  /** An overload of `flatMap` which produces an `AnyRefMap`.
   *
   *  @tparam K2 the key type of the resulting map, must be a subtype of `AnyRef`
   *  @tparam V2 the value type of the resulting map
   *  @param f the function mapping each key-value pair to a collection of new key-value pairs; the resulting keys must be `AnyRef`s
   *  @param dummy implicit parameter used to distinguish this overload from the inherited version after erasure
   *  @return a new `AnyRefMap` containing all the key-value pairs produced by applying `f` to each entry of this map
   */
  def flatMap[K2 <: AnyRef, V2](f: ((K, V)) => IterableOnce[(K2, V2)]^)(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    AnyRefMap.from(new View.FlatMap(this, f))
  /** An overload of `collect` which produces an `AnyRefMap`.
   *
   *  @tparam K2 the key type of the resulting map, must be a subtype of `AnyRef`
   *  @tparam V2 the value type of the resulting map
   *  @param pf the partial function mapping key-value pairs to new key-value pairs; the resulting key must be an `AnyRef`
   *  @param dummy implicit parameter used to distinguish this overload from the inherited version after erasure
   *  @return a new `AnyRefMap` containing the key-value pairs produced by applying `pf` to each entry on which it is defined
   */
  def collect[K2 <: AnyRef, V2](pf: PartialFunction[(K, V), (K2, V2)])(implicit dummy: DummyImplicit): AnyRefMap[K2, V2] =
    strictOptimizedCollect(AnyRefMap.newBuilder[K2, V2], pf)

  /** Removes all entries from this map, keeping the internal buffer at its current size. */
  override def clear(): Unit = {
    import java.util.Arrays.fill
    fill(_keys, null)
    fill(_values, null)
    fill(_hashes, 0)
    _size = 0
    _vacant = 0
  }

  /** Returns a serialization proxy that rebuilds this map on deserialization; called by Java serialization. */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(AnyRefMap.toFactory[K, V](AnyRefMap), this)

  /** Returns `"AnyRefMap"`, the prefix used in this map's string representation. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "AnyRefMap"
}

@deprecated("Use `scala.collection.mutable.HashMap` instead for better performance.", since = "2.13.16")
object AnyRefMap {
  private final val IndexMask  = 0x3FFFFFFF
  private final val MissingBit = 0x80000000
  private final val VacantBit  = 0x40000000
  private final val MissVacant = 0xC0000000

  private class ExceptionDefault extends (Any => Nothing) with Serializable {
    /** Always throws `NoSuchElementException` reporting the missing key; never returns normally.
     *
     *  @param k the key that was not found; rendered as `"(null)"` if `null`
     *  @throws NoSuchElementException always
     */
    def apply(k: Any): Nothing = throw new NoSuchElementException(if (k == null) "(null)" else k.toString)
  }
  private val exceptionDefault = new ExceptionDefault

  /** A builder for instances of `AnyRefMap`.
   *
   *  This builder can be reused to create multiple instances.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   */
  final class AnyRefMapBuilder[K <: AnyRef, V] extends ReusableBuilder[(K, V), AnyRefMap[K, V]] {
    private[collection] var elems: AnyRefMap[K, V] = new AnyRefMap[K, V]
    /** Adds a key/value pair to the map under construction.
     *
     *  @param entry the key/value pair to add; any existing value for the same key is overwritten
     *  @return this builder
     */
    def addOne(entry: (K, V)): this.type = {
      elems += entry
      this
    }
    /** Resets this builder by starting a fresh, empty map; previously returned maps are unaffected. */
    def clear(): Unit = elems = new AnyRefMap[K, V]
    /** Returns the map under construction. The map is returned directly, not copied, so additions made before the next `clear` also appear in the returned map. */
    def result(): AnyRefMap[K, V] = elems
    /** Returns the number of entries added since the last `clear`. */
    override def knownSize: Int = elems.knownSize
  }

  /** Creates a new `AnyRefMap` with zero or more key/value pairs.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param elems the key-value pairs to initialize the map with
   *  @return a new `AnyRefMap` containing the given key-value pairs
   */
  def apply[K <: AnyRef, V](elems: (K, V)*): AnyRefMap[K, V] = buildFromIterableOnce(elems)

  /** Creates a new empty builder for an `AnyRefMap`.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @return a new reusable builder producing an `AnyRefMap`
   */
  def newBuilder[K <: AnyRef, V]: ReusableBuilder[(K, V), AnyRefMap[K, V]] = new AnyRefMapBuilder[K, V]

  private def buildFromIterableOnce[K <: AnyRef, V](elems: IterableOnce[(K, V)]^): AnyRefMap[K, V] = {
    var sz = elems.knownSize
    if(sz < 0) sz = 4
    val arm = new AnyRefMap[K, V](sz * 2)
    elems.iterator.foreach{ case (k,v) => arm(k) = v }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }

  /** Creates a new empty `AnyRefMap`.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @return a new empty `AnyRefMap`
   */
  def empty[K <: AnyRef, V]: AnyRefMap[K, V] = new AnyRefMap[K, V]

  /** Creates a new empty `AnyRefMap` with the supplied default.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param default the function mapping keys to default values
   *  @return a new empty `AnyRefMap` that uses `default` to supply values for missing keys
   */
  def withDefault[K <: AnyRef, V](default: K -> V): AnyRefMap[K, V] = new AnyRefMap[K, V](default)

  /** Creates a new `AnyRefMap` from an existing source collection. A source collection
   *  which is already an `AnyRefMap` gets cloned.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param source Source collection
   *  @return a new `AnyRefMap` with the elements of `source`
   */
  def from[K <: AnyRef, V](source: IterableOnce[(K, V)]^): AnyRefMap[K, V] = source match {
    case source: AnyRefMap[?, ?] => source.clone().asInstanceOf[AnyRefMap[K, V]]
    case _ => buildFromIterableOnce(source)
  }

  /** Creates a new `AnyRefMap` from arrays of keys and values.
   *  Equivalent to but more efficient than `AnyRefMap((keys zip values): _*)`.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param keys the array of keys
   *  @param values the array of values, paired positionally with `keys`
   *  @return a new `AnyRefMap` containing entries `keys(i) -> values(i)` for indices up to the shorter array's length
   */
  def fromZip[K <: AnyRef, V](keys: Array[K], values: Array[V]): AnyRefMap[K, V] = {
    val sz = math.min(keys.length, values.length)
    val arm = new AnyRefMap[K, V](sz * 2)
    var i = 0
    while (i < sz) { arm(keys(i)) = values(i); i += 1 }
    if (arm.size < (sz>>3)) arm.repack()
    arm
  }

  /** Creates a new `AnyRefMap` from keys and values.
   *  Equivalent to but more efficient than `AnyRefMap((keys zip values): _*)`.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param keys the collection of keys
   *  @param values the collection of values, paired positionally with `keys`
   *  @return a new `AnyRefMap` containing the entries formed by pairing each key with the value at the same position, up to the shorter collection's size
   */
  def fromZip[K <: AnyRef, V](keys: Iterable[K], values: Iterable[V]): AnyRefMap[K, V] = {
    val sz = math.min(keys.size, values.size)
    val arm = new AnyRefMap[K, V](sz * 2)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) arm(ki.next()) = vi.next()
    if (arm.size < (sz >> 3)) arm.repack()
    arm
  }

  /** Implicitly converts this companion object to a `Factory`, so it can be passed
   *  where a factory of `AnyRefMap`s is expected, for example to `to(AnyRefMap)`.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param dummy this companion object; its value is never used
   *  @return a `Factory` that builds an `AnyRefMap` from key/value pairs
   */
  implicit def toFactory[K <: AnyRef, V](dummy: AnyRefMap.type): Factory[(K, V), AnyRefMap[K, V]] = ToFactory.asInstanceOf[Factory[(K, V), AnyRefMap[K, V]]]

  @SerialVersionUID(3L)
  private object ToFactory extends Factory[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] with Serializable {
    /** Builds an `AnyRefMap` from a collection of key/value pairs.
     *
     *  @param it the key/value pairs
     *  @return a new `AnyRefMap` containing the pairs of `it`
     */
    def fromSpecific(it: IterableOnce[(AnyRef, AnyRef)]^): AnyRefMap[AnyRef, AnyRef] = AnyRefMap.from[AnyRef, AnyRef](it)
    /** Returns a new empty builder for an `AnyRefMap`. */
    def newBuilder: Builder[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] = AnyRefMap.newBuilder[AnyRef, AnyRef]
  }

  /** Implicitly converts this companion object to a `BuildFrom`, so it can be passed
   *  where a `BuildFrom` producing `AnyRefMap`s is expected.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @param factory this companion object; its value is never used
   *  @return a `BuildFrom` that builds an `AnyRefMap` from key/value pairs, ignoring the source collection
   */
  implicit def toBuildFrom[K <: AnyRef, V](factory: AnyRefMap.type): BuildFrom[Any, (K, V), AnyRefMap[K, V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (K, V), AnyRefMap[K, V]]]
  private object ToBuildFrom extends BuildFrom[Any, (AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] {
    /** Builds an `AnyRefMap` from a collection of key/value pairs.
     *
     *  @param from the source collection; never used
     *  @param it the key/value pairs
     *  @return a new `AnyRefMap` containing the pairs of `it`
     */
    def fromSpecific(from: Any)(it: IterableOnce[(AnyRef, AnyRef)]^): AnyRefMap[AnyRef, AnyRef] = AnyRefMap.from(it)
    /** Returns a new empty builder for an `AnyRefMap`.
     *
     *  @param from the source collection; never used
     *  @return a new reusable builder producing an `AnyRefMap`
     */
    def newBuilder(from: Any): ReusableBuilder[(AnyRef, AnyRef), AnyRefMap[AnyRef, AnyRef]] = AnyRefMap.newBuilder[AnyRef, AnyRef]
  }

  /** An implicit `Factory` for `AnyRefMap`s, for APIs that look one up implicitly.
   *
   *  @tparam K the type of keys, must be a subtype of `AnyRef`
   *  @tparam V the type of values
   *  @return a `Factory` that builds an `AnyRefMap` from key/value pairs
   */
  implicit def iterableFactory[K <: AnyRef, V]: Factory[(K, V), AnyRefMap[K, V]] = toFactory[K, V](this)
  /** An implicit `BuildFrom` that builds an `AnyRefMap` when the source collection is an `AnyRefMap`.
   *
   *  @tparam K the type of keys of the resulting map, must be a subtype of `AnyRef`
   *  @tparam V the type of values of the resulting map
   *  @return a `BuildFrom` that builds an `AnyRefMap` from key/value pairs
   */
  implicit def buildFromAnyRefMap[K <: AnyRef, V]: BuildFrom[AnyRefMap[?, ?], (K, V), AnyRefMap[K, V]] = toBuildFrom(this)
}

