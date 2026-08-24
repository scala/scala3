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
package mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.collection.generic.DefaultSerializationProxy
import scala.language.implicitConversions

/** This class implements mutable maps with `Long` keys based on a hash table with open addressing.
 *
 *  Basic map operations on single entries, including `contains` and `get`,
 *  are typically substantially faster with `LongMap` than [[HashMap]].  Methods
 *  that act on the whole map,  including `foreach` and `map` are not in
 *  general expected to be faster than with a generic map, save for those
 *  that take particular advantage of the internal structure of the map:
 *  `foreachKey`, `foreachValue`, `mapValuesNow`, and `transformValues`.
 *
 *  Maps with open addressing may become less efficient at lookup after
 *  repeated addition/removal of elements.  Although `LongMap` makes a
 *  decent attempt to remain efficient regardless,  calling `repack`
 *  on a map that will no longer have elements removed but will be
 *  used heavily may save both time and storage space.
 *
 *  This map is not intended to contain more than 2<sup>29</sup> entries (approximately
 *  500 million).  The maximum capacity is 2<sup>30</sup>, but performance will degrade
 *  rapidly as 2<sup>30</sup> is approached.
 *
 *  @tparam V the type of the values stored in this map
 */
final class LongMap[V] private[collection] (defaultEntry: Long -> V, initialBufferSize: Int, initBlank: Boolean)
  extends AbstractMap[Long, V]
    with MapOps[Long, V, Map, LongMap[V]]
    with StrictOptimizedIterableOps[(Long, V), Iterable, LongMap[V]]
    with Serializable {
  import LongMap._

  /** Creates a new empty `LongMap` whose `apply` throws `NoSuchElementException` for missing keys. */
  def this() = this(LongMap.exceptionDefault, 16, initBlank = true)

  // TODO: override clear() with an optimization more tailored for efficiency.
  /** Builds a new `LongMap` containing the key/value pairs of the given collection.
   *
   *  If several pairs share a key, the last one wins.
   *
   *  @param coll the key/value pairs to build the map from
   *  @return a new `LongMap` containing the pairs of `coll`
   */
  override protected def fromSpecific(coll: scala.collection.IterableOnce[(Long, V)]^): LongMap[V] = {
    //TODO should this be the default implementation of this method in StrictOptimizedIterableOps?
    val b = newSpecificBuilder
    b.sizeHint(coll)
    b.addAll(coll)
    b.result()
  }
  /** Returns a new empty builder producing a `LongMap` of the same value type. */
  override protected def newSpecificBuilder: Builder[(Long, V),LongMap[V]] = new GrowableBuilder(LongMap.empty[V])

  /** Creates a new `LongMap` that returns default values according to a supplied key-value mapping.
   *
   *  @param defaultEntry the function mapping keys to default values
   */
  def this(defaultEntry: Long -> V) = this(defaultEntry, 16, initBlank = true)

  /** Creates a new `LongMap` with an initial buffer of specified size.
   *
   *  A LongMap can typically contain half as many elements as its buffer size
   *  before it requires resizing.
   *
   *  @param initialBufferSize the initial size of the internal buffer; the map can hold about half this many elements before resizing
   */
  def this(initialBufferSize: Int) = this(LongMap.exceptionDefault, initialBufferSize, initBlank = true)

  /** Creates a new `LongMap` with specified default values and initial buffer size.
   *
   *  @param defaultEntry the function mapping keys to default values
   *  @param initialBufferSize the initial size of the internal buffer; the map can hold about half this many elements before resizing
   */
  def this(defaultEntry: Long -> V,  initialBufferSize: Int) = this(defaultEntry,  initialBufferSize,  initBlank = true)

  private var mask = 0
  private var extraKeys: Int = 0
  @annotation.stableNull private var zeroValue: AnyRef | Null = null
  @annotation.stableNull private var minValue: AnyRef | Null = null
  private var _size = 0
  private var _vacant = 0
  private var _keys: Array[Long] = compiletime.uninitialized
  private var _values: Array[AnyRef | Null] = compiletime.uninitialized

  if (initBlank) defaultInitialize(initialBufferSize)

  private def defaultInitialize(n: Int) = {
    mask =
      if (n<0) 0x7
      else (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(n-1))) - 1) & 0x3FFFFFFF) | 0x7
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef | Null](mask+1)
  }

  private[collection] def initializeTo(
                                        m: Int, ek: Int, zv: AnyRef | Null, mv: AnyRef | Null, sz: Int, vc: Int, kz: Array[Long], vz: Array[AnyRef | Null]
                                      ): Unit = {
    mask = m; extraKeys = ek; zeroValue = zv; minValue = mv; _size = sz; _vacant = vc; _keys = kz; _values = vz
  }

  /** Returns the number of key/value pairs in this map, counting any entries for the keys `0` and `Long.MinValue`, which are stored outside the hash table. */
  override def size: Int = _size + (extraKeys+1)/2
  /** Returns `size`; the number of key/value pairs is always known. */
  override def knownSize: Int = size
  /** Returns `true` if this map contains no key/value pairs. */
  override def isEmpty: Boolean = size == 0
  /** Returns a new empty `LongMap`. Unlike this map, the result carries no default-value function: its `apply` throws `NoSuchElementException` for missing keys. */
  override def empty: LongMap[V] = new LongMap()

  private def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private def toIndex(k: Long): Int = {
    // Part of the MurmurHash3 32 bit finalizer
    val h = ((k ^ (k >>> 32)) & 0xFFFFFFFFL).toInt
    val x = (h ^ (h >>> 16)) * 0x85EBCA6B
    (x ^ (x >>> 13)) & mask
  }

  private def seekEmpty(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    while (_keys(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e
  }

  private def seekEntry(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q != 0}) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e | MissingBit
  }

  private def seekEntryOrOpen(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q+q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (q == 0) return e | MissingBit
    val o = e | MissVacant
    while ({ q = _keys(e); if (q==k) return e; q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    o
  }

  /** Tests whether a key is present in this map.
   *
   *  @param key the key to look up
   *  @return `true` if `key` has an associated value, `false` otherwise
   */
  override def contains(key: Long): Boolean = {
    if (key == -key) (((key>>>63).toInt+1) & extraKeys) != 0
    else seekEntry(key) >= 0
  }

  /** Optionally returns the value associated with a key.
   *
   *  @param key the key to look up
   *  @return `Some` of the value associated with `key`, or `None` if `key` is not present
   */
  override def get(key: Long): Option[V] = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) None
      else if (key == 0) Some(zeroValue.asInstanceOf[V])
      else Some(minValue.asInstanceOf[V])
    }
    else {
      val i = seekEntry(key)
      if (i < 0) None else Some(_values(i).asInstanceOf[V])
    }
  }

  /** Returns the value associated with a key, or a computed alternative if the key is not present.
   *
   *  @tparam V1 the result type, a supertype of this map's value type
   *  @param key the key to look up
   *  @param default the alternative result; evaluated only if `key` is not present
   *  @return the value associated with `key`, or `default` if `key` is not present
   */
  override def getOrElse[V1 >: V](key: Long, default: => V1): V1 = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) default
      else if (key == 0) zeroValue.asInstanceOf[V1]
      else minValue.asInstanceOf[V1]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) default else _values(i).asInstanceOf[V1]
    }
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
  override def getOrElseUpdate(key: Long, defaultValue: => V): V = {
    if (key == -key) {
      val kbits = (key>>>63).toInt + 1
      if ((kbits & extraKeys) == 0) {
        val value = defaultValue
        extraKeys |= kbits
        if (key == 0) zeroValue = value.asInstanceOf[AnyRef]
        else minValue = value.asInstanceOf[AnyRef]
        value
      }
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      var i = seekEntryOrOpen(key)
      if (i < 0) {
        val value = {
          val oks = _keys
          val j = i & IndexMask
          val ok = oks(j)
          val ans = defaultValue
          // Evaluating `defaultValue` may change the map
          //   - repack: the array is different
          //   - element added at `j`: since `i < 0`, the key was missing and `ok` is either 0 or MinValue.
          //     If `defaultValue` added an element at `j` then `_keys(j)` must be different now.
          //     (`_keys` never contains 0 or MinValue.)
          if (oks.ne(_keys) || ok != _keys(j)) {
            i = seekEntryOrOpen(key)
            if (i >= 0) _size -= 1
          }
          ans
        }
        _size += 1
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        value
      }
      else _values(i).asInstanceOf[V]
    }
  }

  /** Retrieves the value associated with a key, or the default for that type if none exists
   *  (null for AnyRef, 0 for floats and integers).
   *
   *  Note: this is the fastest way to retrieve a value that may or
   *  may not exist, if the default null/zero is acceptable.  For key/value
   *  pairs that do exist,  `apply` (i.e. `map(key)`) is equally fast.
   *
   *  @param key the key to look up
   *  @return the value associated with `key`, or `null` if not present
   */
  def getOrNull(key: Long): V | Null = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) null
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) null else _values(i).asInstanceOf[V | Null]
    }
  }

  /** Retrieves the value associated with a key.
   *  If the key does not exist in the map, the `defaultEntry` for that key
   *  will be returned instead.
   *
   *  @param key the key to look up
   *  @return the value associated with `key`, or the result of `defaultEntry(key)` if not present
   */
  override def apply(key: Long): V = {
    if (key == -key) {
      if ((((key>>>63).toInt+1) & extraKeys) == 0) defaultEntry(key)
      else if (key == 0) zeroValue.asInstanceOf[V]
      else minValue.asInstanceOf[V]
    }
    else {
      val i = seekEntry(key)
      if (i < 0) defaultEntry(key) else _values(i).asInstanceOf[V]
    }
  }

  /** The user-supplied default value for the key.  Throws an exception
   *  if no other default behavior was specified.
   *
   *  @param key the key whose default value is requested
   */
  override def default(key: Long) = defaultEntry(key)

  private def repack(newMask: Int): Unit = {
    val ok = _keys
    val ov = _values
    mask = newMask
    _keys = new Array[Long](mask+1)
    _values = new Array[AnyRef | Null](mask+1)
    _vacant = 0
    var i = 0
    while (i < ok.length) {
      val k = ok(i)
      if (k != -k) {
        val j = seekEmpty(k)
        _keys(j) = k
        _values(j) = ov(i)
      }
      i += 1
    }
  }

  /** Repacks the contents of this `LongMap` for maximum efficiency of lookup.
   *
   *  For maps that undergo a complex creation process with both addition and
   *  removal of keys, and then are used heavily with no further removal of
   *  elements, calling `repack` after the end of the creation can result in
   *  improved performance.  Repacking takes time proportional to the number
   *  of entries in the map.
   */
  def repack(): Unit = repack(repackMask(mask, _size = _size, _vacant = _vacant))

  /** Adds a key/value pair to this map, returning any value previously associated with the key.
   *
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return `Some` of the value previously associated with `key`, or `None` if `key` was not present
   */
  override def put(key: Long, value: V): Option[V] = {
    if (key == -key) {
      if (key == 0) {
        val ans = if ((extraKeys&1) == 1) Some(zeroValue.asInstanceOf[V]) else None
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
        ans
      }
      else {
        val ans = if ((extraKeys&2) == 2) Some(minValue.asInstanceOf[V]) else None
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
        ans
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
        None
      }
      else {
        val ans = Some(_values(i).asInstanceOf[V])
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
        ans
      }
    }
  }

  /** Updates the map to include a new key-value pair.
   *
   *  This is the fastest way to add an entry to a `LongMap`.
   *
   *  @param key the key of the entry to update
   *  @param value the new value to associate with `key`
   */
  override def update(key: Long, value: V): Unit = {
    if (key == -key) {
      if (key == 0) {
        zeroValue = value.asInstanceOf[AnyRef]
        extraKeys |= 1
      }
      else {
        minValue = value.asInstanceOf[AnyRef]
        extraKeys |= 2
      }
    }
    else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value.asInstanceOf[AnyRef]
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack()
      }
      else {
        _keys(i) = key
        _values(i) = value.asInstanceOf[AnyRef]
      }
    }
  }

  /** Adds a new key/value pair to this map and returns the map. */
  @deprecated("Use `addOne` or `update` instead; infix operations with an operand of multiple args will be deprecated", "2.13.3")
  def +=(key: Long, value: V): this.type = { update(key, value); this }

  /** Adds a new key/value pair to this map and returns the map.
   *
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return this map after the entry has been added
   */
  @inline final def addOne(key: Long, value: V): this.type = { update(key, value); this }

  /** Adds a key/value pair to this map and returns the map.
   *
   *  @param kv the key/value pair to add; any existing value for the same key is overwritten
   *  @return this map after the entry has been added
   */
  @inline override final def addOne(kv: (Long, V)): this.type = { update(kv._1, kv._2); this }

  /** Removes a key from this map, and returns the map.
   *
   *  For the keys `0` and `Long.MinValue` the dedicated slot is simply emptied.
   *  For any other key that is present, the entry is removed and its hash table
   *  slot is marked vacant; vacant slots are reclaimed on a later `repack`.
   *  Does nothing if `key` is not present.
   *
   *  @param key the key to remove
   *  @return this map after the removal
   */
  def subtractOne(key: Long): this.type = {
    if (key == -key) {
      if (key == 0L) {
        extraKeys &= 0x2
        zeroValue = null
      }
      else {
        extraKeys &= 0x1
        minValue = null
      }
    }
    else {
      val i = seekEntry(key)
      if (i >= 0) {
        _size -= 1
        _vacant += 1
        _keys(i) = Long.MinValue
        _values(i) = null
      }
    }
    this
  }

  /** Returns an iterator over the key/value pairs of this map.
   *
   *  Entries for the keys `0` and `Long.MinValue`, if present, are produced
   *  first, in that order, followed by the remaining entries.
   */
  def iterator: Iterator[(Long, V)] = new AbstractIterator[(Long, V)] {
    private val kz = _keys
    private val vz = _values

    private var nextPair: (Long, V) | Null =
      if (extraKeys==0) null
      else if ((extraKeys&1)==1) (0L, zeroValue.asInstanceOf[V])
      else (Long.MinValue, minValue.asInstanceOf[V])

    private var anotherPair: (Long, V) | Null =
      if (extraKeys==3) (Long.MinValue, minValue.asInstanceOf[V])
      else null

    private var index = 0

    def hasNext: Boolean = nextPair != null || (index < kz.length && {
      var q = kz(index)
      while (q == -q) {
        index += 1
        if (index >= kz.length) return false
        q = kz(index)
      }
      nextPair = (kz(index), vz(index).asInstanceOf[V])
      index += 1
      true
    })
    def next() = {
      if (nextPair == null && !hasNext) throw new NoSuchElementException("next")
      val ans = nextPair
      if (anotherPair != null) {
        nextPair = anotherPair
        anotherPair = null
      }
      else nextPair = null
      ans.nn
    }
  }

  // TODO PERF override these for efficiency. See immutable.LongMap for how to organize the code.
  /** Returns an iterator over the keys of this map, in the same order as `iterator`. */
  override def keysIterator: Iterator[Long] = super.keysIterator
  /** Returns an iterator over the values of this map, in the same order as `iterator`. */
  override def valuesIterator: Iterator[V] = super.valuesIterator

  /** Applies a function to each key/value pair of this map.
   *
   *  Entries for the keys `0` and `Long.MinValue`, if present, are visited
   *  first, in that order.
   *
   *  @tparam U the result type of the function; the results are discarded
   *  @param f the function to apply to each key/value pair
   */
  override def foreach[U](f: ((Long,V)) => U): Unit = {
    if ((extraKeys & 1) == 1) f((0L, zeroValue.asInstanceOf[V]))
    if ((extraKeys & 2) == 2) f((Long.MinValue, minValue.asInstanceOf[V]))
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f((k, _values(i).asInstanceOf[V]))
      }
      i += 1
    }
  }

  /** Applies a function to each key/value pair of this map, passing key and value as separate arguments.
   *
   *  Unlike `foreach`, does not allocate a tuple per entry. Entries for the
   *  keys `0` and `Long.MinValue`, if present, are visited first, in that order.
   *
   *  @tparam U the result type of the function; the results are discarded
   *  @param f the function to apply to each key and value
   */
  override def foreachEntry[U](f: (Long,V) => U): Unit = {
    if ((extraKeys & 1) == 1) f(0L, zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(Long.MinValue, minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k, _values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  /** Returns a copy of this map with the same entries, default-value function, and internal layout.
   *
   *  Later changes to the copy do not affect this map, and vice versa.
   */
  override def clone(): LongMap[V] = {
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = java.util.Arrays.copyOf(_values,  _values.length)
    val lm = new LongMap[V](defaultEntry, 1, initBlank = false)
    lm.initializeTo(mask, extraKeys, zeroValue, minValue, _size, _vacant, kz,  vz)
    lm
  }

  /** Returns a new `LongMap` containing the entries of this map and one additional key/value pair.
   *
   *  This map is not modified.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param kv the key/value pair to add; it overrides any entry of this map with the same key
   *  @return a new `LongMap` with the entries of this map plus `kv`
   */
  @deprecated("Consider requiring an immutable Map or fall back to Map.concat", "2.13.0")
  override def +[V1 >: V](kv: (Long, V1)): LongMap[V1] = {
    val lm = clone().asInstanceOf[LongMap[V1]]
    lm += kv
    lm
  }

  /** Returns a new `LongMap` containing the entries of this map and two or more additional key/value pairs.
   *
   *  This map is not modified. When keys coincide, later pairs override earlier
   *  ones and entries of this map.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param elem1 the first key/value pair to add
   *  @param elem2 the second key/value pair to add
   *  @param elems the remaining key/value pairs to add, if any
   *  @return a new `LongMap` with the entries of this map plus all the given pairs
   */
  @deprecated("Use ++ with an explicit collection argument instead of + with varargs", "2.13.0")
  override def + [V1 >: V](elem1: (Long, V1), elem2: (Long, V1), elems: (Long, V1)*): LongMap[V1]^{} = {
    // TODO: An empty capture annotation is needed in the result type to satisfy the overriding checker.
    val m = this + elem1 + elem2
    if(elems.isEmpty) m else m.concat(elems)
  }

  /** Returns a new `LongMap` containing the entries of this map together with those of another collection.
   *
   *  This map is not modified. When keys coincide, pairs of `xs` override
   *  entries of this map, and later pairs of `xs` override earlier ones.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param xs the key/value pairs to add
   *  @return a new `LongMap` with the entries of this map and of `xs`
   */
  override def concat[V1 >: V](xs: scala.collection.IterableOnce[(Long, V1)]^): LongMap[V1] = {
    val lm = clone().asInstanceOf[LongMap[V1]]
    xs.iterator.foreach(kv => lm += kv)
    lm
  }

  /** Returns a new `LongMap` containing the entries of this map together with those of another collection.
   *
   *  Alias for `concat`; this map is not modified.
   *
   *  @tparam V1 the value type of the resulting map, a supertype of this map's value type
   *  @param xs the key/value pairs to add
   *  @return a new `LongMap` with the entries of this map and of `xs`
   */
  override def ++ [V1 >: V](xs: scala.collection.IterableOnce[(Long, V1)]^): LongMap[V1] = concat(xs)

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
  override def updated[V1 >: V](key: Long, value: V1): LongMap[V1] =
    clone().asInstanceOf[LongMap[V1]].addOne(key, value)

  /** Applies a function to all keys of this map.
   *
   *  @tparam A the result type of the function
   *  @param f the function to apply to each key
   */
  def foreachKey[A](f: Long => A): Unit = {
    if ((extraKeys & 1) == 1) f(0L)
    if ((extraKeys & 2) == 2) f(Long.MinValue)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(k)
      }
      i += 1
    }
  }

  /** Applies a function to all values of this map.
   *
   *  @tparam A the result type of the function
   *  @param f the function to apply to each value
   */
  def foreachValue[A](f: V => A): Unit = {
    if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V])
    if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V])
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        f(_values(i).asInstanceOf[V])
      }
      i += 1
    }
  }

  /** Creates a new `LongMap` with different values.
   *  Unlike `mapValues`, this method generates a new
   *  collection immediately.
   *
   *  @tparam V1 the type of the values in the resulting map
   *  @param f the transformation function applied to each value
   *  @return a new `LongMap` with the same keys, where each value is the result of applying `f` to the corresponding value in this map
   */
  def mapValuesNow[V1](f: V => V1): LongMap[V1] = {
    val zv = if ((extraKeys & 1) == 1) f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef | Null] else null
    val mv = if ((extraKeys & 2) == 2) f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef | Null] else null
    val lm = new LongMap[V1](LongMap.exceptionDefault,  1,  initBlank = false)
    val kz = java.util.Arrays.copyOf(_keys, _keys.length)
    val vz = new Array[AnyRef | Null](_values.length)
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        vz(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef | Null]
      }
      i += 1
    }
    lm.initializeTo(mask, extraKeys, zv, mv, _size, _vacant, kz, vz)
    lm
  }

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   */
  @deprecated("Use transformValuesInPlace instead of transformValues", "2.13.0")
  @`inline` final def transformValues(f: V => V): this.type = transformValuesInPlace(f)

  /** Applies a transformation function to all values stored in this map.
   *  Note: the default, if any,  is not transformed.
   *
   *  @param f the transformation function applied to each value
   *  @return this map after each value has been replaced by `f` applied to it
   */
  def transformValuesInPlace(f: V => V): this.type = {
    if ((extraKeys & 1) == 1) zeroValue = f(zeroValue.asInstanceOf[V]).asInstanceOf[AnyRef | Null]
    if ((extraKeys & 2) == 2) minValue = f(minValue.asInstanceOf[V]).asInstanceOf[AnyRef | Null]
    var i,j = 0
    while (i < _keys.length & j < _size) {
      val k = _keys(i)
      if (k != -k) {
        j += 1
        _values(i) = f(_values(i).asInstanceOf[V]).asInstanceOf[AnyRef | Null]
      }
      i += 1
    }
    this
  }

  /** An overload of `map` which produces a `LongMap`.
   *
   *  @tparam V2 the value type of the resulting map
   *  @param f the mapping function
   *  @return a new `LongMap` containing the key/value pairs produced by applying `f` to each entry of this map
   */
  def map[V2](f: ((Long, V)) => (Long, V2)): LongMap[V2] = LongMap.from(new View.Map(coll, f))

  /** An overload of `flatMap` which produces a `LongMap`.
   *
   *  @tparam V2 the value type of the resulting map
   *  @param f the mapping function
   *  @return a new `LongMap` containing the concatenation of the key/value pairs produced by applying `f` to each entry of this map
   */
  def flatMap[V2](f: ((Long, V)) => IterableOnce[(Long, V2)]^): LongMap[V2] = LongMap.from(new View.FlatMap(coll, f))

  /** An overload of `collect` which produces a `LongMap`.
   *
   *  @tparam V2 the value type of the resulting map
   *  @param pf the partial function to apply to matching elements
   *  @return a new `LongMap` containing the key/value pairs produced by `pf` for the entries on which it is defined
   */
  def collect[V2](pf: PartialFunction[(Long, V), (Long, V2)]): LongMap[V2] =
    strictOptimizedCollect(LongMap.newBuilder[V2], pf)

  /** Returns a serialization proxy that rebuilds this map on deserialization; called by Java serialization. */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(LongMap.toFactory[V](LongMap), this)

  /** Returns `"LongMap"`, the name used in this map's string representation. */
  override protected def className = "LongMap"
}

object LongMap {
  private final val IndexMask  = 0x3FFF_FFFF
  private final val MissingBit = 0x8000_0000
  private final val VacantBit  = 0x4000_0000
  private final val MissVacant = 0xC000_0000

  private val exceptionDefault: Long -> Nothing = (k: Long) => throw new NoSuchElementException(k.toString)

  /** A builder for instances of `LongMap`.
   *
   *  This builder can be reused to create multiple instances.
   *
   *  @tparam V the type of the values in the map being built
   */
  final class LongMapBuilder[V] extends ReusableBuilder[(Long, V), LongMap[V]] {
    private[collection] var elems: LongMap[V] = new LongMap[V]
    /** Adds a key/value pair to the map under construction.
     *
     *  @param entry the key/value pair to add; any existing value for the same key is overwritten
     *  @return this builder
     */
    override def addOne(entry: (Long, V)): this.type = {
      elems += entry
      this
    }
    /** Resets this builder by starting a fresh, empty map; previously returned maps are unaffected. */
    def clear(): Unit = elems = new LongMap[V]
    /** Returns the map under construction. The map is returned directly, not copied, so additions made before the next `clear` also appear in the returned map. */
    def result(): LongMap[V] = elems
    /** Returns the number of entries added since the last `clear`. */
    override def knownSize: Int = elems.knownSize
  }

  /** Creates a new `LongMap` with zero or more key/value pairs.
   *
   *  @tparam V the type of the values
   *  @param elems the key/value pairs to initialize the map with
   *  @return a new `LongMap` populated with the given `elems`
   */
  def apply[V](elems: (Long, V)*): LongMap[V] = buildFromIterableOnce(elems)

  private def buildFromIterableOnce[V](elems: IterableOnce[(Long, V)]^): LongMap[V] = {
    var sz = elems.knownSize
    if(sz < 0) sz = 4
    val lm = new LongMap[V](sz * 2)
    elems.iterator.foreach{ case (k,v) => lm(k) = v }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new empty `LongMap`.
   *
   *  @tparam V the type of the values
   *  @return a new empty `LongMap` whose value type is `V`, and whose default for an
   *          absent key throws, as `LongMap.exceptionDefault` does
   */
  def empty[V]: LongMap[V] = new LongMap[V]

  /** Creates a new empty `LongMap` with the supplied default.
   *
   *  @tparam V the type of the values
   *  @param default the function mapping keys to default values
   *  @return a new empty `LongMap` that uses `default` to supply values for missing keys
   */
  def withDefault[V](default: Long -> V): LongMap[V] = new LongMap[V](default)

  /** Creates a new `LongMap` from an existing source collection. A source collection
   *  which is already a `LongMap` gets cloned.
   *
   *  @tparam V the type of the values
   *  @param source the source collection to create the map from
   *  @return a new `LongMap` with the elements of `source`; if `source` is already a `LongMap`, it is cloned
   */
  def from[V](source: IterableOnce[(Long, V)]^): LongMap[V] = source match {
    case source: LongMap[?] => source.clone().asInstanceOf[LongMap[V]]
    case _ => buildFromIterableOnce(source)
  }

  /** Creates a new empty builder for a `LongMap`.
   *
   *  @tparam V the type of values
   *  @return a new reusable builder producing a `LongMap`
   */
  def newBuilder[V]: ReusableBuilder[(Long, V), LongMap[V]] = new LongMapBuilder[V]

  /** Creates a new `LongMap` from arrays of keys and values.
   *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
   *
   *  @tparam V the type of the values
   *  @param keys the array of `Long` keys
   *  @param values the array of values corresponding to each key
   *  @return a new `LongMap` pairing each key in `keys` with the value at the same index in `values`, up to the shorter of the two
   */
  def fromZip[V](keys: Array[Long], values: Array[V]): LongMap[V] = {
    val sz = math.min(keys.length, values.length)
    val lm = new LongMap[V](sz * 2)
    var i = 0
    while (i < sz) { lm(keys(i)) = values(i); i += 1 }
    if (lm.size < (sz>>3)) lm.repack()
    lm
  }

  /** Creates a new `LongMap` from keys and values.
   *  Equivalent to but more efficient than `LongMap((keys zip values): _*)`.
   *
   *  @tparam V the type of the values
   *  @param keys the iterable of `Long` keys
   *  @param values the iterable of values corresponding to each key
   *  @return a new `LongMap` pairing each key from `keys` with the value at the same position in `values`, up to the shorter of the two iterables
   */
  def fromZip[V](keys: scala.collection.Iterable[Long], values: scala.collection.Iterable[V]): LongMap[V] = {
    val sz = math.min(keys.size, values.size)
    val lm = new LongMap[V](sz * 2)
    val ki = keys.iterator
    val vi = values.iterator
    while (ki.hasNext && vi.hasNext) lm(ki.next()) = vi.next()
    if (lm.size < (sz >> 3)) lm.repack()
    lm
  }

  /** Implicitly converts this companion object to a `Factory`, so it can be passed
   *  where a factory of `LongMap`s is expected, for example to `to(LongMap)`.
   *
   *  @tparam V the type of values
   *  @param dummy this companion object; its value is never used
   *  @return a `Factory` that builds a `LongMap` from key/value pairs
   */
  implicit def toFactory[V](dummy: LongMap.type): Factory[(Long, V), LongMap[V]] = ToFactory.asInstanceOf[Factory[(Long, V), LongMap[V]]]

  @SerialVersionUID(3L)
  private object ToFactory extends Factory[(Long, AnyRef), LongMap[AnyRef]], Serializable {
    /** Builds a `LongMap` from a collection of key/value pairs.
     *
     *  @param it the key/value pairs
     *  @return a new `LongMap` containing the pairs of `it`
     */
    def fromSpecific(it: IterableOnce[(Long, AnyRef)]^): LongMap[AnyRef] = LongMap.from[AnyRef](it)
    /** Returns a new empty builder for a `LongMap`. */
    def newBuilder: Builder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  /** Implicitly converts this companion object to a `BuildFrom`, so it can be passed
   *  where a `BuildFrom` producing `LongMap`s is expected.
   *
   *  @tparam V the type of values
   *  @param factory this companion object; its value is never used
   *  @return a `BuildFrom` that builds a `LongMap` from key/value pairs, ignoring the source collection
   */
  implicit def toBuildFrom[V](factory: LongMap.type): BuildFrom[Any, (Long, V), LongMap[V]] = ToBuildFrom.asInstanceOf[BuildFrom[Any, (Long, V), LongMap[V]]]
  private object ToBuildFrom extends BuildFrom[Any, (Long, AnyRef), LongMap[AnyRef]] {
    /** Builds a `LongMap` from a collection of key/value pairs.
     *
     *  @param from the source collection; never used
     *  @param it the key/value pairs
     *  @return a new `LongMap` containing the pairs of `it`
     */
    def fromSpecific(from: Any)(it: IterableOnce[(Long, AnyRef)]^) = LongMap.from(it)
    /** Returns a new empty builder for a `LongMap`.
     *
     *  @param from the source collection; never used
     *  @return a new reusable builder producing a `LongMap`
     */
    def newBuilder(from: Any): ReusableBuilder[(Long, AnyRef), LongMap[AnyRef]] = LongMap.newBuilder[AnyRef]
  }

  /** An implicit `Factory` for `LongMap`s, for APIs that look one up implicitly.
   *
   *  @tparam V the type of values
   *  @return a `Factory` that builds a `LongMap` from key/value pairs
   */
  implicit def iterableFactory[V]: Factory[(Long, V), LongMap[V]] = toFactory(this)
  /** An implicit `BuildFrom` that builds a `LongMap` when the source collection is a `LongMap`.
   *
   *  @tparam V the type of values of the resulting map
   *  @return a `BuildFrom` that builds a `LongMap` from key/value pairs
   */
  implicit def buildFromLongMap[V]: BuildFrom[LongMap[?], (Long, V), LongMap[V]] = toBuildFrom(this)

  private def repackMask(mask: Int, _size: Int, _vacant: Int): Int = {
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && _size < (m >>> 3)) m = m >>> 1
    m /*.ensuring(_size <= _ + 1)*/
  }
}
