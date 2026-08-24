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

import scala.annotation.nowarn
import java.lang.Integer.numberOfLeadingZeros
import java.util.ConcurrentModificationException
import scala.collection.generic.DefaultSerializable

/**
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 */
@deprecated("Use HashMap or one of the specialized versions (LongMap, AnyRefMap) instead of OpenHashMap", "2.13.0")
@SerialVersionUID(3L)
object OpenHashMap extends MapFactory[OpenHashMap] {

  /** Creates a new empty `OpenHashMap`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   */
  def empty[K, V] = new OpenHashMap[K, V]
  /** Creates a new `OpenHashMap` from a collection of key/value pairs.
   *
   *  If several pairs share a key, the last one wins.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @param it the key/value pairs to initialize the map with
   *  @return a new `OpenHashMap` containing the pairs of `it`
   */
  def from[K, V](it: IterableOnce[(K, V)]^): OpenHashMap[K,V] = empty ++= it

  /** Creates a new empty builder for an `OpenHashMap`.
   *
   *  @tparam K the type of keys
   *  @tparam V the type of values
   *  @return a new builder that produces an `OpenHashMap` from the key/value pairs added to it
   */
  def newBuilder[K, V]: Builder[(K, V), OpenHashMap[K,V]] =
    new GrowableBuilder[(K, V), OpenHashMap[K, V]](empty)

  /** A hash table entry.
   *
   *  The entry is occupied if and only if its `value` is a `Some`;
   *  deleted if and only if its `value` is `None`.
   *  If its `key` is not the default value of type `Key`, the entry is occupied.
   *  If the entry is occupied, `hash` contains the hash value of `key`.
   *
   *  @tparam Key the type of keys stored in this entry
   *  @tparam Value the type of values stored in this entry
   *  @param key the key associated with this entry
   *  @param hash the cached hash code of `key`
   *  @param value `Some(v)` if the entry is occupied, `None` if deleted
   */
  final private class OpenEntry[Key, Value](var key: Key,
                                            var hash: Int,
                                            var value: Option[Value])

  private[mutable] def nextPositivePowerOfTwo(target: Int): Int = 1 << -numberOfLeadingZeros(target - 1)
}

/** A mutable hash map based on an open addressing method. The precise scheme is
 *  undefined, but it should make a reasonable effort to ensure that an insert
 *  with consecutive hash codes is not unnecessarily penalised. In particular,
 *  mappings of consecutive integer keys should work without significant
 *  performance loss.
 *
 *  @tparam Key          type of the keys in this map.
 *  @tparam Value        type of the values in this map.
 *  @param initialSize   the initial size of the internal hash table.
 *
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
@deprecated("Use HashMap or one of the specialized versions (LongMap, AnyRefMap) instead of OpenHashMap", "2.13.0")
class OpenHashMap[Key, Value](initialSize : Int)
  extends AbstractMap[Key, Value]
    with MapOps[Key, Value, OpenHashMap, OpenHashMap[Key, Value]]
    with StrictOptimizedIterableOps[(Key, Value), Iterable, OpenHashMap[Key, Value]]
    with MapFactoryDefaults[Key, Value, OpenHashMap, Iterable]
    with DefaultSerializable {

  import OpenHashMap.OpenEntry
  private type Entry = OpenEntry[Key, Value]

  /** A default constructor creates a hashmap with initial size `8`. */
  def this() = this(8)

  /** Returns the companion object `OpenHashMap`, which builds maps of this kind. */
  override def mapFactory: MapFactory[OpenHashMap] = OpenHashMap

  private val actualInitialSize = OpenHashMap.nextPositivePowerOfTwo(initialSize)

  private var mask = actualInitialSize - 1

  /** The hash table.
   *
   *  The table's entries are initialized to `null`, indication of an empty slot.
   *  A slot is either deleted or occupied if and only if the entry is non-`null`.
   */
  private var table = new Array[Entry](actualInitialSize)

  private var _size = 0
  private var deleted = 0

  // Used for tracking inserts so that iterators can determine if concurrent modification has occurred.
  private var modCount = 0

  /** Returns the number of key/value pairs in this map. Deleted slots are not counted. */
  override def size = _size
  /** Returns `size`; the number of key/value pairs is always known. */
  override def knownSize: Int = size
  private def size_=(s : Int): Unit = _size = s
  /** Returns `true` if this map contains no key/value pairs. */
  override def isEmpty: Boolean = _size == 0
  /** Returns a mangled hash code of the provided key.
   *
   *  @param key the key to compute the hash for
   */
  protected def hashOf(key: Key) = {
    var h = key.##
    h ^= ((h >>> 20) ^ (h >>> 12))
    h ^ (h >>> 7) ^ (h >>> 4)
  }

  /** Increases the size of the table.
   *  Copies only the occupied slots, effectively eliminating the deleted slots.
   */
  private def growTable() = {
    val oldSize = mask + 1
    val newSize = 4 * oldSize
    val oldTable = table
    table = new Array[Entry](newSize)
    mask = newSize - 1
    oldTable.foreach( entry =>
      if (entry != null && entry.value != None)
        table(findIndex(entry.key, entry.hash)) = entry )
    deleted = 0
  }

  /** Returns the index of the first slot in the hash table (in probe order)
   *  that is, in order of preference, either occupied by the given key, deleted, or empty.
   *
   *  @param hash the hash code of `key`
   *  @param key the key to search for in the hash table
   *  @return the index of the slot containing `key` if present; otherwise the index of the first deleted slot encountered during probing, or the first empty slot if no deleted slot was found
   */
  private def findIndex(key: Key, hash: Int): Int = {
    var index = hash & mask
    var j = 0

    // Index of the first slot containing a deleted entry, or -1 if none found yet
    var firstDeletedIndex = -1

    var entry = table(index)
    while (entry != null) {
      if (entry.hash == hash && entry.key == key && entry.value != None)
        return index

      if (firstDeletedIndex == -1 && entry.value == None)
        firstDeletedIndex = index

      j += 1
      index = (index + j) & mask
      entry = table(index)
    }

    if (firstDeletedIndex == -1) index else firstDeletedIndex
  }

  // TODO refactor `put` to extract `findOrAddEntry` and implement this in terms of that to avoid Some boxing.
  /** Adds a new key/value pair to this map, replacing any value previously associated with the key.
   *
   *  Delegates to `put`, discarding its result.
   *
   *  @param key the key of the entry to add or update
   *  @param value the value to associate with `key`
   */
  override def update(key: Key, value: Value): Unit = put(key, value)

  /** Adds a key/value pair to this map and returns the map.
   *
   *  @param kv the key/value pair to add; any existing value for the same key is overwritten
   *  @return this map after the entry has been added
   */
  @deprecatedOverriding("addOne should not be overridden in order to maintain consistency with put.", "2.11.0")
  def addOne (kv: (Key, Value)): this.type = { put(kv._1, kv._2); this }

  /** Removes a key from this map, and returns the map.
   *
   *  Does nothing if `key` is not present.
   *
   *  @param key the key to remove
   *  @return this map after the removal
   */
  @deprecatedOverriding("subtractOne should not be overridden in order to maintain consistency with remove.", "2.11.0")
  def subtractOne (key: Key): this.type = { remove(key); this }

  /** Adds a key/value pair to this map, returning any value previously associated with the key.
   *
   *  A new entry reuses the first deleted slot on its probe path, if any;
   *  the table grows once the entry being added would take the number of occupied or
   *  deleted slots past half of them.
   *
   *  @param key the key to add
   *  @param value the value to associate with `key`
   *  @return `Some` of the value previously associated with `key`, or `None` if `key` was not present
   */
  override def put(key: Key, value: Value): Option[Value] =
    put(key, hashOf(key), value)

  private def put(key: Key, hash: Int, value: Value): Option[Value] = {
    if (2 * (size + deleted) > mask) growTable()
    val index = findIndex(key, hash)
    val entry = table(index)
    if (entry == null) {
      table(index) = new OpenEntry(key, hash, Some(value))
      modCount += 1
      size += 1
      None
    } else {
      val res = entry.value
      if (entry.value == None) {
        entry.key = key
        entry.hash = hash
        size += 1
        deleted -= 1
        modCount += 1
      }
      entry.value = Some(value)
      res
    }
  }

  /** Deletes the hash table slot contained in the given entry.
   *
   *  @param entry the hash table entry to mark as deleted
   */
  @`inline`
  private def deleteSlot(entry: Entry) = {
    entry.key = null.asInstanceOf[Key]
    entry.hash = 0
    entry.value = None

    size -= 1
    deleted += 1
  }

  /** Removes a key from this map, returning the value previously associated with it.
   *
   *  A removed entry's slot is not emptied but marked as deleted, so that
   *  probe sequences for other keys remain intact; deleted slots are
   *  reclaimed when an entry is added over one or when the table grows.
   *
   *  @param key the key to remove
   *  @return `Some` of the value previously associated with `key`, or `None` if `key` was not present
   */
  override def remove(key : Key): Option[Value] = {
    val entry = table(findIndex(key, hashOf(key)))
    if (entry != null && entry.value != None) {
      val res = entry.value
      deleteSlot(entry)
      res
    } else None
  }

  /** Optionally returns the value associated with a key.
   *
   *  @param key the key to look up
   *  @return `Some` of the value associated with `key`, or `None` if `key` is not present
   */
  def get(key : Key) : Option[Value] = {
    val hash = hashOf(key)
    var index = hash & mask
    var entry = table(index)
    var j = 0
    while(entry != null){
      if (entry.hash == hash &&
        entry.key == key){
        return entry.value
      }

      j += 1
      index = (index + j) & mask
      entry = table(index)
    }
    None
  }

  /** An iterator over the elements of this map. Use of this iterator follows
   *  the same contract for concurrent modification as the foreach method.
   *
   *  @return   the iterator
   */
  def iterator: Iterator[(Key, Value)] = new OpenHashMapIterator[(Key, Value)] {
    override protected def nextResult(node: Entry): (Key, Value) = (node.key, node.value.get)
  }

  /** Returns an iterator over the keys of this map, following the same concurrent modification contract as `iterator`. */
  override def keysIterator: Iterator[Key] = new OpenHashMapIterator[Key] {
    override protected def nextResult(node: Entry): Key = node.key
  }
  /** Returns an iterator over the values of this map, following the same concurrent modification contract as `iterator`. */
  override def valuesIterator: Iterator[Value] = new OpenHashMapIterator[Value] {
    override protected def nextResult(node: Entry): Value = node.value.get
  }

  private abstract class OpenHashMapIterator[A] extends AbstractIterator[A] {
    private var index = 0
    private val initialModCount = modCount

    private def advance(): Unit = {
      if (initialModCount != modCount) throw new ConcurrentModificationException
      while((index <= mask) && (table(index) == null || table(index).value == None)) index+=1
    }

    /** Returns `true` if an occupied slot remains, advancing the cursor past empty and deleted slots.
     *
     *  @throws ConcurrentModificationException if an entry was inserted into the map after this iterator was created
     */
    def hasNext = {advance(); index <= mask }

    /** Returns the result for the next occupied slot and advances the cursor past it.
     *
     *  @throws ConcurrentModificationException if an entry was inserted into the map after this iterator was created
     */
    def next() = {
      advance()
      val result = table(index)
      index += 1
      nextResult(result)
    }
    /** Extracts this iterator's result from a hash table entry.
     *
     *  @param node the occupied entry to extract the result from
     *  @return the part of `node` this iterator produces: its key, its value, or both
     */
    protected def nextResult(node: Entry): A
  }

  /** Returns a copy of this map with the same key/value pairs.
   *
   *  The copy is built by inserting each entry into a fresh table, so deleted
   *  slots are not carried over. Later changes to the copy do not affect this
   *  map, and vice versa.
   */
  override def clone() = {
    val it = new OpenHashMap[Key, Value]
    foreachUndeletedEntry(entry => it.put(entry.key, entry.hash, entry.value.get))
    it
  }

  /** Loops over the key, value mappings of this map.
   *
   *  The behaviour of modifying the map during an iteration is as follows:
   *  - Deleting a mapping is always permitted.
   *  - Changing the value of mapping which is already present is permitted.
   *  - Anything else is not permitted. It will usually, but not always, throw an exception.
   *
   *  @tparam U  The return type of the specified function `f`, return result of which is ignored.
   *  @param f   The function to apply to each key, value mapping.
   */
  override def foreach[U](f : ((Key, Value)) => U): Unit = {
    val startModCount = modCount
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) throw new ConcurrentModificationException
      f((entry.key, entry.value.get))}
    )
  }
  /** Loops over the key, value mappings of this map, passing key and value as separate arguments.
   *
   *  Unlike `foreach`, does not allocate a tuple per entry. The contract for
   *  modifying the map during the loop is the same as for `foreach`.
   *
   *  @tparam U  The return type of the specified function `f`, return result of which is ignored.
   *  @param f   The function to apply to each key, value mapping.
   */
  override def foreachEntry[U](f : (Key, Value) => U): Unit = {
    val startModCount = modCount
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) throw new ConcurrentModificationException
      f(entry.key, entry.value.get)}
    )
  }

  private def foreachUndeletedEntry(f : Entry => Unit): Unit = {
    table.foreach(entry => if (entry != null && entry.value != None) f(entry))
  }

  /** Applies a transformation function to all values stored in this map.
   *
   *  @param f the transformation to apply to each key/value pair; its result replaces the value stored for that key
   *  @return this map after each value has been replaced
   */
  override def mapValuesInPlace(f : (Key, Value) => Value): this.type = {
    foreachUndeletedEntry(entry => entry.value = Some(f(entry.key, entry.value.get)))
    this
  }

  /** Retains only those entries for which a predicate returns `true`.
   *
   *  The slots of removed entries are marked as deleted, as with `remove`.
   *
   *  @param f the predicate used to test each key/value pair
   *  @return this map after the entries failing the predicate have been removed
   */
  override def filterInPlace(f : (Key, Value) => Boolean): this.type = {
    foreachUndeletedEntry(entry => if (!f(entry.key, entry.value.get)) deleteSlot(entry))
    this
  }

  /** Returns `"OpenHashMap"`, the name used in this map's string representation. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix = "OpenHashMap"
}
