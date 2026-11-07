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
import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.DefaultSerializable
import scala.util.hashing.MurmurHash3

/** This class implements mutable sets using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam A     the type of the elements contained in this set.
 *
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@deprecatedInheritance("LinkedHashSet will be made final", "2.13.11")
class LinkedHashSet[A]
  extends AbstractSet[A]
    with SetOps[A, LinkedHashSet, LinkedHashSet[A]]
    with StrictOptimizedIterableOps[A, LinkedHashSet, LinkedHashSet[A]]
    with IterableFactoryDefaults[A, LinkedHashSet]
    with DefaultSerializable {

  /** The factory used to build linked hash sets, the [[LinkedHashSet$ `LinkedHashSet`]] companion object. */
  override def iterableFactory: IterableFactory[LinkedHashSet] = LinkedHashSet

  // stepper is not overridden to use XTableStepper because that stepper would not return the
  // elements in insertion order

  /*private*/ type Entry = LinkedHashSet.Entry[A]

  /** The first entry in insertion order, the head of the doubly-linked entry
   *  list, or `null` if this set is empty.
   */
  @annotation.stableNull
  protected var firstEntry: Entry | Null = null

  /** The last entry in insertion order, the tail of the doubly-linked entry
   *  list, or `null` if this set is empty.
   */
  @annotation.stableNull
  protected var lastEntry: Entry | Null = null

  /* Uses the same implementation as mutable.HashSet. The hashtable holds the following invariant:
   * - For each i between 0 and table.length, the bucket at table(i) only contains keys whose hash-index is i.
   * - Every bucket is sorted in ascendant hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  private var table = new Array[Entry | Null](tableSizeFor(LinkedHashSet.defaultinitialSize))

  private var threshold: Int = newThreshold(table.length)

  private var contentSize = 0

  /** Returns the most recently inserted element of this set.
   *
   *  @return the last element in insertion order
   *  @throws NoSuchElementException if this set is empty
   */
  override def last: A =
    if (size > 0) lastEntry.nn.key
    else throw new NoSuchElementException("Cannot call .last on empty LinkedHashSet")

  /** Returns the most recently inserted element of this set wrapped in `Some`,
   *  or `None` if this set is empty.
   */
  override def lastOption: Option[A] =
    if (size > 0) Some(lastEntry.nn.key)
    else None

  /** Returns the first inserted element of this set.
   *
   *  @return the first element in insertion order
   *  @throws NoSuchElementException if this set is empty
   */
  override def head: A =
    if (size > 0) firstEntry.nn.key
    else throw new NoSuchElementException("Cannot call .head on empty LinkedHashSet")

  /** Returns the first inserted element of this set wrapped in `Some`, or
   *  `None` if this set is empty.
   */
  override def headOption: Option[A] =
    if (size > 0) Some(firstEntry.nn.key)
    else None

  /** The number of elements in this set. */
  override def size: Int = contentSize
  /** The number of elements in this set; always known, never `-1`. */
  override def knownSize: Int = size
  /** Tests whether this set contains no elements. */
  override def isEmpty: Boolean = size == 0

  /** Tests whether an element is in this set.
   *
   *  @param elem the element to look for
   *  @return `true` if this set contains `elem`
   */
  def contains(elem: A): Boolean = findEntry(elem) ne null

  /** Grows the hash table, if needed, so that this set can hold `size` elements
   *  without further resizing.
   *
   *  The table never shrinks: a hint smaller than the current capacity does nothing.
   *
   *  @param size the expected number of elements
   */
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / LinkedHashSet.defaultLoadFactor).toInt)
    if (target > table.length) growTable(target)
  }

  /** Adds an element to this set, if it is not already present.
   *
   *  A new element is appended at the end of the iteration order. If `elem`
   *  is already in this set, nothing changes: in particular, the element
   *  keeps its original position in the iteration order.
   *
   *  @param elem the element to add
   *  @return `true` if `elem` was added, `false` if it was already present
   */
  override def add(elem: A): Boolean = {
    if (contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(elem)
    put0(elem, hash, index(hash))
  }

  /** Adds an element to this set, if it is not already present.
   *
   *  As with [[add]], a new element is appended at the end of the iteration
   *  order and an existing element keeps its position.
   *
   *  @param elem the element to add
   *  @return this set
   */
  def addOne(elem: A): this.type = {
    add(elem)
    this
  }

  /** Removes an element from this set, if present.
   *
   *  @param elem the element to remove
   *  @return this set
   */
  def subtractOne(elem: A): this.type = {
    remove(elem)
    this
  }

  /** Removes an element from this set, if present.
   *
   *  @param elem the element to remove
   *  @return `true` if `elem` was present and has been removed, `false` otherwise
   */
  override def remove(elem: A): Boolean = remove0(elem, computeHash(elem))

  private abstract class LinkedHashSetIterator[T] extends AbstractIterator[T] {
    private var cur: Entry | Null = firstEntry
    def extract(nd: Entry): T
    def hasNext: Boolean = cur ne null
    def next(): T =
      if (hasNext) { val r = extract(cur.nn); cur = cur.nn.later; r }
      else Iterator.empty.next()
  }

  /** Returns an iterator over the elements of this set, in insertion order. */
  def iterator: Iterator[A] = new LinkedHashSetIterator[A] {
    override def extract(nd: Entry): A = nd.key
  }

  private[collection] def entryIterator: Iterator[Entry] = new LinkedHashSetIterator[Entry] {
    override def extract(nd: Entry): Entry = nd
  }

  /** Applies a function to each element of this set, in insertion order.
   *
   *  @tparam U the result type of `f`; the results are discarded
   *  @param f the function applied to each element
   */
  override def foreach[U](f: A => U): Unit = {
    var cur: Entry | Null = firstEntry
    while (cur ne null) {
      f(cur.key)
      cur = cur.later
    }
  }

  /** Removes all elements from this set. The hash table keeps its current capacity. */
  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
    firstEntry = null
    lastEntry = null
  }

  private def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity - 1).max(4)) * 2).min(1 << 30)

  private def newThreshold(size: Int) = (size.toDouble * LinkedHashSet.defaultLoadFactor).toInt

  @`inline` private def improveHash(originalHash: Int): Int = {
    originalHash ^ (originalHash >>> 16)
  }

  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of this key.
   *
   *  @param o the key whose hash to compute
   *  @return the improved hash code of `o`, derived by mixing the high and low bits of its `##` value
   */
  @`inline` private def computeHash(o: A): Int = improveHash(o.##)

  @`inline` private def index(hash: Int) = hash & (table.length - 1)

  @`inline` private def findEntry(key: A): Entry | Null = {
    val hash = computeHash(key)
    table(index(hash)) match {
      case null => null
      case nd => nd.findEntry(key, hash)
    }
  }

  /*create a new entry. If table is empty(firstEntry is null), then the
  * new entry will be the firstEntry. If not, just set the new entry to
  * be the lastEntry.
  * */
  private def createNewEntry(key: A, hash: Int): Entry = {
    val e = new Entry(key, hash)
    if (firstEntry eq null) firstEntry = e
    else {
      lastEntry.nn.later = e
      e.earlier = lastEntry
    }
    lastEntry = e
    e
  }

  /** Deletes the entry from the `LinkedHashSet`, set the `earlier` and `later` pointers correctly.
   *
   *  @param e the entry to remove from the `LinkedHashSet`
   */
  private def deleteEntry(e: Entry): Unit = {
    if (e.earlier eq null) firstEntry = e.later
    else e.earlier.later = e.later
    if (e.later eq null) lastEntry = e.earlier
    else e.later.earlier = e.earlier
    e.earlier = null
    e.later = null
    e.next = null
  }

  private def put0(elem: A, hash: Int, idx: Int): Boolean = {
    table(idx) match {
      case null =>
        table(idx) = createNewEntry(elem, hash)
      case old =>
        var prev: Entry | Null = null
        var n: Entry | Null = old
        while ((n ne null) && n.hash <= hash) {
          if (n.hash == hash && elem == n.key) return false
          prev = n
          n = n.next
        }
        val nnode = createNewEntry(elem, hash)
        if (prev eq null) {
          nnode.next = old
          table(idx) = nnode
        } else {
          nnode.next = prev.next
          prev.next = nnode
        }
    }
    contentSize += 1
    true
  }

  private def remove0(elem: A, hash: Int): Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null => false
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        deleteEntry(nd)
        contentSize -= 1
        true
      case nd =>
        // find an element that matches
        var prev = nd
        var next: Entry | Null = nd.next
        while ((next ne null) && next.hash <= hash) {
          if (next.hash == hash && next.key == elem) {
            prev.next = next.next
            deleteEntry(next)
            contentSize -= 1
            return true
          }
          prev = next
          next = next.next
        }
        false
    }
  }

  private def growTable(newlen: Int): Unit = {
    if (newlen < 0)
      throw new RuntimeException(s"new hash table size $newlen exceeds maximum")
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if (size == 0) table = new Array(newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow = new Entry(null.asInstanceOf[A], 0)
      val preHigh = new Entry(null.asInstanceOf[A], 0)
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while (oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old: Entry | Null = table(i)
          if (old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow = preLow
            var lastHigh = preHigh
            var n: Entry | Null = old
            while (n ne null) {
              val next = n.next
              if ((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if (old ne preLow.next) table(i) = preLow.next
            if (preHigh.next ne null) {
              table(i + oldlen) = preHigh.next
              lastHigh.next = null
            }
          }
          i += 1
        }
        oldlen *= 2
      }
    }
  }

  /** Returns a hash code for this set, combining the hashes of its elements
   *  without regard to order, so it is consistent with `equals` across set
   *  implementations.
   */
  override def hashCode(): Int = {
    val setHashIterator =
      if (isEmpty) this.iterator
      else {
        new LinkedHashSetIterator[Any] {
          var hash: Int = 0
          override def hashCode(): Int = hash
          override def extract(nd: Entry): Any = {
            hash = unimproveHash(nd.hash)
            this
          }
        }
      }
    MurmurHash3.unorderedHash(setHashIterator, MurmurHash3.setSeed)
  }

  /** The prefix used in the string representation of this set, `"LinkedHashSet"`. */
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected def stringPrefix: String = "LinkedHashSet"
}

/** $factoryInfo
 *  @define Coll `LinkedHashSet`
 *  @define coll linked hash set
 */
@SerialVersionUID(3L)
object LinkedHashSet extends IterableFactory[LinkedHashSet] {

  /** Creates a new empty linked hash set.
   *
   *  @tparam A the element type of the set
   *  @return a new empty `LinkedHashSet[A]`
   */
  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]

  /** Creates a new linked hash set containing the elements of the given
   *  collection, in its iteration order.
   *
   *  If `it` contains an element more than once, the element keeps the
   *  position of its first occurrence.
   *
   *  @tparam E the element type of the set
   *  @param it the collection of elements
   */
  def from[E](it: collection.IterableOnce[E]^) = {
    val newlhs = empty[E]
    newlhs.sizeHint(it, delta = 0)
    newlhs.addAll(it)
    newlhs
  }

  /** Returns a new builder that accumulates elements into a linked hash set.
   *
   *  @tparam A the element type of the set
   *  @return a builder for a `LinkedHashSet[A]`
   */
  def newBuilder[A]: GrowableBuilder[A, LinkedHashSet[A]] = new GrowableBuilder(empty[A])

  /** Class for the linked hash set entry, used internally.
   *
   *  @tparam A the type of the elements contained in the linked hash set
   *  @param key the element stored in this entry
   *  @param hash the hash value of `key`
   */
  private[mutable] final class Entry[A](val key: A, val hash: Int) {
    @annotation.stableNull var earlier: Entry[A] | Null = null
    @annotation.stableNull var later: Entry[A] | Null = null
    @annotation.stableNull var next: Entry[A] | Null = null

    @tailrec
    final def findEntry(k: A, h: Int): Entry[A] | Null =
      if (h == hash && k == key) this
      else if ((next eq null) || (hash > h)) null
      else next.findEntry(k, h)
  }

  /** The default load factor for the hash table. */
  private[collection] final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table. */
  private[collection] final def defaultinitialSize: Int = 16
}
