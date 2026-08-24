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

import scala.annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection.generic.DefaultSerializationProxy
import scala.util.hashing.MurmurHash3

/** This class implements mutable sets using a hashtable.
 *
 *  @see ["Scala's Collection Library overview"](https://docs.scala-lang.org/overviews/collections-2.13/concrete-mutable-collection-classes.html#hash-tables)
 *  section on `Hash Tables` for more information.
 *
 *  @define Coll `mutable.HashSet`
 *  @define coll mutable hash set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *
 *  @tparam A the element type of the set
 *  @param initialCapacity the initial capacity of the internal hash table
 *  @param loadFactor the load factor for the hash table (ratio of size to capacity that triggers resizing)
 */
final class HashSet[A](initialCapacity: Int, loadFactor: Double)
  extends AbstractSet[A]
    with SetOps[A, HashSet, HashSet[A]]
    with StrictOptimizedIterableOps[A, HashSet, HashSet[A]]
    with IterableFactoryDefaults[A, HashSet]
    with Serializable {

  /** Creates a new, empty hash set with the default initial capacity (16) and the default load factor (0.75). */
  def this() = this(HashSet.defaultInitialCapacity, HashSet.defaultLoadFactor)

  import HashSet.Node

  /* The `HashSet` class holds the following invariant:
   * - For each i between  0 and table.length, the bucket at table(i) only contains elements whose hash-index is i.
   * - Every bucket is sorted in ascendent hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  /** The actual hash table. */
  private var table = new Array[Node[A] | Null](tableSizeFor(initialCapacity))

  /** The next size value at which to resize (capacity * load factor). */
  private var threshold: Int = newThreshold(table.length)

  private var contentSize = 0

  /** Returns the number of elements in this set. */
  override def size: Int = contentSize

  /** Performs the inverse operation of improveHash. In this case, it happens to be identical to improveHash.
   *
   *  @param improvedHash the improved hash value to convert back to a standard hash index
   *  @return the hash index recovered by re-applying the improvement transformation (which happens to be its own inverse)
   */
  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of an original (`any.##`) hash.
   *
   *  @param originalHash the original hash code obtained from `##`
   *  @return the improved hash, with high bits XORed into the low bits to spread entropy into the bits used for bucket indexing
   */
  private def improveHash(originalHash: Int): Int = {
    // Improve the hash by xoring the high 16 bits into the low 16 bits just in case entropy is skewed towards the
    // high-value bits. We only use the lowest bits to determine the hash bucket. This is the same improvement
    // algorithm as in java.util.HashMap.
    originalHash ^ (originalHash >>> 16)
  }

  /** Computes the improved hash of this element.
   *
   *  @param o the element whose hash to compute
   *  @return the improved hash of `o`, computed from `o.##`
   */
  @`inline` private def computeHash(o: A): Int = improveHash(o.##)

  @`inline` private def index(hash: Int) = hash & (table.length - 1)

  /** Tests if some element is contained in this set.
   *
   *  @param elem the element to test for membership
   *  @return `true` if `elem` is contained in this set, `false` otherwise
   */
  override def contains(elem: A): Boolean = findNode(elem) ne null

  @`inline` private def findNode(elem: A): Node[A] | Null = {
    val hash = computeHash(elem)
    table(index(hash)) match {
      case null => null
      case nd => nd.findNode(elem, hash)
    }
  }

  /** Grows the internal table, if necessary, so that `size` elements can be stored without
   *  triggering a resize. Never shrinks the table.
   *
   *  @param size the expected number of elements
   */
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / loadFactor).toInt)
    if(target > table.length) growTable(target)
  }

  /** Adds a single element to this set, growing the table first if the size threshold
   *  (capacity times load factor) would be reached.
   *
   *  @param elem the element to add
   *  @return `true` if `elem` was not yet present and has been added, `false` if an equal
   *          element was already in the set (which is then left unchanged)
   */
  override def add(elem: A) : Boolean = {
    if(contentSize + 1 >= threshold) growTable(table.length * 2)
    addElem(elem, computeHash(elem))
  }

  /** Adds all elements produced by `xs` to this set. Elements already present are left
   *  unchanged. When `xs` is an `immutable.HashSet`, a `mutable.HashSet`, or a
   *  `LinkedHashSet`, the hash codes cached in `xs` are reused instead of being recomputed.
   *
   *  @param xs the elements to add
   *  @return this set
   */
  override def addAll(xs: IterableOnce[A]^): this.type = {
    sizeHint(xs, delta = 0)
    (xs: @unchecked) match {
      case hs: immutable.HashSet[A] =>
        hs.foreachWithHash((k, h) => addElem(k, improveHash(h)))
        this
      case hs: mutable.HashSet[A] =>
        val iter = hs.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          addElem(next.key, next.hash)
        }
        this
      case lhs: mutable.LinkedHashSet[A] =>
        val iter = lhs.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          addElem(next.key, next.hash)
        }
        this
      case _ => super.addAll(xs)
    }
  }

  /** Removes all elements produced by `xs` from this set, stopping early once this set is
   *  empty. When `xs` is an `immutable.HashSet`, a `mutable.HashSet`, or a `LinkedHashSet`,
   *  the hash codes cached in `xs` are reused instead of being recomputed.
   *
   *  @param xs the elements to remove
   *  @return this set
   */
  override def subtractAll(xs: IterableOnce[A]^): this.type = {
    if (size == 0) {
      return this
    }

    (xs: @unchecked) match {
      case hs: immutable.HashSet[A] =>
        hs.foreachWithHashWhile { (k, h) =>
          remove(k, improveHash(h))
          size > 0
        }
        this
      case hs: mutable.HashSet[A] =>
        val iter = hs.nodeIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case lhs: mutable.LinkedHashSet[A] =>
        val iter = lhs.entryIterator
        while (iter.hasNext) {
          val next = iter.next()
          remove(next.key, next.hash)
          if (size == 0) return this
        }
        this
      case _ => super.subtractAll(xs)
    }
  }

  /** Adds an element to this set.
   *
   *  @param elem element to add
   *  @param hash the **improved** hash of `elem` (see computeHash)
   *  @return `true` if `elem` was added to the set, or `false` if an equal element was already present
   */
  private def addElem(elem: A, hash: Int) : Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null =>
        table(idx) = new Node(elem, hash, null)
      case old =>
        var prev: Node[A] | Null = null
        var n: Node[A] | Null = old
        while((n ne null) && n.hash <= hash) {
          if(n.hash == hash && elem == n.key) return false
          prev = n
          n = n.next
        }
        if(prev eq null)
          table(idx) = new Node(elem, hash, old)
        else
          prev.next = new Node(elem, hash, prev.next)
    }
    contentSize += 1
    true
  }

  private def remove(elem: A, hash: Int): Boolean = {
    val idx = index(hash)
    table(idx) match {
      case null => false
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        contentSize -= 1
        true
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while((next ne null) && next.hash <= hash) {
          if(next.hash == hash && next.key == elem) {
            prev.next = next.next
            contentSize -= 1
            return true
          }
          prev = next
          next = next.next
        }
        false
    }
  }

  /** Removes an element from this set.
   *
   *  @param elem the element to remove
   *  @return `true` if `elem` was present and has been removed, `false` if it was not in the set
   */
  override def remove(elem: A) : Boolean = remove(elem, computeHash(elem))

  private abstract class HashSetIterator[B] extends AbstractIterator[B] {
    private var i = 0
    private var node: Node[A] | Null = null
    private val len = table.length

    /** Extracts the value this iterator yields for the given node.
     *
     *  @param nd the node currently visited
     *  @return the value derived from `nd` (for instance its key)
     */
    protected def extract(nd: Node[A]): B

    /** Returns `true` if there are more nodes to visit, advancing to the next non-empty
     *  bucket if the current chain is exhausted.
     */
    def hasNext: Boolean = {
      if(node ne null) true
      else {
        while(i < len) {
          val n = table(i)
          i += 1
          if(n ne null) { node = n; return true }
        }
        false
      }
    }

    /** Returns the value extracted from the next node.
     *
     *  @throws NoSuchElementException if no more nodes remain
     */
    def next(): B =
      if(!hasNext) Iterator.empty.next()
      else {
        val r = extract(node.nn)
        node = node.nn.next
        r
      }
  }

  /** Returns an iterator over the elements of this set. The iteration order is not specified
   *  and may change when the set is modified.
   */
  override def iterator: Iterator[A] = new HashSetIterator[A] {
    override protected def extract(nd: Node[A]): A = nd.key
  }

  /** Returns an iterator over the nodes stored in this HashSet. */
  private[collection] def nodeIterator: Iterator[Node[A]] = new HashSetIterator[Node[A]] {
    override protected def extract(nd: Node[A]): Node[A] = nd
  }

  /** Returns a [[Stepper]] for the elements of this set, stepping through the hash table directly.
   *
   *  @tparam S the type of `Stepper` to use, determined by the implicit `StepperShape`
   *  @param shape the implicit `StepperShape` that selects the appropriate primitive or boxed `Stepper` for `A`
   *  @return a `Stepper` over the elements of this set, specialized for primitives when the
   *          resolved `StepperShape` corresponds to `Int`, `Long`, or `Double`, and supporting
   *          efficient splitting
   */
  override def stepper[S <: Stepper[?]](implicit shape: StepperShape[A, S]): S & EfficientSplit = {
    import convert.impl._
    val s = shape.shape match {
      case StepperShape.IntShape    => new IntTableStepper[Node[A]]   (size, table, _.next, _.key.asInstanceOf[Int],    0, table.length)
      case StepperShape.LongShape   => new LongTableStepper[Node[A]]  (size, table, _.next, _.key.asInstanceOf[Long],   0, table.length)
      case StepperShape.DoubleShape => new DoubleTableStepper[Node[A]](size, table, _.next, _.key.asInstanceOf[Double], 0, table.length)
      case _         => shape.parUnbox(new AnyTableStepper[A, Node[A]](size, table, _.next, _.key,                      0, table.length))
    }
    s.asInstanceOf[S & EfficientSplit]
  }

  private def growTable(newlen: Int) = {
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if(size == 0) table = new Array[Node[A] | Null](newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow: Node[A] = new Node(null.asInstanceOf[A], 0, null)
      val preHigh: Node[A] = new Node(null.asInstanceOf[A], 0, null)
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while(oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if(old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow: Node[A] = preLow
            var lastHigh: Node[A] = preHigh
            var n: Node[A] | Null = old
            while(n ne null) {
              val next = n.next
              if((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if(old ne preLow.next) table(i) = preLow.next
            if(preHigh.next ne null) {
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

  /** Removes all elements from this set for which the predicate returns `false`.
   *
   *  @param p the predicate used to test elements; elements for which it returns `false` are removed
   *  @return this set
   */
  override def filterInPlace(p: A => Boolean): this.type = {
    if (nonEmpty) {
      var bucket = 0

      while (bucket < table.length) {
        var head = table(bucket)

        while ((head ne null) && !p(head.key)) {
          head = head.next
          contentSize -= 1
        }

        if (head ne null) {
          var prev = head
          var next = head.next

          while (next ne null) {
            if (p(next.key)) {
              prev = next
            } else {
              prev.next = next.next
              contentSize -= 1
            }
            next = next.next
          }
        }

        table(bucket) = head
        bucket += 1
      }
    }
    this
  }

  /*
  private[mutable] def checkTable(): Unit = {
    var i = 0
    var count = 0
    var prev: Node[A] = null
    while(i < table.length) {
      var n = table(i)
      prev = null
      while(n != null) {
        count += 1
        assert(index(n.hash) == i)
        if(prev ne null) assert(prev.hash <= n.hash)
        prev = n
        n = n.next
      }
      i += 1
    }
    assert(contentSize == count)
  }
  */

  private def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity-1).max(4))*2).min(1 << 30)

  private def newThreshold(size: Int) = (size.toDouble * loadFactor).toInt

  /** Removes all elements from this set. The internal table keeps its current capacity. */
  def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
  }

  /** Returns the companion object [[HashSet]], used to build hash sets of the same kind. */
  override def iterableFactory: IterableFactory[HashSet] = HashSet

  @`inline` def addOne(elem: A): this.type = { add(elem); this }

  @`inline` def subtractOne(elem: A): this.type = { remove(elem); this }

  /** Returns the number of elements in this set; never `-1`, since the size of a hash set is always known. */
  override def knownSize: Int = size

  /** Returns `true` if this set contains no elements. */
  override def isEmpty: Boolean = size == 0

  /** Applies a function `f` to each element of this set. The order of traversal is not
   *  specified and may change when the set is modified.
   *
   *  @tparam U the result type of `f`, which is discarded
   *  @param f the function to apply to each element
   */
  override def foreach[U](f: A => U): Unit = {
    val len = table.length
    var i = 0
    while(i < len) {
      val n = table(i)
      if(n ne null) n.foreach(f)
      i += 1
    }
  }

  /** Replaces this set with a serialization proxy during Java serialization. The proxy records
   *  the current table length and load factor so deserialization can restore an equivalent set.
   */
  protected def writeReplace(): AnyRef = new DefaultSerializationProxy(new HashSet.DeserializationFactory[A](table.length, loadFactor), this)

  /** The name of this collection class, `"HashSet"`, used as the prefix in `toString`. */
  override protected def className = "HashSet"

  /** Returns the hash code of this set, computed as an unordered [[scala.util.hashing.MurmurHash3]]
   *  hash of the element hash codes. The element hashes are recovered from the values cached in the
   *  nodes (via `unimproveHash`) rather than recomputed, so the result is the same as for other sets
   *  with equal elements.
   */
  override def hashCode(): Int = {
    val setIterator = this.iterator
    val hashIterator: Iterator[Any] =
      if (setIterator.isEmpty) setIterator
      else new HashSetIterator[Any] {
        var hash: Int = 0
        override def hashCode(): Int = hash
        override protected def extract(nd: Node[A]): Any = {
          hash = unimproveHash(nd.hash)
          this
        }
      }
    MurmurHash3.unorderedHash(hashIterator, MurmurHash3.setSeed)
  }
}

/** $factoryInfo
 *  @define Coll `mutable.HashSet`
 *  @define coll mutable hash set
 */
@SerialVersionUID(3L)
object HashSet extends IterableFactory[HashSet] {

  /** Creates a new hash set containing the elements of the given collection. When the size of
   *  `it` is known, the initial capacity is chosen so that all elements can be added without
   *  resizing the table.
   *
   *  @tparam B the element type of the new set
   *  @param it the collection whose elements are added to the new set
   *  @return a new `HashSet` containing the elements of `it`
   */
  def from[B](it: scala.collection.IterableOnce[B]^): HashSet[B] = {
    val k = it.knownSize
    val cap = if(k > 0) ((k + 1).toDouble / defaultLoadFactor).toInt else defaultInitialCapacity
    new HashSet[B](cap, defaultLoadFactor) ++= it
  }

  /** Creates a new, empty hash set with the default initial capacity (16) and load factor (0.75).
   *
   *  @tparam A the element type of the new set
   *  @return a new, empty `HashSet`
   */
  def empty[A]: HashSet[A] = new HashSet[A]

  /** Creates a new builder for a `HashSet` with the default initial capacity and load factor.
   *
   *  @tparam A the element type of the set to build
   *  @return a new builder producing a `HashSet`
   */
  def newBuilder[A]: Builder[A, HashSet[A]] = newBuilder(defaultInitialCapacity, defaultLoadFactor)

  /** Creates a new builder for a `HashSet` with the given initial capacity and load factor.
   *  Size hints given to the builder are forwarded to the underlying set's `sizeHint`.
   *
   *  @tparam A the element type of the set to build
   *  @param initialCapacity the initial capacity of the set's hash table
   *  @param loadFactor the load factor of the set's hash table
   *  @return a new builder producing a `HashSet`
   */
  def newBuilder[A](initialCapacity: Int, loadFactor: Double): Builder[A, HashSet[A]] =
    new GrowableBuilder[A, HashSet[A]](new HashSet[A](initialCapacity, loadFactor)) {
      override def sizeHint(size: Int) = elems.sizeHint(size)
    }

  /** The default load factor for the hash table. */
  final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table. */
  final def defaultInitialCapacity: Int = 16

  @SerialVersionUID(3L)
  private final class DeserializationFactory[A](val tableLength: Int, val loadFactor: Double) extends Factory[A, HashSet[A]], Serializable {
    /** Creates a new `HashSet` with the recorded table length and load factor, containing the
     *  elements of `it`.
     *
     *  @param it the deserialized elements to add
     *  @return a new `HashSet` containing the elements of `it`
     */
    def fromSpecific(it: IterableOnce[A]^): HashSet[A] = new HashSet[A](tableLength, loadFactor) ++= it
    /** Returns a new builder for a `HashSet` with the recorded table length and load factor. */
    def newBuilder: Builder[A, HashSet[A]] = HashSet.newBuilder(tableLength, loadFactor)
  }

  private[collection] final class Node[K](_key: K, _hash: Int, @annotation.stableNull private var _next: Node[K] | Null) {
    /** Returns the element stored in this node. */
    def key: K = _key
    /** Returns the improved hash code of `key`, cached when the node was created. */
    def hash: Int = _hash
    /** Returns the next node in this bucket's chain, or `null` if this is the last one. */
    def next: Node[K] | Null = _next
    /** Sets the next node in this bucket's chain to `n`. */
    def next_= (n: Node[K] | Null): Unit = _next = n

    /** Finds the node for element `k` in the chain starting at this node. Because chains are
     *  sorted in ascending hash order, the search stops as soon as a node with a larger hash
     *  is seen.
     *
     *  @param k the element to look for
     *  @param h the improved hash code of `k`
     *  @return the node whose key equals `k`, or `null` if the chain contains no such node
     */
    @tailrec
    def findNode(k: K, h: Int): Node[K] | Null =
      if(h == _hash && k == _key) this
      else if((_next eq null) || (_hash > h)) null
      else _next.findNode(k, h)

    /** Applies `f` to the element of this node and of every following node in the chain.
     *
     *  @tparam U the result type of `f`, which is discarded
     *  @param f the function to apply to each element
     */
    @tailrec
    def foreach[U](f: K => U): Unit = {
      f(_key)
      if(_next ne null) _next.foreach(f)
    }

    /** Returns a string rendering this node's key and hash followed by the rest of the chain. */
    override def toString() = s"Node($key, $hash) -> $next"
  }
}
