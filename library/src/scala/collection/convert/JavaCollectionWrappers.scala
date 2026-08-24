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
package convert

import scala.language.`2.13`
import language.experimental.captureChecking

import java.util.{concurrent => juc}
import java.util.{NavigableMap}
import java.{lang => jl, util => ju}

import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.chaining._
import scala.util.control.ControlThrowable

/** Wrappers for exposing Scala collections as Java collections and vice-versa. */
@SerialVersionUID(3L)
// not private[convert] because `WeakHashMap` uses JMapWrapper
private[collection] object JavaCollectionWrappers extends Serializable {
  /** Wraps a Scala `Iterator`, exposing it as both a Java `Iterator` and a
   *  Java `Enumeration`.
   *
   *  Both interfaces delegate to the wrapped iterator, so advancing this
   *  wrapper through either interface advances the underlying iterator.
   *
   *  @tparam A the type of the iterator's elements
   *  @param underlying the wrapped Scala iterator
   */
  @SerialVersionUID(3L)
  class IteratorWrapper[A](val underlying: Iterator[A]^) extends ju.Iterator[A] with ju.Enumeration[A] with Serializable {
    /** Returns `true` if the wrapped iterator has more elements. */
    def hasNext = underlying.hasNext
    /** Returns the next element of the wrapped iterator, advancing it.
     *
     *  @throws NoSuchElementException if the wrapped iterator has no more elements
     */
    def next(): A = underlying.next()
    /** Returns `true` if the wrapped iterator has more elements. */
    def hasMoreElements = underlying.hasNext
    /** Returns the next element of the wrapped iterator, advancing it.
     *
     *  @throws NoSuchElementException if the wrapped iterator has no more elements
     */
    def nextElement(): A = underlying.next()
    /** Always throws: this wrapper does not support element removal.
     *
     *  @throws UnsupportedOperationException always
     */
    override def remove(): Nothing = throw new UnsupportedOperationException
    /** Returns `true` if `other` is an `IteratorWrapper` with an equal
     *  underlying iterator, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: IteratorWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying iterator. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Iterator` as a Scala `Iterator`.
   *
   *  Delegates to the wrapped iterator, so advancing this iterator advances
   *  the underlying one and vice versa.
   *
   *  @tparam A the type of the iterator's elements
   *  @param underlying the wrapped Java iterator
   */
  @SerialVersionUID(3L)
  class JIteratorWrapper[A](val underlying: ju.Iterator[A]) extends AbstractIterator[A] with Serializable {
    /** Returns `true` if the wrapped iterator has more elements. */
    def hasNext = underlying.hasNext
    /** Returns the next element of the wrapped iterator, advancing it.
     *
     *  @throws NoSuchElementException if the wrapped iterator has no more elements
     */
    def next(): A = underlying.next
    /** Returns `true` if `other` is a `JIteratorWrapper` with an equal
     *  underlying iterator, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: JIteratorWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying iterator. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Enumeration` as a Scala `Iterator`.
   *
   *  Delegates to the wrapped enumeration, so advancing this iterator
   *  advances the underlying enumeration and vice versa.
   *
   *  @tparam A the type of the enumeration's elements
   *  @param underlying the wrapped Java enumeration
   */
  @SerialVersionUID(3L)
  class JEnumerationWrapper[A](val underlying: ju.Enumeration[A]) extends AbstractIterator[A] with Serializable {
    /** Returns `true` if the wrapped enumeration has more elements. */
    def hasNext = underlying.hasMoreElements
    /** Returns the next element of the wrapped enumeration, advancing it.
     *
     *  @throws NoSuchElementException if the wrapped enumeration has no more elements
     */
    def next(): A = underlying.nextElement
    /** Returns `true` if `other` is a `JEnumerationWrapper` with an equal
     *  underlying enumeration, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: JEnumerationWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying enumeration. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Common implementations of `java.util.Collection` methods for wrappers
   *  that expose a Scala `Iterable` as a Java collection, all delegating to
   *  the wrapped collection.
   *
   *  @tparam A the type of the collection's elements
   */
  trait IterableWrapperTrait[A] extends ju.AbstractCollection[A] {
    /** The wrapped Scala collection. */
    val underlying: Iterable[A]^
    /** Returns the number of elements in the wrapped collection. */
    def size = underlying.size
    /** Returns a Java iterator over the wrapped collection, wrapping its Scala iterator. */
    override def iterator: IteratorWrapper[A]^{this} = new IteratorWrapper(underlying.iterator)
    /** Returns `true` if the wrapped collection is empty. */
    override def isEmpty = underlying.isEmpty
  }

  /** Wraps a Scala `Iterable` as a Java `Collection`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped
   *  collection at all times. Modification through the Java interface is not
   *  supported; the mutating methods inherited from
   *  `java.util.AbstractCollection` throw `UnsupportedOperationException`.
   *
   *  @tparam A the type of the collection's elements
   *  @param underlying the wrapped Scala collection
   */
  @SerialVersionUID(3L)
  class IterableWrapper[A](val underlying: Iterable[A]^) extends ju.AbstractCollection[A] with IterableWrapperTrait[A] with Serializable {
    /** Returns `true` if `other` is an `IterableWrapper` with an equal
     *  underlying collection, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: IterableWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying collection. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Iterable` as a Scala `Iterable`.
   *
   *  The wrapper is a view: iterating it iterates the wrapped iterable, so it
   *  reflects the wrapped iterable's contents at all times.
   *
   *  @tparam A the type of the iterable's elements
   *  @param underlying the wrapped Java iterable
   */
  @SerialVersionUID(3L)
  class JIterableWrapper[A](val underlying: jl.Iterable[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    /** Returns a Scala iterator over the wrapped iterable. */
    def iterator = underlying.iterator.asScala
    /** Returns the factory used to build transformed collections:
     *  transformations produce `mutable.ArrayBuffer`s rather than new wrappers.
     */
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    /** Returns `true` if the wrapped iterable has no elements. */
    override def isEmpty: Boolean = !underlying.iterator().hasNext
    /** Returns `true` if `other` is a `JIterableWrapper` with an equal
     *  underlying iterable, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: JIterableWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying iterable. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Collection` as a Scala `Iterable`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped
   *  collection at all times.
   *
   *  @tparam A the type of the collection's elements
   *  @param underlying the wrapped Java collection
   */
  @SerialVersionUID(3L)
  class JCollectionWrapper[A](val underlying: ju.Collection[A])
    extends AbstractIterable[A]
      with StrictOptimizedIterableOps[A, Iterable, Iterable[A]]
      with Serializable {
    /** Returns a Scala iterator over the wrapped collection. */
    def iterator: Iterator[A] = underlying.iterator.asScala
    /** Returns the number of elements in the wrapped collection. */
    override def size = underlying.size
    /** Returns `0` if the wrapped collection is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    /** Returns `true` if the wrapped collection is empty. */
    override def isEmpty = underlying.isEmpty
    /** Returns the factory used to build transformed collections:
     *  transformations produce `mutable.ArrayBuffer`s rather than new wrappers.
     */
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    /** Returns `true` if `other` is a `JCollectionWrapper` with an equal
     *  underlying collection, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: JCollectionWrapper[?] => this.underlying == that.underlying
      case _ => false
    }
    /** Returns the hash code of the underlying collection. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Scala `Seq` as a Java `List`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped sequence
   *  at all times. Modification through the Java interface is not supported;
   *  the mutating methods inherited from `java.util.AbstractList` throw
   *  `UnsupportedOperationException`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param underlying the wrapped Scala sequence
   */
  @SerialVersionUID(3L)
  class SeqWrapper[A](val underlying: Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    /** Returns the element at index `i` of the wrapped sequence.
     *
     *  @param i the index
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def get(i: Int): A = underlying(i)
  }

  /** Wraps a Scala `mutable.Seq` as a Java `List`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped sequence
   *  at all times, and replacing an element with `set` writes through to the
   *  wrapped sequence. Structural modification (adding or removing elements)
   *  is not supported; those methods, inherited from `java.util.AbstractList`,
   *  throw `UnsupportedOperationException`.
   *
   *  @tparam A the type of the sequence's elements
   *  @param underlying the wrapped Scala sequence
   */
  @SerialVersionUID(3L)
  class MutableSeqWrapper[A](val underlying: mutable.Seq[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    /** Returns the element at index `i` of the wrapped sequence.
     *
     *  @param i the index
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def get(i: Int): A = underlying(i)
    /** Replaces the element at index `i` of the wrapped sequence with `elem`.
     *
     *  @param i the index
     *  @param elem the new element
     *  @return the element previously at index `i`
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    override def set(i: Int, elem: A): A = {
      val p = underlying(i)
      underlying(i) = elem
      p
    }
  }

  /** Wraps a Scala `mutable.Buffer` as a Java `List`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other. Supports element access and replacement, appending,
   *  and removal by index.
   *
   *  @tparam A the type of the buffer's elements
   *  @param underlying the wrapped Scala buffer
   */
  @SerialVersionUID(3L)
  class MutableBufferWrapper[A](val underlying: mutable.Buffer[A]) extends ju.AbstractList[A] with IterableWrapperTrait[A] with Serializable {
    /** Returns the element at index `i` of the wrapped buffer.
     *
     *  @param i the index
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def get(i: Int): A = underlying(i)
    /** Replaces the element at index `i` of the wrapped buffer with `elem`.
     *
     *  @param i the index
     *  @param elem the new element
     *  @return the element previously at index `i`
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    override def set(i: Int, elem: A): A = { val p = underlying(i); underlying(i) = elem; p }
    /** Appends `elem` to the end of the wrapped buffer and returns `true`.
     *
     *  @param elem the element to append
     */
    override def add(elem: A) = { underlying += elem; true }
    /** Removes and returns the element at index `i` of the wrapped buffer.
     *
     *  @param i the index
     *  @return the removed element
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    override def remove(i: Int): A = underlying remove i
  }

  /** Wraps a Java `List` as a Scala `mutable.Buffer`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other.
   *
   *  @tparam A the type of the list's elements
   *  @param underlying the wrapped Java list
   */
  @SerialVersionUID(3L)
  class JListWrapper[A](val underlying: ju.List[A])
    extends mutable.AbstractBuffer[A]
      with SeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with StrictOptimizedSeqOps[A, mutable.Buffer, mutable.Buffer[A]]
      with IterableFactoryDefaults[A, mutable.Buffer]
      with Serializable {
    /** Returns the number of elements in the wrapped list. */
    def length = underlying.size
    /** Returns `0` if the wrapped list is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    /** Returns `true` if the wrapped list is empty. */
    override def isEmpty = underlying.isEmpty
    /** Returns a Scala iterator over the wrapped list. */
    override def iterator: Iterator[A] = underlying.iterator.asScala
    /** Returns the element at index `i` of the wrapped list.
     *
     *  @param i the index
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def apply(i: Int): A = underlying.get(i)
    /** Replaces the element at index `i` of the wrapped list with `elem`.
     *
     *  @param i the index
     *  @param elem the new element
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def update(i: Int, elem: A) = underlying.set(i, elem)
    /** Prepends `elem` to the wrapped list and returns this buffer.
     *
     *  @param elem the element to prepend
     */
    def prepend(elem: A) = { underlying.subList(0, 0).add(elem); this }
    /** Appends `elem` to the end of the wrapped list.
     *
     *  @param elem the element to append
     *  @return this buffer
     */
    def addOne(elem: A): this.type = { underlying.add(elem); this }
    /** Inserts `elem` at index `idx` of the wrapped list, shifting the
     *  elements at and after that index.
     *
     *  @param idx the index at which to insert
     *  @param elem the element to insert
     *  @throws IndexOutOfBoundsException if `idx` is negative or greater than the size
     */
    def insert(idx: Int,elem: A): Unit = underlying.subList(0, idx).add(elem)
    /** Inserts all elements of `elems` at index `i` of the wrapped list,
     *  shifting the elements at and after that index.
     *
     *  @param i the index at which to insert
     *  @param elems the elements to insert
     *  @throws IndexOutOfBoundsException if `i` is negative or greater than the size
     */
    def insertAll(i: Int, elems: IterableOnce[A]^) = {
      val ins = underlying.subList(0, i)
      elems.iterator.foreach(ins.add(_))
    }
    /** Removes and returns the element at index `i` of the wrapped list.
     *
     *  @param i the index
     *  @throws IndexOutOfBoundsException if `i` is out of range
     */
    def remove(i: Int) = underlying.remove(i)
    /** Removes all elements from the wrapped list. */
    def clear() = underlying.clear()
    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    /** Returns a copy of this buffer: a wrapper around a new `java.util.ArrayList`
     *  containing the same elements. The copy is independent of the wrapped list.
     */
    override def clone(): JListWrapper[A] = new JListWrapper(new ju.ArrayList[A](underlying))
    /** Replaces `replaced` elements of the wrapped list, starting at index
     *  `from`, with the elements of `patch`.
     *
     *  @param from the index of the first element to replace
     *  @param patch the elements to insert in place of the removed ones
     *  @param replaced the number of elements to remove
     *  @return this buffer
     */
    def patchInPlace(from: Int, patch: scala.collection.IterableOnce[A]^, replaced: Int): this.type = {
      remove(from, replaced)
      insertAll(from, patch)
      this
    }
    /** Removes `n` elements from the wrapped list, starting at index `from`.
     *
     *  @param from the index of the first element to remove
     *  @param n the number of elements to remove
     *  @throws IndexOutOfBoundsException if the range is out of bounds
     *  @throws IllegalArgumentException if `n` is negative
     */
    def remove(from: Int, n: Int): Unit = underlying.subList(from, from+n).clear()
    /** Returns the factory used to build transformed collections:
     *  transformations produce `mutable.ArrayBuffer`s rather than new wrappers.
     */
    override def iterableFactory: mutable.ArrayBuffer.type = mutable.ArrayBuffer
    /** Removes the first occurrence of `elem` from the wrapped list, if any.
     *
     *  @param elem the element to remove
     *  @return this buffer
     */
    override def subtractOne(elem: A): this.type = { underlying.remove(elem.asInstanceOf[AnyRef]); this }
  }

  /** Wraps a Scala `Set` as a Java `Set`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped set at
   *  all times. Adding elements through the Java interface is not supported;
   *  the iterator's `remove` works only when the wrapped set is a
   *  `mutable.Set`.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Scala set
   */
  @SerialVersionUID(3L)
  class SetWrapper[A](underlying: Set[A]) extends ju.AbstractSet[A] with Serializable { self =>
    // Note various overrides to avoid performance gotchas.
    /** Returns `true` if the wrapped set contains `o`.
     *
     *  Returns `false`, rather than throwing, when `o` is incompatible with
     *  the set's element type.
     *
     *  @param o the object to test for membership
     */
    override def contains(o: Object): Boolean = {
      try { underlying.contains(o.asInstanceOf[A]) }
      catch { case cce: ClassCastException => false }
    }
    /** Returns `true` if the wrapped set is empty. */
    override def isEmpty = underlying.isEmpty
    /** Returns the number of elements in the wrapped set. */
    def size = underlying.size
    /** Returns a Java iterator over the wrapped set.
     *
     *  The iterator's `remove` removes the last returned element from the
     *  wrapped set if that set is a `mutable.Set`, and throws
     *  `UnsupportedOperationException` otherwise; calling `remove` before
     *  `next` throws `IllegalStateException`.
     */
    def iterator: ju.Iterator[A] = new ju.Iterator[A] {
      val ui = underlying.iterator
      var prev: Option[A] = None
      def hasNext = ui.hasNext
      def next: A = { val e = ui.next(); prev = Some(e); e }
      override def remove() = prev match {
        case Some(e) =>
          underlying match {
            case ms: mutable.Set[a] =>
              ms remove e
              prev = None
            case _ =>
              throw new UnsupportedOperationException("remove")
          }
        case _ =>
          throw new IllegalStateException("next must be called at least once before remove")
      }
    }
  }

  /** Wraps a Scala `mutable.Set` as a Java `Set`.
   *
   *  The wrapper is a view supporting addition, removal, and clearing:
   *  changes made through either interface are visible through the other.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Scala set
   */
  @SerialVersionUID(3L)
  class MutableSetWrapper[A](val underlying: mutable.Set[A]) extends SetWrapper[A](underlying) with Serializable {
    /** Adds `elem` to the wrapped set and returns `true` if it was not
     *  already present.
     *
     *  @param elem the element to add
     */
    override def add(elem: A) = {
      val sz = underlying.size
      underlying += elem
      sz < underlying.size
    }
    /** Removes `elem` from the wrapped set and returns `true` if it was
     *  present.
     *
     *  Returns `false`, rather than throwing, when `elem` is incompatible
     *  with the set's element type.
     *
     *  @param elem the element to remove
     */
    override def remove(elem: AnyRef) =
      try underlying.remove(elem.asInstanceOf[A])
      catch { case ex: ClassCastException => false }
    /** Removes all elements from the wrapped set. */
    override def clear() = underlying.clear()
  }

  /** Wraps a Java `Set` as a Scala `mutable.Set`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other.
   *
   *  @tparam A the type of the set's elements
   *  @param underlying the wrapped Java set
   */
  @SerialVersionUID(3L)
  class JSetWrapper[A](val underlying: ju.Set[A])
    extends mutable.AbstractSet[A]
      with mutable.SetOps[A, mutable.Set, mutable.Set[A]]
      with StrictOptimizedSetOps[A, mutable.Set, mutable.Set[A]]
      with Serializable {

    /** Returns the number of elements in the wrapped set. */
    override def size: Int = underlying.size
    /** Returns `true` if the wrapped set is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
    /** Returns `0` if the wrapped set is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    /** Returns a Scala iterator over the wrapped set. */
    def iterator: Iterator[A] = underlying.iterator.asScala

    /** Returns `true` if the wrapped set contains `elem`.
     *
     *  @param elem the element to test for membership
     */
    def contains(elem: A): Boolean = underlying.contains(elem)

    /** Adds `elem` to the wrapped set.
     *
     *  @param elem the element to add
     *  @return this set
     */
    def addOne(elem: A): this.type = { underlying.add(elem); this }
    /** Removes `elem` from the wrapped set, if present.
     *
     *  @param elem the element to remove
     *  @return this set
     */
    def subtractOne(elem: A): this.type = { underlying.remove(elem); this }

    /** Removes `elem` from the wrapped set.
     *
     *  @param elem the element to remove
     *  @return `true` if the element was present
     */
    override def remove(elem: A): Boolean = underlying.remove(elem)

    /** Removes all elements from the wrapped set. */
    override def clear(): Unit = {
      underlying.clear()
    }

    /** Returns an empty set of the same kind: a wrapper around a new, empty `java.util.HashSet`. */
    override def empty: mutable.Set[A] = new JSetWrapper(new ju.HashSet[A])

    // Note: Clone cannot just call underlying.clone because in Java, only specific collections
    // expose clone methods.  Generically, they're protected.
    /** Returns a copy of this set: a wrapper around a new `java.util.LinkedHashSet`
     *  containing the same elements. The copy is independent of the wrapped set.
     */
    override def clone(): mutable.Set[A] = new JSetWrapper[A](new ju.LinkedHashSet[A](underlying))

    /** Returns the factory used to build transformed collections:
     *  transformations produce `mutable.HashSet`s rather than new wrappers.
     */
    override def iterableFactory: IterableFactory[mutable.Set] = mutable.HashSet

    /** Removes from the wrapped set all elements that do not satisfy `p`.
     *
     *  @param p the predicate that retained elements must satisfy
     *  @return this set
     */
    override def filterInPlace(p: A => Boolean): this.type = {
      if (underlying.size() > 0) underlying.removeIf(!p(_))
      this
    }
  }

  /** Wraps a Scala `Map` as a Java `Map`.
   *
   *  The wrapper is a view: it reflects the contents of the wrapped map at
   *  all times. Modification through the Java interface is supported only
   *  when the wrapped map is mutable: `put`, `remove` and `clear` are added
   *  by [[MutableMapWrapper]], and the entry-set view's `remove` and
   *  `setValue` operations work only on a mutable wrapped map.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class MapWrapper[K, V](underlying: Map[K, V]) extends ju.AbstractMap[K, V] with Serializable {
    self: MapWrapper[K, V] =>
    /** Returns the number of entries in the wrapped map. */
    override def size = underlying.size

    /** Returns the value the wrapped map binds to `key`, or `null` if the
     *  key is absent or incompatible with the map's key type.
     *
     *  A `null` result does not distinguish an absent key from a key bound
     *  to `null`.
     *
     *  @param key the key to look up
     */
    override def get(key: AnyRef): V = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    /** Returns a set view of the entries of the wrapped map.
     *
     *  The view's entries follow the `java.util.Map.Entry` contract for
     *  `equals` and `hashCode`, and their `setValue` stores through this
     *  wrapper's `put`, which succeeds only where the wrapper supports it
     *  (see [[MutableMapWrapper]]). The view iterator's `remove` removes the
     *  last returned entry if the wrapped map is a `mutable.Map` and throws
     *  `UnsupportedOperationException` otherwise; calling it before `next`
     *  throws `IllegalStateException`.
     */
    override def entrySet: ju.Set[ju.Map.Entry[K, V]] = new ju.AbstractSet[ju.Map.Entry[K, V]] {
      def size = self.size

      def iterator: ju.Iterator[ju.Map.Entry[K, V]] = new ju.Iterator[ju.Map.Entry[K, V]] {
        val ui = underlying.iterator
        var prev : Option[K] = None

        def hasNext = ui.hasNext

        def next(): ju.Map.Entry[K, V] = {
          val (k, v) = ui.next()
          prev = Some(k)
          new ju.Map.Entry[K, V] {
            def getKey = k
            def getValue = v
            def setValue(v1 : V): V = self.put(k, v1)

            // It's important that this implementation conform to the contract
            // specified in the javadocs of java.util.Map.Entry.hashCode
            //
            // See https://github.com/scala/bug/issues/10663
            override def hashCode() =
              java.util.Objects.hashCode(k) ^ java.util.Objects.hashCode(v)

            override def equals(other: Any) = other match {
              case e: ju.Map.Entry[?, ?] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }

        override def remove(): Unit = {
          prev match {
            case Some(k) =>
              underlying match {
                case mm: mutable.Map[a, ?] =>
                  mm -= k
                  prev = None
                case _ =>
                  throw new UnsupportedOperationException("remove")
              }
            case _ =>
              throw new IllegalStateException("next must be called at least once before remove")
          }
        }
      }
    }

    /** Returns `true` if the wrapped map contains the key `key`.
     *
     *  Returns `false`, rather than throwing, when `key` is incompatible
     *  with the map's key type.
     *
     *  @param key the key to test for
     */
    override def containsKey(key: AnyRef): Boolean = try {
      // Note: Subclass of collection.Map with specific key type may redirect generic
      // contains to specific contains, which will throw a ClassCastException if the
      // wrong type is passed. This is why we need a type cast to A inside a try/catch.
      underlying.contains(key.asInstanceOf[K])
    } catch {
      case ex: ClassCastException => false
    }
  }

  /** Wraps a Scala `mutable.Map` as a Java `Map`.
   *
   *  Extends [[MapWrapper]] with the mutating operations `put`, `remove` and
   *  `clear`; changes made through either interface are visible through the
   *  other.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class MutableMapWrapper[K, V](val underlying: mutable.Map[K, V]) extends MapWrapper[K, V](underlying) {
    /** Associates `v` with the key `k` in the wrapped map.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     *  @return the value previously bound to `k`, or `null` if the key was
     *          absent (a key previously bound to `null` also yields `null`)
     */
    override def put(k: K, v: V): V = underlying.put(k, v) match {
      case Some(v1) => v1
      case None => null.asInstanceOf[V]
    }

    /** Removes the binding for `k` from the wrapped map.
     *
     *  @param k the key to remove
     *  @return the value previously bound to `k`, or `null` if the key was
     *          absent or incompatible with the map's key type
     */
    override def remove(k: AnyRef): V = try {
      underlying remove k.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    /** Removes all entries from the wrapped map. */
    override def clear() = underlying.clear()
  }

  /** Abstract superclass of the wrappers that expose a Java `Map` as a Scala
   *  `mutable.Map`; the shared implementation lives in [[JMapWrapperLike]].
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   */
  @SerialVersionUID(3L)
  abstract class AbstractJMapWrapper[K, V]
    extends mutable.AbstractMap[K, V]
      with JMapWrapperLike[K, V, mutable.Map, mutable.Map[K, V]] with Serializable

  /** Implements a Scala `mutable.Map` in terms of an underlying Java `Map`.
   *
   *  Operations delegate to the wrapped map, so the result is a view: changes
   *  made through either interface are visible through the other. Where the
   *  wrapped map permits `null` values, a key bound to `null` is reported as
   *  `Some(null)` by `get`, `put` and `remove`.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @tparam CC the type constructor of the map returned by transformation operations
   *  @tparam C the type of the map returned by operations that preserve the key and value types
   */
  trait JMapWrapperLike[K, V, +CC[X, Y] <: mutable.MapOps[X, Y, CC, ?], +C <: mutable.MapOps[K, V, CC, C]]
    extends mutable.MapOps[K, V, CC, C]
      with StrictOptimizedMapOps[K, V, CC, C]
      with StrictOptimizedIterableOps[(K, V), mutable.Iterable, C] {

    /** The wrapped Java map. */
    def underlying: ju.Map[K, V]

    /** Returns the number of entries in the wrapped map. */
    override def size = underlying.size

    // support Some(null) if currently bound to null
    /** Returns `Some` of the value bound to `k` in the wrapped map, or `None`
     *  if the key is absent.
     *
     *  A key bound to `null` yields `Some(null)`, distinguished from an
     *  absent key by an additional `containsKey` check; the lookup and the
     *  check are not atomic.
     *
     *  @param k the key to look up
     */
    def get(k: K) = {
      val v = underlying.get(k)
      if (v != null)
        Some(v)
      else if (underlying.containsKey(k))
        Some(null.asInstanceOf[V])
      else
        None
    }

    /** Returns the value bound to `key`; if the key is absent (or bound to
     *  `null`), evaluates `op`, stores its result, and returns it.
     *
     *  Delegates to the wrapped map's `computeIfAbsent`; because that method
     *  does not store `null` results, a `null` computed by `op` is stored
     *  with a separate update.
     *
     *  @param key the key to look up
     *  @param op the value to compute if no non-null value is bound to `key`; evaluated at most once
     */
    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => update(key, null.asInstanceOf[V]); null.asInstanceOf[V]
        case v    => v
      }

    /** Adds the binding `kv` to the wrapped map, replacing any existing
     *  binding for its key.
     *
     *  @param kv the key/value pair to add
     *  @return this map
     */
    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    /** Removes the binding for `key` from the wrapped map, if any.
     *
     *  @param key the key to remove
     *  @return this map
     */
    def subtractOne(key: K): this.type = { underlying.remove(key); this }

    // support Some(null) if currently bound to null
    /** Associates `v` with the key `k` in the wrapped map.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     *  @return the value previously bound to `k`: `Some(null)` if the key
     *          was bound to `null`, `None` if it was absent
     */
    override def put(k: K, v: V): Option[V] =
      if (v == null) {
        val present = underlying.containsKey(k)
        val result  = underlying.put(k, v)
        if (present) Some(result) else None
      } else {
        var result: Option[V] = None
        def recompute(k0: K, v0: V): V = v.tap(_ =>
          if (v0 != null) result = Some(v0)
          else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        )
        underlying.compute(k, recompute)
        result
      }

    /** Associates `v` with the key `k` in the wrapped map, discarding any
     *  previous binding.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     */
    override def update(k: K, v: V): Unit = underlying.put(k, v)

    /** Updates the binding for `key` using `remappingFunction` applied to the
     *  currently bound value, `None` if the key is absent or bound to `null`.
     *
     *  A result of `Some(v)` stores `v`; `None` removes the binding.
     *  Delegates to the wrapped map's `compute`, except that a `Some(null)`
     *  result is stored with a separate update.
     *
     *  @param key the key whose binding to update
     *  @param remappingFunction the function computing the new binding from the current one
     *  @return the value now bound to `key`, or `None` if the binding was
     *          removed or the key was absent
     */
    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => update(key, null.asInstanceOf[V]); Some(null.asInstanceOf[V])
      }
    }

    // support Some(null) if currently bound to null
    /** Removes the binding for `k` from the wrapped map.
     *
     *  @param k the key to remove
     *  @return the value previously bound to `k`: `Some(null)` if the key
     *          was bound to `null`, `None` if it was absent
     */
    override def remove(k: K): Option[V] = {
      var result: Option[V] = None
      def recompute(k0: K, v0: V): V = {
        if (v0 != null) result = Some(v0)
        else if (underlying.containsKey(k0)) result = Some(null.asInstanceOf[V])
        null.asInstanceOf[V]
      }
      underlying.compute(k, recompute)
      result
    }

    /** Returns an iterator over the key/value pairs of the wrapped map, in
     *  its entry-set order.
     */
    def iterator: Iterator[(K, V)] = new AbstractIterator[(K, V)] {
      val ui: java.util.Iterator[java.util.Map.Entry[K, V]] = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = { val e = ui.next(); (e.getKey, e.getValue) }
    }

    /** Applies `f` to each key/value pair of the wrapped map.
     *
     *  @tparam U the result type of `f`; the results are discarded
     *  @param f the function to apply to each key and value
     */
    override def foreachEntry[U](f: (K, V) => U): Unit = {
      val i = underlying.entrySet().iterator()
      while (i.hasNext) {
        val entry = i.next()
        f(entry.getKey, entry.getValue)
      }
    }

    /** Removes all entries from the wrapped map. */
    override def clear() = underlying.clear()

  }

  /** Wraps a Java map as a Scala one.  If the map is to support concurrent access,
    * use [[JConcurrentMapWrapper]] instead.  If the wrapped map is synchronized
    * (e.g. from `java.util.Collections.synchronizedMap`), it is your responsibility
    * to wrap all non-atomic operations with `underlying.synchronized`.
    * This includes `get`, as `java.util.Map`'s API does not allow for an
    * atomic `get` when `null` values may be present.
    */
  @SerialVersionUID(3L)
  class JMapWrapper[K, V](val underlying : ju.Map[K, V])
    extends AbstractJMapWrapper[K, V] with Serializable {

    /** Returns `true` if the wrapped map is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
    /** Returns `0` if the wrapped map is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    /** Returns an empty map of the same kind: a wrapper around a new, empty `java.util.HashMap`. */
    override def empty: JMapWrapper[K, V] = new JMapWrapper(new ju.HashMap[K, V])
  }

  /** Wraps a Scala `concurrent.Map` as a Java `java.util.concurrent.ConcurrentMap`.
   *
   *  The wrapper is a view: changes made through either interface are visible
   *  through the other. The single-entry operations below delegate to the
   *  wrapped map's atomic operations of the same name.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala concurrent map
   */
  @SerialVersionUID(3L)
  class ConcurrentMapWrapper[K, V](underlying: concurrent.Map[K, V]) extends MutableMapWrapper[K, V](underlying) with juc.ConcurrentMap[K, V] {

    /** Returns the wrapped map, typed as a `concurrent.Map`. */
    def underlyingConcurrentMap: concurrent.Map[K, V] = underlying

    /** Associates `v` with the key `k` in the wrapped map, unless `k` is
     *  already bound, delegating to the wrapped map's atomic `putIfAbsent`.
     *
     *  @param k the key
     *  @param v the value to bind to `k` if it is unbound
     *  @return the value already bound to `k`, or `null` if the key was
     *          absent and the new binding was added
     */
    override def putIfAbsent(k: K, v: V): V = underlying.putIfAbsent(k, v).getOrElse(null.asInstanceOf[V])

    /** Removes the entry for `k` if it is currently bound to `v`, delegating
     *  to the wrapped map's atomic conditional `remove`.
     *
     *  Returns `true` if the entry was removed; returns `false` otherwise,
     *  including when `k` or `v` is incompatible with the map's key or value
     *  type.
     *
     *  @param k the key to remove
     *  @param v the value the key must currently be bound to
     */
    override def remove(k: AnyRef, v: AnyRef) =
      try underlying.remove(k.asInstanceOf[K], v.asInstanceOf[V])
      catch { case ex: ClassCastException => false }

    /** Replaces the value bound to `k` with `v`, but only if `k` is
     *  currently bound, delegating to the wrapped map's atomic `replace`.
     *
     *  @param k the key
     *  @param v the new value
     *  @return the value previously bound to `k`, or `null` if the key was
     *          absent and nothing was changed
     */
    override def replace(k: K, v: V): V = underlying.replace(k, v).getOrElse(null.asInstanceOf[V])

    /** Replaces the value bound to `k` with `newval`, but only if `k` is
     *  currently bound to `oldval`, delegating to the wrapped map's atomic
     *  conditional `replace`. Returns `true` if the value was replaced.
     *
     *  @param k the key
     *  @param oldval the value the key must currently be bound to
     *  @param newval the new value
     */
    override def replace(k: K, oldval: V, newval: V) = underlying.replace(k, oldval, newval)
  }

  /** Wraps a concurrent Java map as a Scala one.  Single-element concurrent
   *  access is supported; multi-element operations such as maps and filters
   *  are not guaranteed to be atomic.
   */
  @SerialVersionUID(3L)
  class JConcurrentMapWrapper[K, V](val underlying: juc.ConcurrentMap[K, V])
    extends AbstractJMapWrapper[K, V]
      with concurrent.Map[K, V] {

    /** Returns `Some` of the value bound to `k` in the wrapped map, or
     *  `None` if the key is absent.
     *
     *  A single lookup of the wrapped map; if the wrapped map permits `null`
     *  values, a key bound to `null` is indistinguishable from an absent key
     *  and yields `None`.
     *
     *  @param k the key to look up
     */
    override def get(k: K) = Option(underlying.get(k))

    /** Returns the value bound to `key`; if the key is absent, evaluates
     *  `op`, stores its result, and returns it.
     *
     *  Delegates to the wrapped map's `computeIfAbsent`, which is atomic per
     *  the `ConcurrentMap` contract; if `op` returns `null`, falls back to
     *  the default non-atomic check-then-act implementation.
     *
     *  @param key the key to look up
     *  @param op the value to compute if `key` is absent; may be evaluated a
     *            second time if it returns `null`
     */
    override def getOrElseUpdate(key: K, op: => V): V =
      underlying.computeIfAbsent(key, _ => op) match {
        case null => super/*[concurrent.Map]*/.getOrElseUpdate(key, op)
        case v    => v
      }

    /** Returns `true` if the wrapped map is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
    /** Returns `0` if the wrapped map is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize
    /** Returns an empty map of the same kind: a wrapper around a new, empty
     *  `java.util.concurrent.ConcurrentHashMap`.
     */
    override def empty: JConcurrentMapWrapper[K, V] = new JConcurrentMapWrapper(new juc.ConcurrentHashMap[K, V])

    /** Associates `v` with the key `k` in the wrapped map, unless `k` is
     *  already bound, delegating to the wrapped map's atomic `putIfAbsent`.
     *
     *  @param k the key
     *  @param v the value to bind to `k` if it is unbound
     *  @return `Some` of the value already bound to `k`, or `None` if the
     *          key was absent and the new binding was added
     */
    def putIfAbsent(k: K, v: V): Option[V] = Option(underlying.putIfAbsent(k, v))

    /** Removes the entry for `k` if it is currently bound to `v`, delegating
     *  to the wrapped map's atomic conditional `remove`.
     *
     *  @param k the key to remove
     *  @param v the value the key must currently be bound to
     *  @return `true` if the entry was removed
     */
    def remove(k: K, v: V): Boolean = underlying.remove(k, v)

    /** Replaces the value bound to `k` with `v`, but only if `k` is
     *  currently bound, delegating to the wrapped map's atomic `replace`.
     *
     *  @param k the key
     *  @param v the new value
     *  @return `Some` of the value previously bound to `k`, or `None` if the
     *          key was absent and nothing was changed
     */
    def replace(k: K, v: V): Option[V] = Option(underlying.replace(k, v))

    /** Replaces the value bound to `k` with `newvalue`, but only if `k` is
     *  currently bound to `oldvalue`, delegating to the wrapped map's atomic
     *  conditional `replace`.
     *
     *  @param k the key
     *  @param oldvalue the value the key must currently be bound to
     *  @param newvalue the new value
     *  @return `true` if the value was replaced
     */
    def replace(k: K, oldvalue: V, newvalue: V): Boolean = underlying.replace(k, oldvalue, newvalue)

    /** Returns the last entry of the wrapped map, or `None` if the map is
     *  empty.
     *
     *  For a `NavigableMap` this is its `lastEntry` (the entry with the
     *  greatest key); otherwise it is the last entry in iteration order, and
     *  `None` is returned if the traversal fails, for example because the
     *  map was concurrently emptied.
     */
    override def lastOption: Option[(K, V)] =
      underlying match {
        case nav: NavigableMap[K @unchecked, V @unchecked] => Option(nav.lastEntry).map(e => (e.getKey, e.getValue))
        case _ if isEmpty => None
        case _ => Try(last).toOption
      }

    /** Updates the binding for `key` using `remappingFunction` applied to
     *  the currently bound value, `None` if the key is absent.
     *
     *  A result of `Some(v)` stores `v`; `None` removes the binding.
     *  Delegates to the wrapped map's `compute`, which is atomic per the
     *  `ConcurrentMap` contract, except that a `Some(null)` result falls
     *  back to the default non-atomic implementation.
     *
     *  @param key the key whose binding to update
     *  @param remappingFunction the function computing the new binding from the current one
     *  @return the value now bound to `key`, or `None` if the binding was
     *          removed or the key was absent
     */
    override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
      def remap(k: K, v: V): V =
        remappingFunction(Option(v)) match {
          case Some(null) => throw PutNull // see scala/scala#10129
          case Some(x)    => x
          case None       => null.asInstanceOf[V]
        }
      try Option(underlying.compute(key, remap))
      catch {
        case PutNull => super/*[concurrent.Map]*/.updateWith(key)(remappingFunction)
      }
    }
  }

  /** Wraps a Scala `mutable.Map` as a Java `Dictionary`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other.
   *
   *  @tparam K the type of the map's keys
   *  @tparam V the type of the map's values
   *  @param underlying the wrapped Scala map
   */
  @SerialVersionUID(3L)
  class DictionaryWrapper[K, V](val underlying: mutable.Map[K, V]) extends ju.Dictionary[K, V] with Serializable {
    /** Returns the number of entries in the wrapped map. */
    def size: Int = underlying.size
    /** Returns `true` if the wrapped map is empty. */
    def isEmpty: Boolean = underlying.isEmpty
    /** Returns an enumeration of the keys of the wrapped map. */
    def keys: ju.Enumeration[K] = underlying.keysIterator.asJavaEnumeration
    /** Returns an enumeration of the values of the wrapped map. */
    def elements: ju.Enumeration[V] = underlying.valuesIterator.asJavaEnumeration
    /** Returns the value the wrapped map binds to `key`, or `null` if the
     *  key is absent or incompatible with the map's key type.
     *
     *  A `null` result does not distinguish an absent key from a key bound
     *  to `null`.
     *
     *  @param key the key to look up
     */
    def get(key: AnyRef): V = try {
      underlying get key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }
    /** Associates `value` with the key `key` in the wrapped map.
     *
     *  @param key the key
     *  @param value the value to bind to `key`
     *  @return the value previously bound to `key`, or `null` if the key
     *          was absent (a key previously bound to `null` also yields `null`)
     */
    def put(key: K, value: V): V = underlying.put(key, value) match {
      case Some(v) => v
      case None => null.asInstanceOf[V]
    }
    /** Removes the binding for `key` from the wrapped map.
     *
     *  @param key the key to remove
     *  @return the value previously bound to `key`, or `null` if the key
     *          was absent or incompatible with the map's key type
     */
    override def remove(key: AnyRef): V = try {
      underlying remove key.asInstanceOf[K] match {
        case None => null.asInstanceOf[V]
        case Some(v) => v
      }
    } catch {
      case ex: ClassCastException => null.asInstanceOf[V]
    }

    /** Returns `true` if `other` is a `DictionaryWrapper` with an equal
     *  underlying map, `false` otherwise.
     *
     *  @param other the object to compare with
     */
    override def equals(other: Any): Boolean = other match {
      case that: DictionaryWrapper[?, ?] => this.underlying == that.underlying
      case _ => false
    }

    /** Returns the hash code of the underlying map. */
    override def hashCode(): Int = underlying.hashCode()
  }

  /** Wraps a Java `Dictionary` as a Scala `mutable.Map`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other. (`Dictionary` permits neither `null` keys
   *  nor `null` values.)
   *
   *  @tparam K the type of the dictionary's keys
   *  @tparam V the type of the dictionary's values
   *  @param underlying the wrapped Java dictionary
   */
  @SerialVersionUID(3L)
  class JDictionaryWrapper[K, V](val underlying: ju.Dictionary[K, V]) extends mutable.AbstractMap[K, V] with Serializable {
    /** Returns the number of entries in the wrapped dictionary. */
    override def size: Int = underlying.size
    /** Returns `true` if the wrapped dictionary is empty. */
    override def isEmpty: Boolean = underlying.isEmpty
    /** Returns `0` if the wrapped dictionary is empty, otherwise `-1` (unknown). */
    override def knownSize: Int = if (underlying.isEmpty) 0 else super.knownSize

    /** Returns `Some` of the value bound to `k` in the wrapped dictionary,
     *  or `None` if the key is absent.
     *
     *  @param k the key to look up
     */
    def get(k: K) = Option(underlying.get(k))

    /** Adds the binding `kv` to the wrapped dictionary, replacing any
     *  existing binding for its key.
     *
     *  @param kv the key/value pair to add
     *  @return this map
     */
    def addOne(kv: (K, V)): this.type = { underlying.put(kv._1, kv._2); this }
    /** Removes the binding for `key` from the wrapped dictionary, if any.
     *
     *  @param key the key to remove
     *  @return this map
     */
    def subtractOne(key: K): this.type = { underlying.remove(key); this }

    /** Associates `v` with the key `k` in the wrapped dictionary.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     *  @return `Some` of the value previously bound to `k`, or `None` if
     *          the key was absent
     */
    override def put(k: K, v: V): Option[V] = Option(underlying.put(k, v))

    /** Associates `v` with the key `k` in the wrapped dictionary, discarding
     *  any previous binding.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     */
    override def update(k: K, v: V): Unit = { underlying.put(k, v) }

    /** Removes the binding for `k` from the wrapped dictionary.
     *
     *  @param k the key to remove
     *  @return `Some` of the value previously bound to `k`, or `None` if
     *          the key was absent
     */
    override def remove(k: K): Option[V] = Option(underlying.remove(k))
    /** Returns an iterator over the key/value pairs of the wrapped
     *  dictionary, obtained by enumerating its keys and looking each one up.
     */
    def iterator = underlying.keys.asScala map (k => (k, underlying.get(k)))

    /** Removes all entries from the wrapped dictionary, by removing each key in turn. */
    override def clear() = iterator.foreach(entry => underlying.remove(entry._1))

    /** Returns the factory used to build transformed maps: transformations
     *  produce `mutable.HashMap`s rather than new wrappers.
     */
    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  /** Wraps a `java.util.Properties` object as a Scala
   *  `mutable.Map[String, String]`.
   *
   *  The wrapper is a view: changes made through either interface are
   *  visible through the other. The `Map` operations access only the
   *  properties object's own entries, which are assumed to be strings; any
   *  default properties are consulted only by `getProperty`.
   *
   *  @param underlying the wrapped properties object
   */
  @SerialVersionUID(3L)
  class JPropertiesWrapper(underlying: ju.Properties)
    extends mutable.AbstractMap[String, String]
      with mutable.MapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedMapOps[String, String, mutable.Map, mutable.Map[String, String]]
      with StrictOptimizedIterableOps[(String, String), mutable.Iterable, mutable.Map[String, String]]
      with Serializable {

    /** Returns the number of entries in the wrapped properties object, not counting defaults. */
    override def size = underlying.size
    /** Returns `true` if the wrapped properties object has no entries of its own. */
    override def isEmpty: Boolean = underlying.isEmpty
    /** Returns the number of entries; the size is always known. */
    override def knownSize: Int = size
    /** Returns `Some` of the value bound to the key `k` among the wrapped
     *  properties object's own entries, or `None` if the key is absent;
     *  defaults are not consulted.
     *
     *  @param k the key to look up
     */
    def get(k: String) = {
      val v = underlying.get(k)
      if (v != null) Some(v.asInstanceOf[String]) else None
    }

    /** Adds the binding `kv` to the wrapped properties object, replacing any
     *  existing binding for its key.
     *
     *  @param kv the key/value pair to add
     *  @return this map
     */
    def addOne(kv: (String, String)): this.type = { underlying.put(kv._1, kv._2); this }
    /** Removes the binding for `key` from the wrapped properties object, if any.
     *
     *  @param key the key to remove
     *  @return this map
     */
    def subtractOne(key: String): this.type = { underlying.remove(key); this }

    /** Associates `v` with the key `k` in the wrapped properties object.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     *  @return `Some` of the value previously bound to `k`, or `None` if
     *          the key was absent
     */
    override def put(k: String, v: String): Option[String] = {
      val r = underlying.put(k, v)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    /** Associates `v` with the key `k` in the wrapped properties object,
     *  discarding any previous binding.
     *
     *  @param k the key
     *  @param v the value to bind to `k`
     */
    override def update(k: String, v: String): Unit = { underlying.put(k, v) }

    /** Removes the binding for `k` from the wrapped properties object.
     *
     *  @param k the key to remove
     *  @return `Some` of the value previously bound to `k`, or `None` if
     *          the key was absent
     */
    override def remove(k: String): Option[String] = {
      val r = underlying.remove(k)
      if (r != null) Some(r.asInstanceOf[String]) else None
    }

    /** Returns an iterator over the wrapped properties object's own
     *  key/value pairs, cast to strings.
     */
    def iterator: Iterator[(String, String)] = new AbstractIterator[(String, String)] {
      val ui: java.util.Iterator[java.util.Map.Entry[Object, Object]] = underlying.entrySet.iterator
      def hasNext = ui.hasNext
      def next() = {
        val e = ui.next()
        (e.getKey.asInstanceOf[String], e.getValue.asInstanceOf[String])
      }
    }

    /** Removes all entries from the wrapped properties object. */
    override def clear() = underlying.clear()

    /** Returns an empty map of the same kind: a wrapper around a new, empty
     *  `java.util.Properties` object.
     */
    override def empty: JPropertiesWrapper = new JPropertiesWrapper(new ju.Properties)

    /** Returns the value of the property `key`, consulting the wrapped
     *  properties object's defaults, or `null` if the property is not found.
     *
     *  @param key the property key
     */
    def getProperty(key: String): String | Null = underlying.getProperty(key)

    /** Returns the value of the property `key`, consulting the wrapped
     *  properties object's defaults, or `defaultValue` if the property is
     *  not found.
     *
     *  @param key the property key
     *  @param defaultValue the value to return if the property is not found
     */
    def getProperty(key: String, defaultValue: String): String = underlying.getProperty(key, defaultValue)

    /** Sets the property `key` to `value` in the wrapped properties object.
     *
     *  @param key the property key
     *  @param value the value to set
     *  @return the value previously bound to `key`, or `null` if there was none
     */
    def setProperty(key: String, value: String): AnyRef | Null =
      underlying.setProperty(key, value)

    /** Returns the factory used to build transformed maps: transformations
     *  produce `mutable.HashMap`s rather than new wrappers.
     */
    override def mapFactory: mutable.HashMap.type = mutable.HashMap
  }

  /** Thrown when certain Map operations attempt to put a null value. */
  private val PutNull = new ControlThrowable {}
}
