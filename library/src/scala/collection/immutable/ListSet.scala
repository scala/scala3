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

import mutable.{Builder, ImmutableBuilder}
import scala.annotation.tailrec
import scala.collection.generic.DefaultSerializable

/** This class implements immutable sets using a list-based data structure. List set iterators and
 *  traversal methods visit elements in the order they were first inserted.
 *
 *  Elements are stored internally in reversed insertion order, which means the newest element is at
 *  the head of the list. As such, methods such as `head` and `tail` are O(n), while `last` and
 *  `init` are O(1). Other operations, such as inserting or removing entries, are also O(n), which
 *  makes this collection suitable only for a small number of elements.
 *
 *  Instances of `ListSet` represent empty sets; they can be either created by calling the
 *  constructor directly, or by applying the function `ListSet.empty`.
 *
 *  @tparam A the type of the elements contained in this list set
 *
 *  @define Coll ListSet
 *  @define coll list set
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
sealed class ListSet[A]
  extends AbstractSet[A]
    with StrictOptimizedSetOps[A, ListSet, ListSet[A]]
    with IterableFactoryDefaults[A, ListSet]
    with DefaultSerializable {

  /** The prefix of this set's string representation: `"ListSet"`. */
  override protected def className: String = "ListSet"

  /** Returns `0`; an instance of `ListSet` itself, as opposed to one of its nodes, is empty. */
  override def size: Int = 0
  /** Returns `0`; the size of an empty list set is known without traversal. */
  override def knownSize: Int = 0
  /** Returns `true`; an instance of `ListSet` itself, as opposed to one of its nodes, is empty. */
  override def isEmpty: Boolean = true

  /** Returns `false`; this set is empty, so it contains nothing.
   *
   *  @param elem the element to look for; never used
   */
  def contains(elem: A): Boolean = false

  /** Returns a set holding `elem` alone, since this set is empty.
   *
   *  @param elem the element to add
   *  @return a one-element list set whose remainder is this empty set
   */
  def incl(elem: A): ListSet[A] = new Node(elem)
  /** Returns this set itself; an empty set contains nothing to remove.
   *
   *  @param elem the element to remove; never used
   */
  def excl(elem: A): ListSet[A] = this

  /** Returns an iterator over the elements of this set, in the order in which they
   *  were first inserted.
   *
   *  The elements are held in reverse insertion order, so the whole chain is walked
   *  and reversed into a `List` before the iterator is returned; this costs `O(n)`.
   */
  def iterator: scala.collection.Iterator[A] = {
    var curr: ListSet[A] = this
    var res: List[A] = Nil
    while (!curr.isEmpty) {
      res = curr.elem :: res
      curr = curr.next
    }
    res.iterator
  }

  /** The element held by this node of the chain; an empty list set holds none.
   *
   *  @throws NoSuchElementException always, since this set is empty
   */
  protected def elem: A = throw new NoSuchElementException("elem of empty set")
  /** The remainder of the chain below this node; an empty list set has none.
   *
   *  @throws NoSuchElementException always, since this set is empty
   */
  protected def next: ListSet[A] = throw new NoSuchElementException("next of empty set")

  /** The factory used to build list sets, the [[ListSet$ `ListSet`]] companion object. */
  override def iterableFactory: IterableFactory[ListSet] = ListSet

  /** Represents an entry in the `ListSet`.
   *
   *  @param elem the element contained in this node of the list set
   */
  protected class Node(override protected val elem: A) extends ListSet[A] {

    /** Returns the number of elements in this set, counted by walking the chain, which costs `O(n)`. */
    override def size = sizeInternal(this, 0)
    /** Returns `-1`; the size of a non-empty list set is not known without walking the chain. */
    override def knownSize: Int = -1
    @tailrec private def sizeInternal(n: ListSet[A], acc: Int): Int =
      if (n.isEmpty) acc
      else sizeInternal(n.next, acc + 1)

    /** Returns `false`; a node always holds at least its own element. */
    override def isEmpty: Boolean = false

    /** Returns `true` if this set contains `e`, comparing elements with `==`.
     *
     *  The chain is searched from the most recently inserted element towards the
     *  oldest, which costs `O(n)`.
     *
     *  @param e the element to look for
     */
    override def contains(e: A): Boolean = containsInternal(this, e)

    @tailrec private def containsInternal(n: ListSet[A], e: A): Boolean =
      !n.isEmpty && (n.elem == e || containsInternal(n.next, e))

    /** Returns a set that contains `e` and all elements of this set. Returns this set
     *  itself if it already contains `e`, so that re-inserting an element leaves its
     *  position in the insertion order untouched; otherwise `e` becomes the most
     *  recently inserted element and the rest of the chain is shared.
     *
     *  The membership test makes this `O(n)`.
     *
     *  @param e the element to add
     *  @return a list set containing all elements of this set plus `e`
     */
    override def incl(e: A): ListSet[A] = if (contains(e)) this else new Node(e)

    /** Returns a set that contains all elements of this set except `e`, in their
     *  original insertion order. Returns this set itself if it does not contain `e`;
     *  otherwise the nodes inserted after `e` are rebuilt and the ones inserted
     *  before it are shared, which costs `O(n)`.
     *
     *  @param e the element to remove
     *  @return a list set containing all elements of this set except `e`
     */
    override def excl(e: A): ListSet[A] = removeInternal(e, this, Nil)

    @tailrec private def removeInternal(k: A, cur: ListSet[A], acc: List[ListSet[A]]): ListSet[A] =
      if (cur.isEmpty) acc.last
      else if (k == cur.elem) acc.foldLeft(cur.next)((t, h) => new t.Node(h.elem))
      else removeInternal(k, cur.next, cur :: acc)

    /** The set of all elements inserted before this node's element. */
    override protected def next: ListSet[A] = ListSet.this

    /** Returns the most recently inserted element of this set, which is this node's own
     *  element and therefore costs `O(1)`.
     */
    override def last: A = elem

    /** Returns this set without its most recently inserted element, which is the chain
     *  below this node and therefore costs `O(1)`.
     */
    override def init: ListSet[A] = next
  }
}

/** $factoryInfo
 *
 *  Note that each element insertion takes O(n) time, which means that creating a list set with
 *  n elements will take O(n^2^) time. This makes the builder suitable only for a small number of
 *  elements.
 *
 *  @define Coll ListSet
 *  @define coll list set
 */
@SerialVersionUID(3L)
object ListSet extends IterableFactory[ListSet] {

  /** Returns a list set containing the elements of `it`, in the order `it` gives them
   *  out, with any element that occurs more than once kept at the position of its
   *  first occurrence.
   *
   *  If `it` is already a `ListSet` it is returned unchanged; otherwise the elements
   *  are inserted one by one, which costs `O(n^2^)`.
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  def from[E](it: scala.collection.IterableOnce[E]^): ListSet[E] =
    (it: @unchecked) match {
      case ls: ListSet[E] => ls
      case _ if it.knownSize == 0 => empty[E]
      case _ => (newBuilder[E] ++= it).result()
    }

  private object EmptyListSet extends ListSet[Any] {
    override def knownSize: Int = 0
  }
  private[collection] def emptyInstance: ListSet[Any] = EmptyListSet

  /** Returns the empty list set.
   *
   *  All calls return the same instance, cast to the requested element type.
   *
   *  @tparam A the element type of the set
   */
  def empty[A]: ListSet[A] = EmptyListSet.asInstanceOf[ListSet[A]]

  /** Returns a new builder that adds elements to a list set one at a time, keeping the
   *  first occurrence of each.
   *
   *  Since each insertion is `O(n)`, building a set of n elements costs `O(n^2^)`.
   *
   *  @tparam A the element type of the set being built
   */
  def newBuilder[A]: Builder[A, ListSet[A]] =
    new ImmutableBuilder[A, ListSet[A]](empty) {
      def addOne(elem: A): this.type = { elems = elems + elem; this }
    }
}
