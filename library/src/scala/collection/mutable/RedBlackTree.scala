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
package collection.mutable

import scala.language.`2.13`
import language.experimental.captureChecking
import scala.annotation.tailrec
import collection.{AbstractIterator, Iterator}
import java.lang.String

/** An object containing the red-black tree implementation used by mutable `TreeMaps`.
 *
 *  The trees implemented in this object are *not* thread safe.
 */
private[collection] object RedBlackTree {

  // ---- class structure ----

  // For performance reasons, this implementation uses `null` references to represent leaves instead of a sentinel node.
  // Currently, the internal nodes do not store their subtree size - only the tree object keeps track of their size.
  // Therefore, while obtaining the size of the whole tree is O(1), knowing the number of entries inside a range is O(n)
  // on the size of the range.

  final class Tree[A, B](var root: Node[A, B] | Null, var size: Int) {
    def treeCopy(): Tree[A, B] = new Tree(copyTree(root), size)
  }

  final class Node[A, B](
      var key: A, var value: B, var red: Boolean,
      @annotation.stableNull
      var left: Node[A, B] | Null,
      @annotation.stableNull
      var right: Node[A, B] | Null,
      @annotation.stableNull
      var parent: Node[A, B] | Null
    ) {
    override def toString(): String = "Node(" + key + ", " + value + ", " + red + ", " + left + ", " + right + ")"
  }

  object Tree {
    def empty[A, B]: Tree[A, B] = new Tree(null, 0)
  }

  object Node {

    @`inline` def apply[A, B](key: A, value: B, red: Boolean,
                            left: Node[A, B], right: Node[A, B], parent: Node[A, B] | Null): Node[A, B] =
      new Node(key, value, red, left, right, parent)

    @`inline` def leaf[A, B](key: A, value: B, red: Boolean, parent: Node[A, B] | Null): Node[A, B] =
      new Node(key, value, red, null, null, parent)

    def unapply[A, B](t: Node[A, B]) = Some((t.key, t.value, t.left, t.right, t.parent))
  }

  // ---- getters ----

  def isRed(node: Node[?, ?] | Null) = (node ne null) && node.red
  def isBlack(node: Node[?, ?] | Null) = (node eq null) || !node.red

  // ---- size ----

  def size(node: Node[?, ?] | Null): Int = if (node eq null) 0 else 1 + size(node.left) + size(node.right)
  def size(tree: Tree[?, ?]): Int = tree.size
  def isEmpty(tree: Tree[?, ?]) = tree.root eq null
  def clear(tree: Tree[?, ?]): Unit = { tree.root = null; tree.size = 0 }

  // ---- search ----

  def get[A: Ordering, B](tree: Tree[A, B], key: A): Option[B] = getNode(tree.root, key) match {
    case null => None
    case node => Some(node.value)
  }

  @tailrec private def getNode[A, B](node: Node[A, B] | Null, key: A)(implicit ord: Ordering[A]): Node[A, B] | Null =
    if (node eq null) null
    else {
      val cmp = ord.compare(key, node.key)
      if (cmp < 0) getNode(node.left, key)
      else if (cmp > 0) getNode(node.right, key)
      else node
    }

  def contains[A: Ordering](tree: Tree[A, ?], key: A): Boolean = getNode(tree.root, key) ne null

  def min[A, B](tree: Tree[A, B]): Option[(A, B)] = minNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  def minKey[A](tree: Tree[A, ?]): Option[A] = minNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def minNode[A, B](node: Node[A, B] | Null): Node[A, B] | Null =
    if (node eq null) null else minNodeNonNull(node)

  @tailrec def minNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.left eq null) node else minNodeNonNull(node.left)

  def max[A, B](tree: Tree[A, B]): Option[(A, B)] = maxNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  def maxKey[A](tree: Tree[A, ?]): Option[A] = maxNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def maxNode[A, B](node: Node[A, B] | Null): Node[A, B] | Null =
    if (node eq null) null else maxNodeNonNull(node)

  @tailrec def maxNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.right eq null) node else maxNodeNonNull(node.right)

  /** Returns the first (lowest) map entry with a key equal or greater than `key`. Returns `None` if there is no such
   *  node.
   */
  def minAfter[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    minNodeAfter(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  def minKeyAfter[A](tree: Tree[A, ?], key: A)(implicit ord: Ordering[A]): Option[A] =
    minNodeAfter(tree.root, key) match {
      case null => None
      case node => Some(node.key)
    }

  private def minNodeAfter[A, B](node: Node[A, B] | Null, key: A)(implicit ord: Ordering[A]): Node[A, B] | Null = {
    if (node eq null) null
    else {
      // We know x is not null initially, so y will only be null before the first iteration of the loop.
      var y: Node[A, B] = null.asInstanceOf[Node[A, B]]
      var x: Node[A, B] | Null = node
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = ord.compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp <= 0) y else successor(y)
    }
  }

  /** Returns the last (highest) map entry with a key smaller than `key`. Returns `None` if there is no such node. */
  def maxBefore[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    maxNodeBefore(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  def maxKeyBefore[A](tree: Tree[A, ?], key: A)(implicit ord: Ordering[A]): Option[A] =
    maxNodeBefore(tree.root, key) match {
      case null => None
      case node => Some(node.key)
    }

  private def maxNodeBefore[A, B](node: Node[A, B] | Null, key: A)(implicit ord: Ordering[A]): Node[A, B] | Null = {
    if (node eq null) null
    else {
      // We know x is not null initially, so y will only be null before the first iteration of the loop.
      var y: Node[A, B] = null.asInstanceOf[Node[A, B]]
      var x: Node[A, B] | Null = node
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = ord.compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp > 0) y else predecessor(y)
    }
  }

  // ---- insertion ----

  def insert[A, B](tree: Tree[A, B], key: A, value: B)(implicit ord: Ordering[A]): Unit = {
    var y: Node[A, B] | Null = null
    var x = tree.root
    var cmp = 1
    while ((x ne null) && cmp != 0) {
      y = x
      cmp = ord.compare(key, x.key)
      x = if (cmp < 0) x.left else x.right
    }

    if (cmp == 0) y.nn.value = value
    else {
      val z = Node.leaf(key, value, red = true, y)

      if (y eq null) tree.root = z
      else if (cmp < 0) y.left = z
      else y.right = z

      fixAfterInsert(tree, z)
      tree.size += 1
    }
  }

  private def fixAfterInsert[A, B](tree: Tree[A, B], node: Node[A, B]): Unit = {
    var z = node
    while (isRed(z.parent)) {
      if (z.parent eq z.parent.nn.parent.nn.left) {
        val y = z.parent.nn.parent.nn.right
        if (isRed(y)) {
          z.parent.nn.red = false
          y.nn.red = false
          z.parent.nn.parent.nn.red = true
          z = z.parent.nn.parent.nn
        } else {
          if (z eq z.parent.nn.right) {
            z = z.parent.nn
            rotateLeft(tree, z)
          }
          z.parent.nn.red = false
          z.parent.nn.parent.nn.red = true
          rotateRight(tree, z.parent.nn.parent.nn)
        }
      } else { // symmetric cases
        val y = z.parent.nn.parent.nn.left
        if (isRed(y)) {
          z.parent.nn.red = false
          y.nn.red = false
          z.parent.nn.parent.nn.red = true
          z = z.parent.nn.parent.nn
        } else {
          if (z eq z.parent.nn.left) {
            z = z.parent.nn
            rotateRight(tree, z)
          }
          z.parent.nn.red = false
          z.parent.nn.parent.nn.red = true
          rotateLeft(tree, z.parent.nn.parent.nn)
        }
      }
    }
    tree.root.nn.red = false
  }

  // ---- deletion ----

  def delete[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Unit = {
    val z = getNode(tree.root, key)
    if (z ne null) {
      var y = z
      var yIsRed = y.red
      var x: Node[A, B] | Null = null
      var xParent: Node[A, B] | Null = null

      if (z.left eq null) {
        x = z.right
        transplant(tree, z, z.right)
        xParent = z.parent
      }
      else if (z.right eq null) {
        x = z.left
        transplant(tree, z, z.left)
        xParent = z.parent
      }
      else {
        y = minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          transplant(tree, y, y.right)
          y.right = z.right
          y.right.nn.parent = y
        }
        transplant(tree, z, y)
        y.left = z.left
        y.left.nn.parent = y
        y.red = z.red
      }

      if (!yIsRed) fixAfterDelete(tree, x, xParent)
      tree.size -= 1
    }
  }

  private def fixAfterDelete[A, B](tree: Tree[A, B], node: Node[A, B] | Null, parent: Node[A, B] | Null): Unit = {
    var x = node
    var xParent = parent
    while ((x ne tree.root) && isBlack(x)) {
      if (x eq xParent.nn.left) {
        var w = xParent.nn.right
        // assert(w ne null)

        if (w.nn.red) {
          w.nn.red = false
          xParent.nn.red = true
          rotateLeft(tree, xParent.nn)
          w = xParent.nn.right
        }
        if (isBlack(w.nn.left) && isBlack(w.nn.right)) {
          w.nn.red = true
          x = xParent
        } else {
          if (isBlack(w.nn.right)) {
            w.nn.left.nn.red = false
            w.nn.red = true
            rotateRight(tree, w.nn)
            w = xParent.nn.right
          }
          w.nn.red = xParent.nn.red
          xParent.nn.red = false
          w.nn.right.nn.red = false
          rotateLeft(tree, xParent.nn)
          x = tree.root
        }
      } else { // symmetric cases
        var w = xParent.nn.left
        // assert(w ne null)

        if (w.nn.red) {
          w.nn.red = false
          xParent.nn.red = true
          rotateRight(tree, xParent.nn)
          w = xParent.nn.left
        }
        if (isBlack(w.nn.right) && isBlack(w.nn.left)) {
          w.nn.red = true
          x = xParent
        } else {
          if (isBlack(w.nn.left)) {
            w.nn.right.nn.red = false
            w.nn.red = true
            rotateLeft(tree, w.nn)
            w = xParent.nn.left
          }
          w.nn.red = xParent.nn.red
          xParent.nn.red = false
          w.nn.left.nn.red = false
          rotateRight(tree, xParent.nn)
          x = tree.root
        }
      }
      xParent = x.nn.parent
    }
    if (x ne null) x.red = false
  }

  // ---- helpers ----

  /** Returns the node that follows `node` in an in-order tree traversal. If `node` has the maximum key (and is,
   *  therefore, the last node), this method returns `null`.
   */
  private def successor[A, B](node: Node[A, B]): Node[A, B] | Null = {
    if (node.right ne null) minNodeNonNull(node.right)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.nn.right)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  /** Returns the node that precedes `node` in an in-order tree traversal. If `node` has the minimum key (and is,
   *  therefore, the first node), this method returns `null`.
   */
  private def predecessor[A, B](node: Node[A, B]): Node[A, B] | Null = {
    if (node.left ne null) maxNodeNonNull(node.left)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.left)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  private def rotateLeft[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = if (x ne null) {
    // assert(x.right ne null)
    val y = x.right.nn
    x.right = y.left

    if (y.left ne null) y.left.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.nn.left) x.parent.left = y
    else x.parent.right = y

    y.left = x
    x.parent = y
  }

  private def rotateRight[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = if (x ne null) {
    // assert(x.left ne null)
    val y = x.left.nn
    x.left = y.right

    if (y.right ne null) y.right.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.right) x.parent.right = y
    else x.parent.left = y

    y.right = x
    x.parent = y
  }

  /** Transplant the node `from` to the place of node `to`. This is done by setting `from` as a child of `to`'s previous
   *  parent and setting `from`'s parent to the `to`'s previous parent. The children of `from` are left unchanged.
   */
  private def transplant[A, B](tree: Tree[A, B], to: Node[A, B], from: Node[A, B] | Null): Unit = {
    if (to.parent eq null) tree.root = from
    else if (to eq to.parent.nn.left) to.parent.left = from
    else to.parent.right = from

    if (from ne null) from.parent = to.parent
  }

  // ---- tree traversal ----

  def foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U): Unit = foreachNode(tree.root, f)

  private def foreachNode[A, B, U](node: Node[A, B] | Null, f: ((A, B)) => U): Unit =
    if (node ne null) foreachNodeNonNull(node, f)

  private def foreachNodeNonNull[A, B, U](node: Node[A, B], f: ((A, B)) => U): Unit = {
    if (node.left ne null) foreachNodeNonNull(node.left, f)
    f((node.key, node.value))
    if (node.right ne null) foreachNodeNonNull(node.right, f)
  }

  def foreachKey[A, U](tree: Tree[A, ?], f: A => U): Unit = {
    def g(node: Node[A, ?]): Unit = {
      val l = node.left
      if(l ne null) g(l)
      f(node.key)
      val r = node.right
      if(r ne null) g(r)
    }
    val r = tree.root
    if(r ne null) g(r)
  }

  def foreachEntry[A, B, U](tree: Tree[A, B], f: (A, B) => U): Unit = {
    def g(node: Node[A, B]): Unit = {
      val l = node.left
      if(l ne null) g(l)
      f(node.key, node.value)
      val r = node.right
      if(r ne null) g(r)
    }
    val r = tree.root
    if(r ne null) g(r)
  }

  def transform[A, B](tree: Tree[A, B], f: (A, B) => B): Unit = transformNode(tree.root, f)

  private def transformNode[A, B, U](node: Node[A, B] | Null, f: (A, B) => B): Unit =
    if (node ne null) transformNodeNonNull(node, f)

  private def transformNodeNonNull[A, B, U](node: Node[A, B], f: (A, B) => B): Unit = {
    if (node.left ne null) transformNodeNonNull(node.left, f)
    node.value = f(node.key, node.value)
    if (node.right ne null) transformNodeNonNull(node.right, f)
  }

  def iterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[(A, B)] =
    new EntriesIterator(tree, start, end)

  def keysIterator[A: Ordering](tree: Tree[A, ?], start: Option[A] = None, end: Option[A] = None): Iterator[A] =
    new KeysIterator(tree, start, end)

  def valuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[B] =
    new ValuesIterator(tree, start, end)

  private abstract class TreeIterator[A, B, R](tree: Tree[A, B], start: Option[A], end: Option[A])
                                                    (implicit ord: Ordering[A]) extends AbstractIterator[R] {

    protected def nextResult(node: Node[A, B]): R

    def hasNext: Boolean = nextNode ne null

    @throws[NoSuchElementException]
    def next(): R = nextNode match {
      case null => throw new NoSuchElementException("next on empty iterator")
      case node =>
        nextNode = successor(node)
        setNullIfAfterEnd()
        nextResult(node)
    }

    private var nextNode: Node[A, B] | Null = start match {
      case None => minNode(tree.root)
      case Some(from) => minNodeAfter(tree.root, from)
    }

    private def setNullIfAfterEnd(): Unit =
      if (end.isDefined && (nextNode ne null) && ord.compare(nextNode.nn.key, end.get) >= 0)
        nextNode = null

    setNullIfAfterEnd()
  }
  private final class EntriesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, (A, B)](tree, start, end) {

    def nextResult(node: Node[A, B]) = (node.key, node.value)
  }

  private final class KeysIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, A](tree, start, end) {

    def nextResult(node: Node[A, B]) = node.key
  }

  private final class ValuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, B](tree, start, end) {

    def nextResult(node: Node[A, B]) = node.value
  }

  // ---- debugging ----

  /** Checks if the tree is in a valid state. That happens if:
   *  - It is a valid binary search tree;
   *  - All red-black properties are satisfied;
   *  - All non-null nodes have their `parent` reference correct;
   *  - The size variable in `tree` corresponds to the actual size of the tree.
   */
  def isValid[A: Ordering, B](tree: Tree[A, B]): Boolean =
    isValidBST(tree.root) && hasProperParentRefs(tree) && isValidRedBlackTree(tree) && size(tree.root) == tree.size

  /** Returns true if all non-null nodes have their `parent` reference correct. */
  private def hasProperParentRefs[A, B](tree: Tree[A, B]): Boolean = {

    def hasProperParentRefs(node: Node[A, B] | Null): Boolean = {
      if (node eq null) true
      else {
        if ((node.left ne null) && (node.left.parent ne node) ||
          (node.right ne null) && (node.right.parent ne node)) false
        else hasProperParentRefs(node.left) && hasProperParentRefs(node.right)
      }
    }

    if(tree.root eq null) true
    else (tree.root.nn.parent eq null) && hasProperParentRefs(tree.root)
  }

  /** Returns true if this node follows the properties of a binary search tree. */
  private def isValidBST[A, B](node: Node[A, B] | Null)(implicit ord: Ordering[A]): Boolean = {
    if (node eq null) true
    else {
      if ((node.left ne null) && (ord.compare(node.key, node.left.key) <= 0) ||
        (node.right ne null) && (ord.compare(node.key, node.right.key) >= 0)) false
      else isValidBST(node.left) && isValidBST(node.right)
    }
  }

  /** Returns true if the tree has all the red-black tree properties: if the root node is black, if all children of red
   *  nodes are black and if the path from any node to any of its null children has the same number of black nodes.
   */
  private def isValidRedBlackTree[A, B](tree: Tree[A, B]): Boolean = {

    def noRedAfterRed(node: Node[A, B] | Null): Boolean = {
      if (node eq null) true
      else if (node.red && (isRed(node.left) || isRed(node.right))) false
      else noRedAfterRed(node.left) && noRedAfterRed(node.right)
    }

    def blackHeight(node: Node[A, B] | Null): Int = {
      if (node eq null) 1
      else {
        val lh = blackHeight(node.left)
        val rh = blackHeight(node.right)

        if (lh == -1 || lh != rh) -1
        else if (isRed(node)) lh
        else lh + 1
      }
    }

    isBlack(tree.root) && noRedAfterRed(tree.root) && blackHeight(tree.root) >= 0
  }

  // building

  /** Builds a Tree suitable for a TreeSet from an ordered sequence of keys. */
  def fromOrderedKeys[A](xs: Iterator[A]^, size: Int): Tree[A, Null] = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): Node[A, Null] | Null = size match {
      case 0 => null
      case 1 => new Node(xs.next(), null, level == maxUsedDepth && level != 1, null, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val x = xs.next()
        val right = f(level+1, size-1-leftSize)
        val n = new Node(x, null, red = false, left, right, null)
        if(left ne null) left.parent = n
        right.nn.parent = n
        n
    }
    new Tree(f(1, size), size)
  }

  /** Builds a Tree suitable for a TreeMap from an ordered sequence of key/value pairs. */
  def fromOrderedEntries[A, B](xs: Iterator[(A, B)]^, size: Int): Tree[A, B] = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): Node[A, B] | Null = size match {
      case 0 => null
      case 1 =>
        val (k, v) = xs.next()
        new Node(k, v, level == maxUsedDepth && level != 1, null, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val (k, v) = xs.next()
        val right = f(level+1, size-1-leftSize)
        val n = new Node(k, v, red = false, left, right, null)
        if(left ne null) left.parent = n
        right.nn.parent = n
        n
    }
    new Tree(f(1, size), size)
  }

  def copyTree[A, B](n: Node[A, B] | Null): Node[A, B] | Null =
    if(n eq null) null else {
      val c = new Node(n.key, n.value, n.red, copyTree(n.left), copyTree(n.right), null)
      if(c.left != null) c.left.parent = c
      if(c.right != null) c.right.parent = c
      c
    }
}
