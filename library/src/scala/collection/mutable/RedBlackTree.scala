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

  /** A mutable red-black tree, consisting of a root node and an entry count.
   *
   *  Leaves are represented by `null` references instead of sentinel nodes, so an empty tree has a `null` root. The
   *  entry count is kept only here, not in the nodes, which makes reading the size of the whole tree O(1) while
   *  counting the entries of a subtree or range remains O(n).
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param root the root node of the tree, or `null` if the tree is empty
   *  @param size the number of entries in the tree
   */
  final class Tree[A, B](var root: Node[A, B] | Null, var size: Int) {
    /** Returns an independent copy of this tree, created by copying every node; the keys and values are shared */
    def treeCopy(): Tree[A, B] = new Tree(copyTree(root), size)
  }

  /** A mutable node of a red-black tree, holding one key-value entry, the node's color, and references to its
   *  children and parent. A `null` child reference stands for a leaf.
   *
   *  @tparam A the type of the key stored in this node
   *  @tparam B the type of the value stored in this node
   *  @param key the key of this node's entry
   *  @param value the value of this node's entry
   *  @param red `true` if this node is red, `false` if it is black
   *  @param left the left child of this node, or `null` if there is none
   *  @param right the right child of this node, or `null` if there is none
   *  @param parent the parent of this node, or `null` if this node is a root
   */
  final class Node[A, B](
      /** The key of this node's entry; `value` holds the associated value, and `red` is `true` if this node is red, `false` if black */
      var key: A, var value: B, var red: Boolean,
      /** The left child of this node; `null` stands for a leaf */
      @annotation.stableNull
      var left: Node[A, B] | Null,
      /** The right child of this node; `null` stands for a leaf */
      @annotation.stableNull
      var right: Node[A, B] | Null,
      /** The parent of this node, or `null` if this node is a root */
      @annotation.stableNull
      var parent: Node[A, B] | Null
    ) {
    /** Returns a string representation of this node showing its key, value, color, and left and right subtrees; the parent is omitted */
    override def toString(): String = "Node(" + key + ", " + value + ", " + red + ", " + left + ", " + right + ")"
  }

  object Tree {
    /** Returns a new, empty tree, with a `null` root and size 0.
     *
     *  @tparam A the key type of the tree entries
     *  @tparam B the value type of the tree entries
     */
    def empty[A, B]: Tree[A, B] = new Tree(null, 0)
  }

  object Node {

    @`inline` def apply[A, B](key: A, value: B, red: Boolean,
                            left: Node[A, B], right: Node[A, B], parent: Node[A, B] | Null): Node[A, B] =
      new Node(key, value, red, left, right, parent)

    @`inline` def leaf[A, B](key: A, value: B, red: Boolean, parent: Node[A, B] | Null): Node[A, B] =
      new Node(key, value, red, null, null, parent)

    /** Destructures the node `t` into its key, value, left child, right child, and parent; the color is not exposed.
     *
     *  @tparam A the key type of the node
     *  @tparam B the value type of the node
     *  @param t the node to destructure
     */
    def unapply[A, B](t: Node[A, B]) = Some((t.key, t.value, t.left, t.right, t.parent))
  }

  // ---- getters ----

  /** Returns `true` if `node` is red; a `null` leaf is not red.
   *
   *  @param node the node to test, or `null` for a leaf
   */
  def isRed(node: Node[?, ?] | Null) = (node ne null) && node.red
  /** Returns `true` if `node` is black; a `null` leaf counts as black.
   *
   *  @param node the node to test, or `null` for a leaf
   */
  def isBlack(node: Node[?, ?] | Null) = (node eq null) || !node.red

  // ---- size ----

  /** Returns the number of nodes in the subtree rooted at `node`, counted by traversing the subtree in O(n) time.
   *
   *  @param node the root of the subtree to count, or `null` for an empty subtree
   *  @return the number of nodes in the subtree; 0 if `node` is `null`
   */
  def size(node: Node[?, ?] | Null): Int = if (node eq null) 0 else 1 + size(node.left) + size(node.right)
  /** Returns the number of entries in `tree`, read from its size field in O(1) time.
   *
   *  @param tree the tree whose number of entries to return
   */
  def size(tree: Tree[?, ?]): Int = tree.size
  /** Returns `true` if `tree` contains no entries, that is, if its root is `null`.
   *
   *  @param tree the tree to test for emptiness
   */
  def isEmpty(tree: Tree[?, ?]) = tree.root eq null
  /** Removes all entries from `tree`, by setting its root to `null` and its size to 0.
   *
   *  @param tree the tree to clear
   */
  def clear(tree: Tree[?, ?]): Unit = { tree.root = null; tree.size = 0 }

  // ---- search ----

  /** Returns the value associated with `key` in `tree`.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the key to look up
   *  @return a `Some` containing the value associated with `key`, or `None` if the key is not present
   */
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

  /** Returns `true` if `tree` contains an entry whose key is equal to `key` under the implicit ordering.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the key to look for
   */
  def contains[A: Ordering](tree: Tree[A, ?], key: A): Boolean = getNode(tree.root, key) ne null

  /** Returns the entry with the smallest key in `tree`.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to search
   *  @return a `Some` containing the key-value pair with the smallest key, or `None` if the tree is empty
   */
  def min[A, B](tree: Tree[A, B]): Option[(A, B)] = minNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  /** Returns the smallest key in `tree`.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to search
   *  @return a `Some` containing the smallest key, or `None` if the tree is empty
   */
  def minKey[A](tree: Tree[A, ?]): Option[A] = minNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def minNode[A, B](node: Node[A, B] | Null): Node[A, B] | Null =
    if (node eq null) null else minNodeNonNull(node)

  /** Returns the leftmost node of the subtree rooted at `node`, that is, the node holding the smallest key of the
   *  subtree. Returns `node` itself if it has no left child.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param node the non-null root of the subtree to search
   *  @return the leftmost node of the subtree
   */
  @tailrec def minNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.left eq null) node else minNodeNonNull(node.left)

  /** Returns the entry with the largest key in `tree`.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to search
   *  @return a `Some` containing the key-value pair with the largest key, or `None` if the tree is empty
   */
  def max[A, B](tree: Tree[A, B]): Option[(A, B)] = maxNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  /** Returns the largest key in `tree`.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to search
   *  @return a `Some` containing the largest key, or `None` if the tree is empty
   */
  def maxKey[A](tree: Tree[A, ?]): Option[A] = maxNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def maxNode[A, B](node: Node[A, B] | Null): Node[A, B] | Null =
    if (node eq null) null else maxNodeNonNull(node)

  /** Returns the rightmost node of the subtree rooted at `node`, that is, the node holding the largest key of the
   *  subtree. Returns `node` itself if it has no right child.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param node the non-null root of the subtree to search
   *  @return the rightmost node of the subtree
   */
  @tailrec def maxNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.right eq null) node else maxNodeNonNull(node.right)

  /** Returns the first (lowest) map entry with a key equal or greater than `key`. Returns `None` if there is no such
   *  node.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the lower bound (inclusive) for the key lookup
   *  @param ord the ordering used to compare keys
   *  @return a `Some` containing the key-value pair whose key is the smallest value greater than or equal to `key`, or `None` if no such entry exists
   */
  def minAfter[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    minNodeAfter(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  /** Returns the first (lowest) key equal or greater than `key`. Returns `None` if there is no such key.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the lower bound (inclusive) for the key lookup
   *  @param ord the ordering used to compare keys
   *  @return a `Some` containing the smallest key greater than or equal to `key`, or `None` if no such key exists
   */
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

  /** Returns the last (highest) map entry with a key smaller than `key`. Returns `None` if there is no such entry.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the upper bound (exclusive) for the key lookup
   *  @param ord the ordering used to compare keys
   *  @return a `Some` containing the key-value pair whose key is the largest value strictly less than `key`, or `None` if no such entry exists
   */
  def maxBefore[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    maxNodeBefore(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  /** Returns the last (highest) key smaller than `key`. Returns `None` if there is no such key.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to search
   *  @param key the upper bound (exclusive) for the key lookup
   *  @param ord the ordering used to compare keys
   *  @return a `Some` containing the largest key strictly less than `key`, or `None` if no such key exists
   */
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

  /** Inserts the entry `key -> value` into `tree`. If the tree already contains an entry with a key equal to `key`
   *  under `ord`, that entry's value is replaced with `value` and the tree structure and size are unchanged. Otherwise
   *  a new red leaf node is added, the red-black invariants are restored by recolorings and rotations, and the tree's
   *  size grows by one.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to insert into
   *  @param key the key of the entry to insert
   *  @param value the value to associate with `key`
   *  @param ord the ordering used to compare keys
   */
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

  /** Removes the entry with key `key` from `tree`, if one exists; otherwise does nothing. When an entry is removed,
   *  its node is unlinked (a node with two children is first replaced by its in-order successor), the red-black
   *  invariants are restored by recolorings and rotations if a black node was unlinked, and the tree's size shrinks by
   *  one.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to remove from
   *  @param key the key of the entry to remove
   *  @param ord the ordering used to compare keys
   */
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
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param node the node whose in-order successor is to be found
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
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param node the node whose in-order predecessor is to be found
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
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree being modified
   *  @param to the node to be replaced
   *  @param from the node to put in `to`'s position, or `null` to leave the position empty
   */
  private def transplant[A, B](tree: Tree[A, B], to: Node[A, B], from: Node[A, B] | Null): Unit = {
    if (to.parent eq null) tree.root = from
    else if (to eq to.parent.nn.left) to.parent.left = from
    else to.parent.right = from

    if (from ne null) from.parent = to.parent
  }

  // ---- tree traversal ----

  /** Applies `f` to each key-value pair of `tree`, in ascending order of keys.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @tparam U the result type of `f`, which is discarded
   *  @param tree the red-black tree to traverse
   *  @param f the function applied to each key-value pair
   */
  def foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U): Unit = foreachNode(tree.root, f)

  private def foreachNode[A, B, U](node: Node[A, B] | Null, f: ((A, B)) => U): Unit =
    if (node ne null) foreachNodeNonNull(node, f)

  private def foreachNodeNonNull[A, B, U](node: Node[A, B], f: ((A, B)) => U): Unit = {
    if (node.left ne null) foreachNodeNonNull(node.left, f)
    f((node.key, node.value))
    if (node.right ne null) foreachNodeNonNull(node.right, f)
  }

  /** Applies `f` to each key of `tree`, in ascending order.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam U the result type of `f`, which is discarded
   *  @param tree the red-black tree to traverse
   *  @param f the function applied to each key
   */
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

  /** Applies `f` to the key and value of each entry of `tree`, in ascending order of keys. Unlike `foreach`, `f`
   *  receives the key and value as separate arguments, so no tuple is allocated per entry.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @tparam U the result type of `f`, which is discarded
   *  @param tree the red-black tree to traverse
   *  @param f the function applied to the key and value of each entry
   */
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

  /** Replaces the value of every entry of `tree`, in place, with the result of applying `f` to the entry's key and
   *  current value, visiting the entries in ascending order of keys. The keys and the tree structure are unchanged.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to transform
   *  @param f the function computing the new value from each key and current value
   */
  def transform[A, B](tree: Tree[A, B], f: (A, B) => B): Unit = transformNode(tree.root, f)

  private def transformNode[A, B, U](node: Node[A, B] | Null, f: (A, B) => B): Unit =
    if (node ne null) transformNodeNonNull(node, f)

  private def transformNodeNonNull[A, B, U](node: Node[A, B], f: (A, B) => B): Unit = {
    if (node.left ne null) transformNodeNonNull(node.left, f)
    node.value = f(node.key, node.value)
    if (node.right ne null) transformNodeNonNull(node.right, f)
  }

  /** Returns an iterator over the key-value pairs of `tree` whose keys are within the given bounds, in ascending
   *  order of keys.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to iterate over
   *  @param start the lower bound (inclusive) on the keys of the entries to return, or `None` for no lower bound
   *  @param end the upper bound (exclusive) on the keys of the entries to return, or `None` for no upper bound
   *  @return an iterator over the entries whose keys are greater than or equal to `start` (if defined) and smaller than `end` (if defined)
   */
  def iterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[(A, B)] =
    new EntriesIterator(tree, start, end)

  /** Returns an iterator over the keys of `tree` that are within the given bounds, in ascending order.
   *
   *  @tparam A the key type of the tree entries
   *  @param tree the red-black tree to iterate over
   *  @param start the lower bound (inclusive) on the keys to return, or `None` for no lower bound
   *  @param end the upper bound (exclusive) on the keys to return, or `None` for no upper bound
   *  @return an iterator over the keys that are greater than or equal to `start` (if defined) and smaller than `end` (if defined)
   */
  def keysIterator[A: Ordering](tree: Tree[A, ?], start: Option[A] = None, end: Option[A] = None): Iterator[A] =
    new KeysIterator(tree, start, end)

  /** Returns an iterator over the values of the entries of `tree` whose keys are within the given bounds, in
   *  ascending order of the associated keys.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to iterate over
   *  @param start the lower bound (inclusive) on the keys of the entries whose values to return, or `None` for no lower bound
   *  @param end the upper bound (exclusive) on the keys of the entries whose values to return, or `None` for no upper bound
   *  @return an iterator over the values of the entries whose keys are greater than or equal to `start` (if defined) and smaller than `end` (if defined)
   */
  def valuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[B] =
    new ValuesIterator(tree, start, end)

  private abstract class TreeIterator[A, B, R](tree: Tree[A, B], start: Option[A], end: Option[A])
                                                    (implicit ord: Ordering[A]) extends AbstractIterator[R] {

    /** Returns the value this iterator yields for `node`.
     *
     *  @param node the tree node to extract the result value from
     */
    protected def nextResult(node: Node[A, B]): R

    /** Returns `true` if there are more nodes within the iteration bounds */
    def hasNext: Boolean = nextNode ne null

    /** Returns the result value for the next node within the iteration bounds and advances past it.
     *
     *  @throws NoSuchElementException if there are no more nodes within the bounds
     */
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

    /** Returns the key-value pair stored in `node`.
     *
     *  @param node the tree node to extract the entry from
     */
    def nextResult(node: Node[A, B]) = (node.key, node.value)
  }

  private final class KeysIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, A](tree, start, end) {

    /** Returns the key stored in `node`.
     *
     *  @param node the tree node to extract the key from
     */
    def nextResult(node: Node[A, B]) = node.key
  }

  private final class ValuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, B](tree, start, end) {

    /** Returns the value stored in `node`.
     *
     *  @param node the tree node to extract the value from
     */
    def nextResult(node: Node[A, B]) = node.value
  }

  // ---- debugging ----

  /** Checks if the tree is in a valid state. That happens if:
   *  - It is a valid binary search tree;
   *  - All red-black properties are satisfied;
   *  - All non-null nodes have their `parent` reference correct;
   *  - The size variable in `tree` corresponds to the actual size of the tree.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to validate
   *  @return `true` if `tree` satisfies all of the above invariants, `false` otherwise
   */
  def isValid[A: Ordering, B](tree: Tree[A, B]): Boolean =
    isValidBST(tree.root) && hasProperParentRefs(tree) && isValidRedBlackTree(tree) && size(tree.root) == tree.size

  /** Returns true if all non-null nodes have their `parent` reference correct.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to check
   *  @return `true` if every non-null node's children point back to it via their `parent` field and the root has a `null` parent, `false` otherwise
   */
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

  /** Returns true if this node follows the properties of a binary search tree.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param node the root node of the subtree to validate
   *  @param ord the ordering used to compare keys
   *  @return `true` if the subtree rooted at `node` is a valid binary search tree under `ord`, `false` otherwise
   */
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
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param tree the red-black tree to validate
   *  @return `true` if `tree` satisfies the red-black tree invariants, `false` otherwise
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

  /** Builds a Tree suitable for a TreeSet from an ordered sequence of keys.
   *
   *  @tparam A the key type of the set entries
   *  @param xs an iterator over keys in ascending order
   *  @param size the number of keys in the iterator
   *  @return a balanced red-black tree containing the given keys, each paired with a `null` value (suitable for use as the backing store of a `TreeSet`)
   */
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

  /** Builds a Tree suitable for a TreeMap from an ordered sequence of key/value pairs.
   *
   *  @tparam A the key type of the map entries
   *  @tparam B the value type of the map entries
   *  @param xs an iterator over key-value pairs in ascending key order
   *  @param size the number of key-value pairs in the iterator
   *  @return a balanced red-black tree containing the given key-value pairs
   */
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

  /** Returns a copy of the subtree rooted at `n`: every node is copied along with its key, value, and color, while
   *  the keys and values themselves are shared with the original. The `parent` of the returned root is `null`.
   *
   *  @tparam A the key type of the tree entries
   *  @tparam B the value type of the tree entries
   *  @param n the root of the subtree to copy, or `null` for an empty subtree
   *  @return the root of the copied subtree, or `null` if `n` is `null`
   */
  def copyTree[A, B](n: Node[A, B] | Null): Node[A, B] | Null =
    if(n eq null) null else {
      val c = new Node(n.key, n.value, n.red, copyTree(n.left), copyTree(n.right), null)
      if(c.left != null) c.left.parent = c
      if(c.right != null) c.right.parent = c
      c
    }
}
