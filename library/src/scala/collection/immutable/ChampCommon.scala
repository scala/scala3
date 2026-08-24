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

package scala.collection.immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.AbstractIterator
import java.lang.Integer.bitCount
import java.lang.Math.ceil
import java.lang.System.arraycopy

private[collection] object Node {
  /** The number of bits in an `Int` hash code (32), and thus the number of hash bits
   *  available to address positions in a CHAMP trie.
   */
  final val HashCodeLength = 32

  /** The number of hash bits consumed at each level of the trie (5). Each node
   *  discriminates its children on a `BitPartitionSize`-bit slice of the improved hash,
   *  so the `shift` argument threaded through node operations grows by this amount
   *  per level of descent.
   */
  final val BitPartitionSize = 5

  /** A mask with the lowest `BitPartitionSize` bits set (`0x1f`), used to cut a
   *  single 5-bit slice out of a hash code.
   */
  final val BitPartitionMask = (1 << BitPartitionSize) - 1

  /** The maximum depth of the trie: the number of 5-bit slices in a 32-bit hash
   *  code, rounded up (7). Once a hash is exhausted at this depth, colliding
   *  elements are stored in hash-collision leaf nodes instead.
   */
  final val MaxDepth = ceil(HashCodeLength.toDouble / BitPartitionSize).toInt

  /** The maximum number of children of a single trie node: `2^BitPartitionSize` (32),
   *  one child position per possible value of a 5-bit hash slice.
   */
  final val BranchingFactor = 1 << BitPartitionSize

  /** Returns the 5-bit slice of `hash` that addresses a child position at the trie
   *  level identified by `shift`, as a value in `[0, BranchingFactor)`.
   *
   *  @param hash the (improved) hash code of an element
   *  @param shift the number of low hash bits already consumed by ancestor levels
   *              (0 at the root, growing by `BitPartitionSize` per level)
   *  @return the child position ("mask") of the hash at this level, in `[0, 31]`
   */
  final def maskFrom(hash: Int, shift: Int): Int = (hash >>> shift) & BitPartitionMask

  /** Returns the single-bit bitmap position corresponding to child position `mask`,
   *  i.e. `1 << mask`. Node bitmaps (`dataMap`, `nodeMap`) are tested and updated
   *  with such one-bit values.
   *
   *  @param mask a child position in `[0, BranchingFactor)`, as produced by [[maskFrom]]
   *  @return an `Int` with exactly the bit number `mask` set
   */
  final def bitposFrom(mask: Int): Int = 1 << mask

  /** Returns the index in a node's compressed content array of the child at bit
   *  position `bitpos`: the number of bits set in `bitmap` below `bitpos`. CHAMP
   *  nodes store only present children, packed contiguously, so a child's array
   *  index is the count of present children at lower bit positions.
   *
   *  @param bitmap a node bitmap (`dataMap` or `nodeMap`) in which the bit of `bitpos` is set
   *  @param bitpos a one-bit value as produced by [[bitposFrom]]
   *  @return the compressed array index, in `[0, bitCount(bitmap))`
   */
  final def indexFrom(bitmap: Int, bitpos: Int): Int = bitCount(bitmap & (bitpos - 1))

  /** Returns the index in a node's compressed content array of the child at bit
   *  position `bitpos`, like the two-argument [[indexFrom]], but with a fast path
   *  for a full bitmap: when all 32 bits of `bitmap` are set the array is dense
   *  and the index is `mask` itself, with no bit counting needed.
   *
   *  @param bitmap a node bitmap (`dataMap` or `nodeMap`) in which the bit of `bitpos` is set
   *  @param mask the child position in `[0, BranchingFactor)` from which `bitpos` was derived
   *  @param bitpos a one-bit value as produced by [[bitposFrom]]
   *  @return the compressed array index, in `[0, bitCount(bitmap))`
   */
  final def indexFrom(bitmap: Int, mask: Int, bitpos: Int): Int = if (bitmap == -1) mask else indexFrom(bitmap, bitpos)

}

private[collection] abstract class Node[T <: Node[T]] {

  /** Returns `true` if this node has at least one sub-node child. */
  def hasNodes: Boolean

  /** Returns the number of sub-node children of this node. */
  def nodeArity: Int

  /** Returns the sub-node child at the given index.
   *
   *  @param index the position among this node's sub-nodes, in `[0, nodeArity)`
   *  @return the sub-node at that position
   */
  def getNode(index: Int): T

  /** Returns `true` if this node stores at least one payload element directly. */
  def hasPayload: Boolean

  /** Returns the number of payload elements stored directly in this node. */
  def payloadArity: Int

  /** Returns the payload stored directly in this node at the given index.
   *
   *  @param index the position among this node's payload elements, in `[0, payloadArity)`
   *  @return the payload at that position (an element for sets, a key or key-value pair for maps)
   */
  def getPayload(index: Int): Any

  /** Returns the original (unimproved) hash code, i.e. `key.##`, of the payload
   *  stored directly in this node at the given index.
   *
   *  @param index the position among this node's payload elements, in `[0, payloadArity)`
   *  @return the cached original hash code of that payload
   */
  def getHash(index: Int): Int

  /** The sum of the improved hash codes of all elements (or keys, for maps) in the
   *  subtree rooted at this node, following the `java.util.Set` hash-code contract.
   *  Cached so parents can maintain their own sum incrementally and equality checks
   *  can fail fast without traversal.
   */
  def cachedJavaKeySetHashCode: Int

  private final def arrayIndexOutOfBounds(as: Array[?], ix:Int): ArrayIndexOutOfBoundsException =
    new ArrayIndexOutOfBoundsException(s"$ix is out of bounds (min 0, max ${as.length-1}")

  /** Returns a copy of `as` with the element at index `ix` removed, shifting
   *  any trailing elements one position left.
   *
   *  @param as the source array; not modified
   *  @param ix the index of the element to remove
   *  @return a new array of length `as.length - 1`
   *  @throws ArrayIndexOutOfBoundsException if `ix` is negative or `>= as.length`
   */
  protected final def removeElement(as: Array[Int], ix: Int): Array[Int] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length - 1) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Int](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  /** Returns a copy of `as` with the element at index `ix` removed, shifting
   *  any trailing elements one position left.
   *
   *  @param as the source array; not modified
   *  @param ix the index of the element to remove
   *  @return a new array of length `as.length - 1`
   *  @throws ArrayIndexOutOfBoundsException if `ix` is negative or `>= as.length`
   */
  protected final def removeAnyElement(as: Array[Any], ix: Int): Array[Any] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length - 1) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Any](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  /** Returns a copy of `as` with `elem` inserted at index `ix`, shifting the
   *  elements from `ix` onwards one position right.
   *
   *  @param as the source array; not modified
   *  @param ix the index at which to insert, in `[0, as.length]`
   *  @param elem the value to insert
   *  @return a new array of length `as.length + 1`
   *  @throws ArrayIndexOutOfBoundsException if `ix` is negative or `> as.length`
   */
  protected final def insertElement(as: Array[Int], ix: Int, elem: Int): Array[Int] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Int](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }
  /** Returns a copy of `as` with `elem` (boxed) inserted at index `ix`, shifting
   *  the elements from `ix` onwards one position right. Note that the inserted
   *  element is an `Int`, not an `Any`; this helper is currently never called.
   *
   *  @param as the source array; not modified
   *  @param ix the index at which to insert, in `[0, as.length]`
   *  @param elem the value to insert
   *  @return a new array of length `as.length + 1`
   *  @throws ArrayIndexOutOfBoundsException if `ix` is negative or `> as.length`
   */
  protected final def insertAnyElement(as: Array[Any], ix: Int, elem: Int): Array[Any] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Any](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }
}

/** Base class for fixed-stack iterators that traverse a hash-trie. The iterator performs a
 *  depth-first pre-order traversal, which yields first all payload elements of the current
 *  node before traversing sub-nodes (left to right).
 *
 *  @tparam T the trie node type we are iterating over
 *  @tparam A the element type produced by the iterator
 */
private[immutable] abstract class ChampBaseIterator[A, T <: Node[T]] extends AbstractIterator[A] {

  import Node.MaxDepth

  // Note--this code is duplicated to a large extent both in
  // ChampBaseReverseIterator and in convert.impl.ChampStepperBase.
  // If you change this code, check those also in case they also
  // need to be modified.
  
  /** The index in `currentValueNode` of the next payload element to return, in
   *  `[0, currentValueLength]`; equal to `currentValueLength` when the current
   *  node's payload is exhausted.
   */
  protected var currentValueCursor: Int = 0
  /** The number of payload elements in `currentValueNode` (its `payloadArity`),
   *  the exclusive upper bound for `currentValueCursor`.
   */
  protected var currentValueLength: Int = 0
  /** The node whose payload elements are currently being emitted. Uninitialized
   *  until the traversal first reaches a payload-bearing node.
   */
  protected var currentValueNode: T = compiletime.uninitialized

  private var currentStackLevel: Int = -1
  private var nodeCursorsAndLengths: Array[Int] = compiletime.uninitialized
  private var nodes: Array[T] = compiletime.uninitialized
  private def initNodes(): Unit = {
    if (nodeCursorsAndLengths eq null) {
      nodeCursorsAndLengths = new Array[Int](MaxDepth * 2)
      nodes = new Array[Node[T]](MaxDepth).asInstanceOf[Array[T]]
    }
  }

  /** Creates an iterator over the trie rooted at `rootNode`: pushes the root on
   *  the traversal stack if it has sub-nodes, and starts with the root's own
   *  payload elements if it has any.
   *
   *  @param rootNode the root of the trie to traverse
   */
  def this(rootNode: T) = {
    this()
    if (rootNode.hasNodes) pushNode(rootNode)
    if (rootNode.hasPayload) setupPayloadNode(rootNode)
  }

  private final def setupPayloadNode(node: T): Unit = {
    currentValueNode = node
    currentValueCursor = 0
    currentValueLength = node.payloadArity
  }

  private final def pushNode(node: T): Unit = {
    initNodes()
    currentStackLevel = currentStackLevel + 1

    val cursorIndex = currentStackLevel * 2
    val lengthIndex = currentStackLevel * 2 + 1

    nodes(currentStackLevel) = node
    nodeCursorsAndLengths(cursorIndex) = 0
    nodeCursorsAndLengths(lengthIndex) = node.nodeArity
  }

  private final def popNode(): Unit = {
    currentStackLevel = currentStackLevel - 1
  }

  /** Searches for next node that contains payload values,
   *  and pushes encountered sub-nodes on a stack for depth-first traversal.
   */
  private final def searchNextValueNode(): Boolean = {
    while (currentStackLevel >= 0) {
      val cursorIndex = currentStackLevel * 2
      val lengthIndex = currentStackLevel * 2 + 1

      val nodeCursor = nodeCursorsAndLengths(cursorIndex)
      val nodeLength = nodeCursorsAndLengths(lengthIndex)

      if (nodeCursor < nodeLength) {
        nodeCursorsAndLengths(cursorIndex) += 1

        val nextNode = nodes(currentStackLevel).getNode(nodeCursor)

        if (nextNode.hasNodes)   { pushNode(nextNode) }
        if (nextNode.hasPayload) { setupPayloadNode(nextNode) ; return true }
      } else {
        popNode()
      }
    }

    return false
  }

  /** Returns `true` if elements remain: either unconsumed payload in the current
   *  node, or a further payload-bearing node found by continuing the depth-first
   *  search. In the latter case, advances the traversal state to that node as a
   *  side effect.
   */
  final def hasNext = (currentValueCursor < currentValueLength) || searchNextValueNode()

}

/** Base class for fixed-stack iterators that traverse a hash-trie in reverse order. The base
 *  iterator performs a depth-first post-order traversal, traversing sub-nodes (right to left).
 *
 *  @tparam T the trie node type we are iterating over
 *  @tparam A the element type produced by the iterator
 */
private[immutable] abstract class ChampBaseReverseIterator[A, T <: Node[T]] extends AbstractIterator[A] {

  import Node.MaxDepth

  /** The index in `currentValueNode` of the next payload element to return,
   *  counting downwards; negative when the current node's payload is exhausted.
   */
  protected var currentValueCursor: Int = -1
  /** The node whose payload elements are currently being emitted. Uninitialized
   *  until the traversal first reaches a payload-bearing node.
   */
  protected var currentValueNode: T = compiletime.uninitialized

  private var currentStackLevel: Int = -1
  private val nodeIndex: Array[Int] = new Array[Int](MaxDepth + 1)
  private val nodeStack: Array[T] = new Array[Node[T]](MaxDepth + 1).asInstanceOf[Array[T]]

  /** Creates a reverse iterator over the trie rooted at `rootNode`: pushes the
   *  root on the traversal stack and immediately descends to the rightmost
   *  payload-bearing node, so that iteration starts with the trie's last element.
   *
   *  @param rootNode the root of the trie to traverse
   */
  def this(rootNode: T) = {
    this()
    pushNode(rootNode)
    searchNextValueNode()
  }

  private final def setupPayloadNode(node: T): Unit = {
    currentValueNode = node
    currentValueCursor = node.payloadArity - 1
  }

  private final def pushNode(node: T): Unit = {
    currentStackLevel = currentStackLevel + 1

    nodeStack(currentStackLevel) = node
    nodeIndex(currentStackLevel) = node.nodeArity - 1
  }

  private final def popNode(): Unit = {
    currentStackLevel = currentStackLevel - 1
  }

  /** Searches for rightmost node that contains payload values,
   *  and pushes encountered sub-nodes on a stack for depth-first traversal.
   */
  private final def searchNextValueNode(): Boolean = {
    while (currentStackLevel >= 0) {
      val nodeCursor = nodeIndex(currentStackLevel) ; nodeIndex(currentStackLevel) = nodeCursor - 1

      if (nodeCursor >= 0) {
        val nextNode = nodeStack(currentStackLevel).getNode(nodeCursor)
        pushNode(nextNode)
      } else {
        val currNode = nodeStack(currentStackLevel)
        popNode()

        if (currNode.hasPayload) { setupPayloadNode(currNode) ; return true }
      }
    }

    return false
  }

  /** Returns `true` if elements remain: either unconsumed payload in the current
   *  node, or an earlier payload-bearing node found by continuing the reverse
   *  depth-first search. In the latter case, advances the traversal state to that
   *  node as a side effect.
   */
  final def hasNext = (currentValueCursor >= 0) || searchNextValueNode()

}
