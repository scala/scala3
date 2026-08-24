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

package scala.collection.convert
package impl

import scala.language.`2.13`
import scala.collection.Stepper.EfficientSplit
import scala.collection._
import scala.collection.immutable.Node

/** A stepper that is a slightly elaborated version of the ChampBaseIterator;
 *  the main difference is that it knows when it should stop instead of running
 *  to the end of all trees.
 *
 *  @tparam A the element type produced by this stepper
 *  @tparam T the CHAMP trie node type, with recursive bound `T <: Node[T]`
 *  @tparam Sub the public stepper supertype returned by `trySplit`
 *  @tparam Semi the concrete self-type produced by `semiclone`, a subtype of `Sub`
 *  @param maxSize the maximum number of remaining elements this stepper will produce
 */
private[collection] abstract class ChampStepperBase[
  A, T <: Node[T], Sub, Semi <: Sub & ChampStepperBase[A, T, ?, ?]
](protected var maxSize: Int)
extends EfficientSplit {
  import Node.MaxDepth

  // Much of this code is identical to ChampBaseIterator.  If you change that, look here too!

  /** The index in `currentValueNode`'s payload of the next element to read. */
  protected var currentValueCursor: Int = 0
  /** The number of payload elements of `currentValueNode` available to this stepper:
   *  the node's full `payloadArity`, unless reduced by a split.
   */
  protected var currentValueLength: Int = 0
  /** The node whose payload elements are currently being read; unset until `initRoot`
   *  or a split assigns it.
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
  /** Initializes this stepper, which must be freshly created, to traverse the trie rooted
   *  at `rootNode`: pushes the root's sub-nodes for depth-first traversal and prepares
   *  the root's payload, if any, for consumption.
   *
   *  @param rootNode the root node of the trie to traverse
   */
  def initRoot(rootNode: T): Unit = {
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
    false
  }

  /** Returns no Java `Spliterator` characteristics: hash-trie order is not meaningful
   *  and after a split the remaining count is only an upper bound.
   */
  def characteristics: Int = 0

  /** Returns `maxSize`, an upper bound on the number of elements remaining (exact until
   *  the first split), or 0 if none remain.
   */
  def estimateSize: Long = if (hasStep) maxSize else 0L

  /** Creates a new, empty stepper of the concrete type; `trySplit` populates it with the
   *  split-off portion of the traversal state.
   *
   *  @return the new, empty stepper
   */
  def semiclone(): Semi

  /** Returns `true` if elements remain, searching depth-first for the next payload-bearing
   *  node if the current one is exhausted; sets `maxSize` to 0 once the traversal is done.
   */
  final def hasStep: Boolean = maxSize > 0 && {
    val ans = (currentValueCursor < currentValueLength) || searchNextValueNode()
    if (!ans) maxSize = 0
    ans
  }

  /** Splits off a prefix of the remaining traversal: if only the current payload node
   *  remains, hands the first half of its remaining values to the returned stepper;
   *  otherwise copies the traversal stack and divides the sub-node range at the
   *  shallowest level that still has nodes to visit, this stepper resuming from the
   *  division point.  Both halves keep the current `maxSize`, so sizes become upper
   *  bounds after a split.
   *
   *  @return a stepper over a prefix of the remaining elements, or `null` if no elements
   *          remain or fewer than two values are left in the sole remaining payload node
   */
  final def trySplit(): Sub | Null =
    if (!hasStep) null
    else {
      var fork = 0
      while (fork <= currentStackLevel && nodeCursorsAndLengths(2*fork) >= nodeCursorsAndLengths(2*fork + 1)) fork += 1
      if (fork > currentStackLevel && currentValueCursor > currentValueLength -2) null
      else {
        val semi = semiclone()
        semi.maxSize = maxSize
        semi.currentValueCursor = currentValueCursor
        semi.currentValueNode = currentValueNode
        if (fork > currentStackLevel) {
          // Just need to finish the current node
          semi.currentStackLevel = -1
          val i = (currentValueCursor + currentValueLength) >>> 1
          semi.currentValueLength = i
          currentValueCursor = i
        }
        else {
          // Need (at least some of) the full stack, so make an identical copy
          semi.nodeCursorsAndLengths = java.util.Arrays.copyOf(nodeCursorsAndLengths, nodeCursorsAndLengths.length)
          semi.nodes = java.util.Arrays.copyOf(nodes.asInstanceOf[Array[Node[T]]], nodes.length).asInstanceOf[Array[T]]
          semi.currentStackLevel = currentStackLevel
          semi.currentValueLength = currentValueLength

          // Split the top level of the stack where there's still something to split
          // Could make this more efficient by duplicating code from searchNextValueNode
          // instead of setting up for it to run normally.  But splits tend to be rare,
          // so it's not critically important.
          //
          // Note that this split can be kind of uneven; if we knew how many child nodes there
          // were we could do better.
          val i = (nodeCursorsAndLengths(2*fork) + nodeCursorsAndLengths(2*fork + 1)) >>> 1
          semi.nodeCursorsAndLengths(2*fork + 1) = i
          var j = currentStackLevel
          while (j > fork) {
            nodeCursorsAndLengths(2*j) = nodeCursorsAndLengths(2*j + 1)
            j -= 1
          }
          nodeCursorsAndLengths(2*fork) = i
          searchNextValueNode()
        }
        semi
      }
    }
}


private[collection] final class AnyChampStepper[A, T <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => A)
extends ChampStepperBase[A, T, AnyStepper[A], AnyChampStepper[A, T]](_maxSize)
with AnyStepper[A] {
  /** Returns the next element, extracted from the current payload node, and decrements
   *  the remaining-size bound.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): A =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  /** Returns a new, empty `AnyChampStepper` with the same `extract` function, for
   *  `trySplit` to populate.
   */
  def semiclone(): AnyChampStepper[A, T] = new AnyChampStepper[A, T](0, extract)
}
private[collection] object AnyChampStepper {
  /** Creates a stepper over all elements of the CHAMP trie rooted at `root`.
   *
   *  @tparam A the type of elements to produce
   *  @tparam T the type of the trie's nodes
   *  @param maxSize the number of elements in the trie
   *  @param root the root node of the trie
   *  @param extract the function reading the element at a payload index of a node
   *  @return a stepper over all elements of the trie
   */
  def from[A, T <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => A): AnyChampStepper[A, T] = {
    val ans = new AnyChampStepper[A, T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class DoubleChampStepper[T <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Double)
extends ChampStepperBase[Double, T, DoubleStepper, DoubleChampStepper[T]](_maxSize)
with DoubleStepper {
  /** Returns the next element, extracted from the current payload node, and decrements
   *  the remaining-size bound.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Double =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  /** Returns a new, empty `DoubleChampStepper` with the same `extract` function, for
   *  `trySplit` to populate.
   */
  def semiclone(): DoubleChampStepper[T] = new DoubleChampStepper[T](0, extract)
}
private[collection] object DoubleChampStepper {
  /** Creates a stepper over all elements of the CHAMP trie rooted at `root`.
   *
   *  @tparam T the type of the trie's nodes
   *  @param maxSize the number of elements in the trie
   *  @param root the root node of the trie
   *  @param extract the function reading the element at a payload index of a node
   *  @return a stepper over all elements of the trie
   */
  def from[T <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Double): DoubleChampStepper[T] = {
    val ans = new DoubleChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class IntChampStepper[T <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Int)
extends ChampStepperBase[Int, T, IntStepper, IntChampStepper[T]](_maxSize)
with IntStepper {
  /** Returns the next element, extracted from the current payload node, and decrements
   *  the remaining-size bound.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Int =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  /** Returns a new, empty `IntChampStepper` with the same `extract` function, for
   *  `trySplit` to populate.
   */
  def semiclone(): IntChampStepper[T] = new IntChampStepper[T](0, extract)
}
private[collection] object IntChampStepper {
  /** Creates a stepper over all elements of the CHAMP trie rooted at `root`.
   *
   *  @tparam T the type of the trie's nodes
   *  @param maxSize the number of elements in the trie
   *  @param root the root node of the trie
   *  @param extract the function reading the element at a payload index of a node
   *  @return a stepper over all elements of the trie
   */
  def from[T <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Int): IntChampStepper[T] = {
    val ans = new IntChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class LongChampStepper[T <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Long)
extends ChampStepperBase[Long, T, LongStepper, LongChampStepper[T]](_maxSize)
with LongStepper {
  /** Returns the next element, extracted from the current payload node, and decrements
   *  the remaining-size bound.
   *
   *  @throws NoSuchElementException if no elements remain
   */
  def nextStep(): Long =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  /** Returns a new, empty `LongChampStepper` with the same `extract` function, for
   *  `trySplit` to populate.
   */
  def semiclone(): LongChampStepper[T] = new LongChampStepper[T](0, extract)
}
private[collection] object LongChampStepper {
  /** Creates a stepper over all elements of the CHAMP trie rooted at `root`.
   *
   *  @tparam T the type of the trie's nodes
   *  @param maxSize the number of elements in the trie
   *  @param root the root node of the trie
   *  @param extract the function reading the element at a payload index of a node
   *  @return a stepper over all elements of the trie
   */
  def from[T <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Long): LongChampStepper[T] = {
    val ans = new LongChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}
