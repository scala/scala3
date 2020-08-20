package dotty.tools.dotc
package util

import java.util.NoSuchElementException

class SparseIntArray:
  import SparseIntArray._

  private var siz: Int = 0
  private var root: Node = LeafNode()

  private def grow() =
    val newRoot = InnerNode(root.level + 1)
    newRoot.elems(0) = root
    root = newRoot

  private def capacity: Int = root.elemSize * NodeSize

  def size = siz

  def contains(index: Int): Boolean =
    0 <= index && index < capacity && root.contains(index)

  def apply(index: Int): Value =
    require(index >= 0)
    if index >= capacity then throw NoSuchElementException()
    root.apply(index)

  def update(index: Int, value: Value): Unit =
    require(index >= 0)
    while capacity <= index do
      require(root.level < MaxLevels, "array index too large, maximum is 2^30 - 1")
      grow()
    if !root.update(index, value) then siz += 1

  /** Remove element at `index` if it is present
   *  @return element was present
   */
  def remove(index: Int): Boolean =
    require(index >= 0)
    index < capacity && {
      val result = root.remove(index)
      if result then siz -= 1
      result
    }

  /** All defined indices in an iterator */
  def keysIterator: Iterator[Int] = root.keysIterator(0)

  /** Perform operation for each key/value pair */
  def foreachBinding(op: (Int, Value) => Unit): Unit =
    root.foreachBinding(op, 0)

  /** Transform each defined value with transformation `op`.
   *  The transformation takes the element index and value as parameters.
   */
  def transform(op: Transform): Unit =
    root.transform(op, 0)

  /** Access to some info about low-level representation */
  def repr: Repr = root

  override def toString =
    val b = StringBuilder() ++= "SparseIntArray("
    var first = true
    foreachBinding { (idx, elem) =>
      if first then first = false else b ++= ", "
      b ++= s"$idx -> $elem"
    }
    b ++= ")"
    b.toString

object SparseIntArray:
  type Value = Int

  trait Transform:
    def apply(key: Int, v: Value): Value

  private inline val NodeSizeLog = 5
  private inline val NodeSize = 1 << NodeSizeLog
  private inline val MaxLevels = 5  // max size is 2 ^ ((MaxLevels + 1) * NodeSizeLog) = 2 ^ 30

  /** The exposed representation. Should be used just for nodeCount and
   *  low-level toString.
   */
  abstract class Repr:
    def nodeCount: Int

  private abstract class Node(val level: Int) extends Repr:
    private[SparseIntArray] def elemShift = level * NodeSizeLog
    private[SparseIntArray] def elemSize = 1 << elemShift
    private[SparseIntArray] def elemMask = elemSize - 1
    def contains(index: Int): Boolean
    def apply(index: Int): Value
    def update(index: Int, value: Value): Boolean
    def remove(index: Int): Boolean
    def isEmpty: Boolean
    def keysIterator(offset: Int): Iterator[Int]
    def foreachBinding(op: (Int, Value) => Unit, offset: Int): Unit
    def transform(op: Transform, offset: Int): Unit
    def nodeCount: Int
  end Node

  private class LeafNode extends Node(0):
    private val elems = new Array[Value](NodeSize)
    private var present: Int = 0

    def contains(index: Int): Boolean =
      (present & (1 << index)) != 0

    def apply(index: Int) =
      if !contains(index) then throw NoSuchElementException()
      elems(index)

    def update(index: Int, value: Value): Boolean =
      elems(index) = value
      val result = contains(index)
      present = present | (1 << index)
      result

    def remove(index: Int): Boolean =
      val result = contains(index)
      present = present & ~(1 << index)
      result

    def isEmpty = present == 0

    private def skipUndefined(i: Int): Int =
      if i < NodeSize && !contains(i) then skipUndefined(i + 1) else i

    def keysIterator(offset: Int) = new Iterator[Int]:
      private var curIdx = skipUndefined(0)
      def hasNext = curIdx < NodeSize
      def next(): Int =
        val result = curIdx + offset
        curIdx = skipUndefined(curIdx + 1)
        result

    def foreachBinding(op: (Int, Value) => Unit, offset: Int): Unit =
      var i = 0
      while i < NodeSize do
        if contains(i) then op(offset + i, elems(i))
        i += 1

    def transform(op: Transform, offset: Int): Unit =
      var i = 0
      while i < NodeSize do
        if contains(i) then elems(i) = op(offset + i, elems(i))
        i += 1

    def nodeCount = 1

    override def toString =
      elems
        .zipWithIndex
        .filter((elem, idx) => contains(idx))
        .map((elem, idx) => s"$idx -> $elem").mkString(s"0#(", ", ", ")")
  end LeafNode

  private class InnerNode(level: Int) extends Node(level):
    private[SparseIntArray] val elems = new Array[Node](NodeSize)
    private var empty: Boolean = true

    def contains(index: Int): Boolean =
      val elem = elems(index >>> elemShift)
      elem != null && elem.contains(index & elemMask)

    def apply(index: Int): Value =
      val elem = elems(index >>> elemShift)
      if elem == null then throw NoSuchElementException()
      elem.apply(index & elemMask)

    def update(index: Int, value: Value): Boolean =
      empty = false
      var elem = elems(index >>> elemShift)
      if elem == null then
        elem = newNode(level - 1)
        elems(index >>> elemShift) = elem
      elem.update(index & elemMask, value)

    def remove(index: Int): Boolean =
      val elem = elems(index >>> elemShift)
      if elem == null then false
      else
        val result = elem.remove(index & elemMask)
        if elem.isEmpty then
          elems(index >>> elemShift) = null
          var i = 0
          while i < NodeSize && elems(i) == null do i += 1
          if i == NodeSize then empty = true
        result

    def isEmpty = empty

    private def skipUndefined(i: Int): Int =
      if i < NodeSize && elems(i) == null then skipUndefined(i + 1) else i

    // Note: This takes (depth of tree) recursive steps to produce the
    // next index. It could be more efficient if we kept all active iterators
    // in a path.
    def keysIterator(offset: Int) = new Iterator[Value]:
      private var curIdx = skipUndefined(0)
      private var elemIt = Iterator.empty[Int]
      def hasNext = elemIt.hasNext || curIdx < NodeSize
      def next(): Value =
        if elemIt.hasNext then elemIt.next()
        else
          elemIt = elems(curIdx).keysIterator(offset + curIdx * elemSize)
          curIdx = skipUndefined(curIdx + 1)
          elemIt.next()

    def foreachBinding(op: (Int, Value) => Unit, offset: Int): Unit =
      var i = 0
      while i < NodeSize do
        if elems(i) != null then
          elems(i).foreachBinding(op, offset + i * elemSize)
        i += 1

    def transform(op: Transform, offset: Int): Unit =
      var i = 0
      while i < NodeSize do
        if elems(i) != null then
          elems(i).transform(op, offset + i * elemSize)
        i += 1

    def nodeCount =
      1 + elems.filter(_ != null).map(_.nodeCount).sum

    override def toString =
      elems
        .zipWithIndex
        .filter((elem, idx) => elem != null)
        .map((elem, idx) => s"$idx -> $elem").mkString(s"$level#(", ", ", ")")
  end InnerNode

  private def newNode(level: Int): Node =
    if level == 0 then LeafNode() else InnerNode(level)

end SparseIntArray
