package dotty.tools.dotc.util

import collection.mutable

/** A simple linked set with `eq` as the comparison, optimized for small sets.
 * It has linear complexity for `contains`, `+`, and `-`.
 */
final class SimpleIdentitySet[+Elem <: AnyRef](val xs: Array[AnyRef]) {
  def size: Int = xs.length

  def +[E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
    if (contains(x)) this
    else {
      val xs1 = new Array[AnyRef](size + 1)
      System.arraycopy(xs, 0, xs1, 0, size)
      xs1(size) = x
      new SimpleIdentitySet[E](xs1)
    }

  def -[E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] = {
    var i = 0
    while (i < size && (xs(i) `ne` x)) i += 1
    if (i == size) this
    else if (size == 4)
      if (i == 0) SimpleIdentitySet(xs(1).asInstanceOf[Elem], xs(2).asInstanceOf[Elem], xs(3).asInstanceOf[Elem])
      else if (i == 1) SimpleIdentitySet(xs(0).asInstanceOf[Elem], xs(2).asInstanceOf[Elem], xs(3).asInstanceOf[Elem])
      else if (i == 2) SimpleIdentitySet(xs(0).asInstanceOf[Elem], xs(1).asInstanceOf[Elem], xs(3).asInstanceOf[Elem])
      else SimpleIdentitySet(xs(0).asInstanceOf[Elem], xs(1).asInstanceOf[Elem], xs(2).asInstanceOf[Elem])
    else {
      val xs1 = new Array[AnyRef](size - 1)
      System.arraycopy(xs, 0, xs1, 0, i)
      System.arraycopy(xs, i + 1, xs1, i, size - (i + 1))
      new SimpleIdentitySet(xs1)
    }
  }

  def contains[E >: Elem <: AnyRef](x: E): Boolean = {
    var i = 0
    while (i < size && (xs(i) `ne` x)) i += 1
    i < size
  }

  def foreach(f: Elem => Unit): Unit = {
    var i = 0
    while (i < size) {
      f(xs(i).asInstanceOf[Elem]); i += 1
    }
  }

  def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean =
    xs.asInstanceOf[Array[E]].exists(p)

  def map[B <: AnyRef](f: Elem => B): SimpleIdentitySet[B] =
    var acc: SimpleIdentitySet[B] = SimpleIdentitySet.empty
    foreach(x => acc += f(x))
    acc

  def flatMap[B <: AnyRef](f: Elem => SimpleIdentitySet[B]): SimpleIdentitySet[B] =
    var acc: SimpleIdentitySet[B] = SimpleIdentitySet.empty
    foreach(x => acc ++= f(x))
    acc

  def /:[A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A =
    xs.asInstanceOf[Array[E]].foldLeft(z)(f)

  def toList: List[Elem] = {
    val buf = new mutable.ListBuffer[Elem]
    foreach(buf += _)
    buf.toList
  }

  def nth(n: Int): Elem =
    if 0 <= n && n < size then xs(n).asInstanceOf[Elem]
    else throw new IndexOutOfBoundsException(n.toString)

  def isEmpty: Boolean = size == 0

  def iterator: Iterator[Elem] = Iterator.tabulate(size)(nth)

  def forall[E >: Elem <: AnyRef](p: E => Boolean): Boolean = !exists(!p(_))

  def filter(p: Elem => Boolean): SimpleIdentitySet[Elem] =
    val z: SimpleIdentitySet[Elem] = SimpleIdentitySet.empty
    (z /: this)((s, x) => if p(x) then s + x else s)

  def ++[E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
    if that.isEmpty then return this
    var toAdd: mutable.ArrayBuffer[AnyRef] | Null = null
    var i = 0
    val limit = that.xs.length
    while (i < limit) {
      val elem = that.xs(i)
      if (!contains(elem)) {
        if (toAdd == null) toAdd = new mutable.ArrayBuffer
        toAdd += elem
      }
      i += 1
    }
    if (toAdd == null) this
    else {
      val numAdded = toAdd.size
      val xs1 = new Array[AnyRef](size + numAdded)
      System.arraycopy(xs, 0, xs1, 0, size)
      var i = 0
      while (i < numAdded) {
        xs1(i + size) = toAdd(i)
        i += 1
      }
      new SimpleIdentitySet[E](xs1)
    }

  def --[E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
    if that.isEmpty then return this
    // optimize assuming they are similar
    // by starting from empty set and adding elements
    var toAdd: mutable.ArrayBuffer[AnyRef] | Null = null
    val thisSize = this.size
    val thatSize = that.size
    val thatElems = that.xs
    var i = 0
    var searchStart = 0
    while (i < thisSize) {
      val elem = this.xs(i)
      var j = searchStart // search thatElems in round-robin fashion, starting one after latest hit
      var missing = false
      while (!missing && (elem ne thatElems(j))) {
        j += 1
        if (j == thatSize) j = 0
        missing = j == searchStart
      }
      if (missing) {
        if (toAdd == null) toAdd = new mutable.ArrayBuffer
        toAdd += elem
      }
      else searchStart = (j + 1) % thatSize
      i += 1
    }
    if (toAdd == null) SimpleIdentitySet.empty
    else new SimpleIdentitySet[E](toAdd.toArray)

  def **[E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
    if this.size == 0 then this
    else if that.size == 0 then that
    else this.filter(that.contains)

  def ==[E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): Boolean =
    (this eq that) || this.size == that.size && forall(that.contains)

  def !=[E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): Boolean =
    !(this == that)

  override def toString: String = toList.mkString("{", ", ", "}")
}

object SimpleIdentitySet {
  private val emptySet = new SimpleIdentitySet(Array.empty[AnyRef])

  def apply[Elem <: AnyRef](elems: Elem*): SimpleIdentitySet[Elem] =
    elems.foldLeft(empty: SimpleIdentitySet[Elem])(_ + _)

  extension [E <: AnyRef](xs: SimpleIdentitySet[E])
    def intersect(ys: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
      xs.filter(ys.contains)

  def empty: SimpleIdentitySet[Nothing] =
    emptySet
}
