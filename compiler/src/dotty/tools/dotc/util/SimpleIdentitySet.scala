package dotty.tools.dotc.util

import scala.language.unsafeNulls

import collection.mutable

/** A simple linked set with `eq` as the comparison, optimized for small sets.
 *  It has linear complexity for `contains`, `+`, and `-`.
 */
abstract class SimpleIdentitySet[+Elem <: AnyRef] {
  def size: Int
  def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E]
  def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem]
  def contains[E >: Elem <: AnyRef](x: E): Boolean
  def foreach(f: Elem => Unit): Unit
  def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean
  def map[B <: AnyRef](f: Elem => B): SimpleIdentitySet[B] =
    var acc: SimpleIdentitySet[B] = SimpleIdentitySet.empty
    foreach(x => acc += f(x))
    acc
  def /: [A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A
  def toList: List[Elem]
  def iterator: Iterator[Elem]

  final def isEmpty: Boolean = size == 0

  def forall[E >: Elem <: AnyRef](p: E => Boolean): Boolean = !exists(!p(_))

  def filter(p: Elem => Boolean): SimpleIdentitySet[Elem] =
    val z: SimpleIdentitySet[Elem] = SimpleIdentitySet.empty
    (z /: this)((s, x) => if p(x) then s + x else s)

  def ++ [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
    if (this.size == 0) that
    else if (that.size == 0) this
    else ((this: SimpleIdentitySet[E]) /: that)(_ + _)

  def -- [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
    if (that.size == 0) this
    else
      ((SimpleIdentitySet.empty: SimpleIdentitySet[E]) /: this) { (s, x) =>
        if (that.contains(x)) s else s + x
      }

  def == [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): Boolean =
    this.size == that.size && forall(that.contains)

  override def toString: String = toList.mkString("{", ", ", "}")
}

object SimpleIdentitySet {

  def apply[Elem <: AnyRef](elems: Elem*): SimpleIdentitySet[Elem] =
    elems.foldLeft(empty: SimpleIdentitySet[Elem])(_ + _)

  extension [E <: AnyRef](xs: SimpleIdentitySet[E])
    def intersect(ys: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
      xs.filter(ys.contains)

  object empty extends SimpleIdentitySet[Nothing] {
    def size: Int = 0
    def + [E <: AnyRef](x: E): SimpleIdentitySet[E] =
      new Set1[E](x)
    def - [E <: AnyRef](x: E): SimpleIdentitySet[Nothing] =
      this
    def contains[E <: AnyRef](x: E): Boolean = false
    def foreach(f: Nothing => Unit): Unit = ()
    def exists[E <: AnyRef](p: E => Boolean): Boolean = false
    override def map[B <: AnyRef](f: Nothing => B): SimpleIdentitySet[B] = empty
    def /: [A, E <: AnyRef](z: A)(f: (A, E) => A): A = z
    def toList = Nil
    def iterator = Iterator.empty
  }

  private class Set1[+Elem <: AnyRef](x0: AnyRef) extends SimpleIdentitySet[Elem] {
    def size = 1
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
      if (contains(x)) this else new Set2[E](x0, x)
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] =
      if (x `eq` x0) empty else this
    def contains[E >: Elem <: AnyRef](x: E): Boolean = x `eq` x0
    def foreach(f: Elem => Unit): Unit = f(x0.asInstanceOf[Elem])
    def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean =
      p(x0.asInstanceOf[E])
    override def map[B <: AnyRef](f: Elem => B): SimpleIdentitySet[B] =
      Set1(f(x0.asInstanceOf[Elem]))
    def /: [A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A =
      f(z, x0.asInstanceOf[E])
    def toList = x0.asInstanceOf[Elem] :: Nil
    def iterator = Iterator.single(x0.asInstanceOf[Elem])
  }

  private class Set2[+Elem <: AnyRef](x0: AnyRef, x1: AnyRef) extends SimpleIdentitySet[Elem] {
    def size = 2
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
      if (contains(x)) this else new Set3(x0, x1, x)
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] =
      if (x `eq` x0) new Set1(x1)
      else if (x `eq` x1) new Set1(x0)
      else this
    def contains[E >: Elem <: AnyRef](x: E): Boolean = (x `eq` x0) || (x `eq` x1)
    def foreach(f: Elem => Unit): Unit = { f(x0.asInstanceOf[Elem]); f(x1.asInstanceOf[Elem]) }
    def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean =
      p(x0.asInstanceOf[E]) || p(x1.asInstanceOf[E])
    override def map[B <: AnyRef](f: Elem => B): SimpleIdentitySet[B] =
      val y0 = f(x0.asInstanceOf[Elem])
      val y1 = f(x1.asInstanceOf[Elem])
      if y0 eq y1 then Set1(y0) else Set2(y0, y1)
    def /: [A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A =
      f(f(z, x0.asInstanceOf[E]), x1.asInstanceOf[E])
    def toList = x0.asInstanceOf[Elem] :: x1.asInstanceOf[Elem] :: Nil
    def iterator = Iterator.tabulate(2) {
      case 0 => x0.asInstanceOf[Elem]
      case 1 => x1.asInstanceOf[Elem]
    }
  }

  private class Set3[+Elem <: AnyRef](x0: AnyRef, x1: AnyRef, x2: AnyRef) extends SimpleIdentitySet[Elem] {
    def size = 3
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
      if (contains(x)) this
      else {
        val xs = new Array[AnyRef](4)
        xs(0) = x0
        xs(1) = x1
        xs(2) = x2
        xs(3) = x
        new SetN[E](xs)
      }
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] =
      if (x `eq` x0) new Set2(x1, x2)
      else if (x `eq` x1) new Set2(x0, x2)
      else if (x `eq` x2) new Set2(x0, x1)
      else this
    def contains[E >: Elem <: AnyRef](x: E): Boolean = (x `eq` x0) || (x `eq` x1) || (x `eq` x2)
    def foreach(f: Elem => Unit): Unit = {
      f(x0.asInstanceOf[Elem]); f(x1.asInstanceOf[Elem]); f(x2.asInstanceOf[Elem])
    }
    def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean =
      p(x0.asInstanceOf[E]) || p(x1.asInstanceOf[E]) || p(x2.asInstanceOf[E])
    override def map[B <: AnyRef](f: Elem => B): SimpleIdentitySet[B] =
      val y0 = f(x0.asInstanceOf[Elem])
      val y1 = f(x1.asInstanceOf[Elem])
      val y2 = f(x2.asInstanceOf[Elem])
      if y1 eq y0 then
        if y2 eq y0 then Set1(y0) else Set2(y0, y2)
      else if (y2 eq y0) || (y2 eq y1) then Set2(y0, y1)
      else Set3(y0, y1, y2)
    def /: [A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A =
      f(f(f(z, x0.asInstanceOf[E]), x1.asInstanceOf[E]), x2.asInstanceOf[E])
    def toList = x0.asInstanceOf[Elem] :: x1.asInstanceOf[Elem] :: x2.asInstanceOf[Elem] :: Nil
    def iterator = Iterator.tabulate(3) {
      case 0 => x0.asInstanceOf[Elem]
      case 1 => x1.asInstanceOf[Elem]
      case 2 => x2.asInstanceOf[Elem]
    }
  }

  private class SetN[+Elem <: AnyRef](val xs: Array[AnyRef]) extends SimpleIdentitySet[Elem] {
    def size = xs.length
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
      if (contains(x)) this
      else {
        val xs1 = new Array[AnyRef](size + 1)
        System.arraycopy(xs, 0, xs1, 0, size)
        xs1(size) = x
        new SetN[E](xs1)
      }
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] = {
      var i = 0
      while (i < size && (xs(i) `ne` x)) i += 1
      if (i == size) this
      else if (size == 4)
        if (i == 0) new Set3(xs(1), xs(2), xs(3))
        else if (i == 1) new Set3(xs(0), xs(2), xs(3))
        else if (i == 2) new Set3(xs(0), xs(1), xs(3))
        else new Set3(xs(0), xs(1), xs(2))
      else {
        val xs1 = new Array[AnyRef](size - 1)
        System.arraycopy(xs, 0, xs1, 0, i)
        System.arraycopy(xs, i + 1, xs1, i, size - (i + 1))
        new SetN(xs1)
      }
    }
    def contains[E >: Elem <: AnyRef](x: E): Boolean = {
      var i = 0
      while (i < size && (xs(i) `ne` x)) i += 1
      i < size
    }
    def foreach(f: Elem => Unit): Unit = {
      var i = 0
      while (i < size) { f(xs(i).asInstanceOf[Elem]); i += 1 }
    }
    def exists[E >: Elem <: AnyRef](p: E => Boolean): Boolean =
      xs.asInstanceOf[Array[E]].exists(p)
    def /: [A, E >: Elem <: AnyRef](z: A)(f: (A, E) => A): A =
      xs.asInstanceOf[Array[E]].foldLeft(z)(f)
    def toList: List[Elem] = {
      val buf = new mutable.ListBuffer[Elem]
      foreach(buf += _)
      buf.toList
    }
    def iterator = xs.iterator.asInstanceOf[Iterator[Elem]]
    override def ++ [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
      that match {
        case that: SetN[?] =>
          var toAdd: mutable.ArrayBuffer[AnyRef] = null
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
            new SetN[E](xs1)
          }
        case _ => super.++(that)
      }
    override def -- [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]): SimpleIdentitySet[E] =
      that match {
        case that: SetN[?] =>
          // both sets are large, optimize assuming they are similar
          // by starting from empty set and adding elements
          var toAdd: mutable.ArrayBuffer[AnyRef] = null
          val thisSize = this.size
          val thatSize = that.size
          val thatElems = that.xs
          var i = 0
          var searchStart = 0
          while (i < thisSize) {
            val elem = this.xs(i)
            var j = searchStart    // search thatElems in round robin fashion, starting one after latest hit
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
          if (toAdd == null) empty
          else toAdd.size match {
            case 1 => new Set1[E](toAdd(0))
            case 2 => new Set2[E](toAdd(0), toAdd(1))
            case 3 => new Set3[E](toAdd(0), toAdd(1), toAdd(2))
            case _ => new SetN[E](toAdd.toArray)
          }
        case _ => // this set is large, that set is small: reduce from above using `-`
          ((this: SimpleIdentitySet[E]) /: that)(_ - _)
      }
  }
}
