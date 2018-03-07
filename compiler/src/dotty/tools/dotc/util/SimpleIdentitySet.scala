package dotty.tools.dotc.util

import collection.mutable.ListBuffer

/** A simple linked set with `eq` as the comparison, optimized for small sets.
 *  It has linear complexity for `contains`, `+`, and `-`.
 */
abstract class SimpleIdentitySet[+Elem <: AnyRef] {
  def size: Int
  def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E]
  def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem]
  def contains[E >: Elem <: AnyRef](x: E): Boolean
  def foreach(f: Elem => Unit): Unit
  def toList: List[Elem] = {
    val buf = new ListBuffer[Elem]
    foreach(buf += _)
    buf.toList
  }
  def ++ [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]) =
    ((this: SimpleIdentitySet[E]) /: that.toList)(_ + _)
  def -- [E >: Elem <: AnyRef](that: SimpleIdentitySet[E]) =
    (this /: that.toList)(_ - _)
  override def toString = toList.mkString("(", ", ", ")")
}

object SimpleIdentitySet {
  object empty extends SimpleIdentitySet[Nothing] {
    def size = 0
    def + [E <: AnyRef](x: E): SimpleIdentitySet[E] =
      new Set1[E](x)
    def - [E <: AnyRef](x: E): SimpleIdentitySet[Nothing] =
      this
    def contains[E <: AnyRef](x: E): Boolean = false
    def foreach(f: Nothing => Unit): Unit = ()
  }

  private class Set1[+Elem <: AnyRef](x0: AnyRef) extends SimpleIdentitySet[Elem] {
    def size = 1
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] =
      new Set2[E](x0, x)
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] =
      if (x `eq` x0) empty else this
    def contains[E >: Elem <: AnyRef](x: E): Boolean = x `eq` x0
    def foreach(f: Elem => Unit): Unit = f(x0.asInstanceOf[Elem])
  }

  private class Set2[+Elem <: AnyRef](x0: AnyRef, x1: AnyRef) extends SimpleIdentitySet[Elem] {
    def size = 2
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] = {
      val xs = new Array[AnyRef](3)
      xs(0) = x0
      xs(1) = x1
      xs(2) = x
      new SetN[E](xs)
    }
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] =
      if (x `eq` x0) new Set1(x1)
      else if (x `eq` x1) new Set1(x0)
      else this
    def contains[E >: Elem <: AnyRef](x: E): Boolean = (x `eq` x0) || (x `eq` x1)
    def foreach(f: Elem => Unit): Unit = { f(x0.asInstanceOf[Elem]); f(x1.asInstanceOf[Elem]) }
  }

  private class SetN[+Elem <: AnyRef](xs: Array[AnyRef]) extends SimpleIdentitySet[Elem] {
    def size = xs.length
    def + [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[E] = {
      val xs1 = new Array[AnyRef](size + 1)
      Array.copy(xs, 0, xs1, 0, size)
      xs1(size) = x
      new SetN[E](xs1)
    }
    def - [E >: Elem <: AnyRef](x: E): SimpleIdentitySet[Elem] = {
      var i = 0
      while (i < size && (xs(i) `ne` x)) i += 1
      if (i == size) this
      else if (size == 3)
        if (i == 0) new Set2(xs(1), xs(2))
        else if (i == 1) new Set2(xs(0), xs(2))
        else new Set2(xs(0), xs(1))
      else {
        val xs1 = new Array[AnyRef](size - 1)
        Array.copy(xs, 0, xs1, 0, i)
        Array.copy(xs, i + 1, xs1, i, size - (i + 1))
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
  }
}