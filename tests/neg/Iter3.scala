package dotty1.collections
package immutable

import annotation.unchecked.uncheckedVariance

// Like Iter2, but with non-variant types only.
object Iter2 {

  trait Iterator[A] extends IterableOnce[A] {
    def hasNext: Boolean
    def next: A
    def iterator = this
    def foreach(f: A => Unit): Unit = ???
    def map[B](f: A => B): Iterator[B] = ???
    def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = ???
    def ++[B >: A](xs: IterableOnce[B]): Iterator[B] = ???
    def drop(n: Int): Iterator[A] = ???
    def indexWhere(p: A => Boolean): Int = {
      var i = 0
      while (hasNext) {
        if (p(next)) return i
        i += 1
      }
      -1
    }
    def zip[B](that: Iterator[B]): Iterator[(A, B)] = ???
  }

  trait IterableOnce[A] {
    def iterator: Iterator[A]
    def buildIterator: Iterator[A] = iterator
  }

  trait FromIterator[C[X] <: Iterable[X]] {
    def fromIterator[B](it: Iterator[B]): C[B]
  }

  trait Iterable[IA] extends IterableOnce[IA] with FromIterator[Iterable]

  trait Seq[AA] extends Iterable[AA] with FromIterator[Seq] {
    def apply(i: Int): AA
    def length: Int
  }

  sealed trait List[A] extends Seq[A] with FromIterator[List] {
    def isEmpty: Boolean
    def head: A
    def tail: List[A]
    def iterator = new ListIterator[A](this)
    def fromIterator[B](it: Iterator[B]): List[B] = it match {
      case ListIterator(xs) => xs
      case _ => if (it.hasNext) Cons(it.next, fromIterator(it)) else Nil.asInstanceOf[List[B]]
    }
    def apply(i: Int): A = {
      require(!isEmpty)
      if (i == 0) head else tail.apply(i - 1)
    }
    def length: Int =
      if (isEmpty) 0 else 1 + tail.length
  }

  case class Cons[A](x: A, xs: List[A]) extends List[A] { // error: cannot be instantiated
    def isEmpty = false
    def head = x
    def tail = xs
  }

  case object Nil extends List[Nothing] { // error: cannot be instantiated
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  class ArrayBuffer[A] private (initElems: Array[AnyRef], initLen: Int) extends Seq[A] with FromIterator[ArrayBuffer] { // error: cannot be instantiated
    def this() = this(new Array[AnyRef](16), 0)
    def this(it: ArrayIterator[A]) = this(it.elems, it.len)
    private var elems: Array[AnyRef] = initElems
    private var len = 0
    def iterator =
      elems.iterator.take(len).asInstanceOf[Iterator[A]]
    override def buildIterator =
      new ArrayIterator(elems, len).asInstanceOf[Iterator[A]]
    def fromIterator[B](it: Iterator[B]): ArrayBuffer[B] =
      new ArrayBuffer(ArrayIterator.fromIterator(it))
    def apply(i: Int) = elems(i).asInstanceOf[A]
    def length = len
  }

  implicit class IterableTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] & FromIterator[C]) extends AnyVal {
    def map[B](f: A => B): C[B] = c.fromIterator(c.buildIterator.map(f))
    def flatMap[B](f: A => IterableOnce[B]): C[B] = c.fromIterator(c.buildIterator.flatMap(f(_).buildIterator))
    def ++[B >: A](xs: IterableOnce[B]): C[B] = c.fromIterator(c.buildIterator ++ xs.buildIterator)
    def drop(n: Int): C[A] = c.fromIterator(c.buildIterator.drop(n))
    def head: A = c.iterator.next
    def zip[B](xs: IterableOnce[B]): C[(A, B)] = c.fromIterator(c.iterator.zip(xs.iterator))
  }

  implicit class SeqTransforms[SA, C[X] <: Seq[X]](val c: Seq[SA] & FromIterator[C]) extends AnyVal {
    def reverse: C[SA] = {
      val elems = new Array[AnyRef](c.length)
      var i = elems.length
      val it = c.iterator
      while (it.hasNext) {
        i -= 1
        elems(i) = it.next.asInstanceOf[AnyRef]
      }
      val xzz = c.fromIterator(ArrayIterator[SA](elems, c.length))
      xzz
    }
    def indexWhere(p: SA => Boolean): Int = c.iterator.indexWhere(p)
  }

  case class ListIterator[A](xs: List[A]) extends Iterator[A] {
    private[this] var current: List[A] = xs
    def hasNext = !current.isEmpty
    def next = { val res = current.head; current = current.tail; res }
  }

  case class ArrayIterator[A](elems: Array[AnyRef], len: Int) extends Iterator[A] {
    import ArrayIterator.*

    private def elem(i: Int) = elems(i).asInstanceOf[A]

    private var cur = 0

    def hasNext = cur < len
    def next = { val res = elem(cur); cur += 1; res }

    override def foreach(f: A => Unit): Unit =
      for (i <- 0 until len) f(elem(i))

    override def map[B](f: A => B): ArrayIterator[B] = {
      var mapped = elems
      for (i <- 0 until len) {
        val x = elem(i)
        val y = widen(f(x))
        if (widen(x) ne y) {
          if (mapped eq elems) mapped = new Array[AnyRef](len)
          mapped(i) = y
        }
      }
      if (mapped eq elems) this.asInstanceOf[ArrayIterator[B]]
      else new ArrayIterator(mapped, len)
    }

    override def flatMap[B](f: A => IterableOnce[B]): ArrayIterator[B] =
      flatten(map(f(_).buildIterator))

    override def ++[B >: A](that: IterableOnce[B]): ArrayIterator[B] = {
      val thatIterator @ ArrayIterator(elems2, len2) = fromIterator(that.iterator)
      if (len == 0) thatIterator
      else if (len2 == 0) this.asInstanceOf[ArrayIterator[B]]
      else {
        val resLen = len + len2
        val resElems = new Array[AnyRef](resLen)
        Array.copy(elems, 0, resElems, 0, len)
        Array.copy(elems2, 0, resElems, len, len2)
        new ArrayIterator(resElems, resLen)
      }
    }
  }

  object ArrayIterator {
    private def widen(x: Any): AnyRef = x.asInstanceOf[AnyRef]

    def fromIterator[A](it: Iterator[A]): ArrayIterator[A] = it match {
      case it: ArrayIterator[A] => it
      case _ =>
        var elems = new Array[AnyRef](32)
        var len = 0
        def ensureCapacity() = {
          while (len > elems.length) {
            val newElems = new Array[AnyRef](elems.length * 2)
            Array.copy(elems, 0, newElems, 0, elems.length)
            elems = newElems
          }
        }
        while (it.hasNext) {
          len += 1
          ensureCapacity()
          elems(len - 1) = widen(it.next)
        }
        ArrayIterator(elems, len)
    }

    def flatten[A](its: ArrayIterator[Iterator[A]]): ArrayIterator[A] = {
      var arrayIts = its.map(fromIterator)
      var totalLen = 0
      arrayIts.foreach(totalLen += _.len)
      val allElems = new Array[AnyRef](totalLen)
      var j = 0
      arrayIts.foreach { it =>
        Array.copy(it.elems, 0, allElems, j, it.len)
        j += it.len
      }
      new ArrayIterator(allElems, totalLen)
    }
  }
}
