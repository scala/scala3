package strawman.collections

import Predef.{augmentString as _, wrapString as _, *}
import scala.reflect.ClassTag

/** A strawman architecture for new collections. It contains some
 *  example collection classes and methods with the intent to expose
 *  some key issues. It would be good to compare this to other
 *  implementations of the same functionality, to get an idea of the
 *  strengths and weaknesses of different collection architectures.
 *
 *  For a test file, see tests/run/CollectionTests.scala.
 */
object CollectionStrawMan1 {

  /* ------------ Base Traits -------------------------------- */

  /** Replaces TraversableOnce */
  trait CanIterate[+A] {
    def iterator: Iterator[A]
  }

  /** Base trait for instances that can construct a collection from an iterator */
  trait FromIterator[+C[X] <: Iterable[X]] {
    def fromIterator[B](it: Iterator[B]): C[B]
  }

  /** Base trait for companion objects of collections */
  trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterator[C] {
    def empty[X]: C[X] = fromIterator(Iterator.empty)
    def apply[A](xs: A*): C[A] = fromIterator(Iterator(xs*))
  }

  /** Base trait for generic collections */
  trait Iterable[+A] extends CanIterate[A] with FromIterator[Iterable]

  /** Base trait for sequence collections */
  trait Seq[+A] extends Iterable[A] with FromIterator[Seq] {
    def apply(i: Int): A
    def length: Int
  }

  /* ------------ Operations ----------------------------------- */

  /** Operations returning types unrelated to current collection */
  trait Ops[A] extends Any {
    def iterator: Iterator[A]
    def foreach(f: A => Unit): Unit = iterator.foreach(f)
    def foldLeft[B](z: B)(op: (B, A) => B): B = iterator.foldLeft(z)(op)
    def foldRight[B](z: B)(op: (A, B) => B): B = iterator.foldRight(z)(op)
    def indexWhere(p: A => Boolean): Int = iterator.indexWhere(p)
    def isEmpty: Boolean = !iterator.hasNext
    def head: A = iterator.next
    def view: View[A] = new View(iterator)
    def to[C[X] <: Iterable[X]](fi: FromIterator[C]): C[A] = fi.fromIterator(iterator)
  }

  /** Transforms returning same collection type */
  trait MonoTransforms[A, Repr] extends Any {
    protected def iter: Iterator[A]
    protected def fromIter(it: => Iterator[A]): Repr
    def partition(p: A => Boolean): (Repr, Repr) = {
      val (xs, ys) = iter.partition(p)
      (fromIter(xs), fromIter(ys))
    }
    def drop(n: Int): Repr = fromIter(iter.drop(n))
  }

  /** Transforms returning same collection type constructor */
  trait PolyTransforms[A, C[X]] extends Any {
    protected def iter: Iterator[A]
    protected def fromIter[B](it: => Iterator[B]): C[B]
    def map[B](f: A => B): C[B] = fromIter(iter.map(f))
    def flatMap[B](f: A => CanIterate[B]): C[B] = fromIter(iter.flatMap(f(_)))
    def ++[B >: A](xs: CanIterate[B]): C[B] = fromIter(iter ++ xs)
    def zip[B](xs: CanIterate[B]): C[(A, B)] = fromIter(iter.zip(xs.iterator))
  }

  /** Transforms that only apply to Seq */
  trait MonoTransformsOfSeqs[A, Repr] extends Any with MonoTransforms[A, Repr] {
    def reverse: Repr = fromIter(iter.reverse)
  }

  /** Implementation of Ops for all generic collections */
  implicit class IterableOps[A](val c: Iterable[A])
  extends AnyVal with Ops[A] {
    def iterator = c.iterator
  }

  /** Implementation of MonoTransforms for all generic collections */
  implicit class IterableMonoTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterator[C])
  extends AnyVal with MonoTransforms[A, C[A]] {
    protected def iter = c.iterator
    protected def fromIter(it: => Iterator[A]): C[A] = c.fromIterator(it)
  }

  /** Implementation of PolyTransforms for all generic collections */
  implicit class IterablePolyTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterator[C])
  extends AnyVal with PolyTransforms[A, C] {
    protected def iter = c.iterator
    protected def fromIter[B](it: => Iterator[B]) = c.fromIterator(it)
  }

  /** Implementation of MonoTransformsForSeqs for all generic collections */
  implicit class SeqMonoTransforms[A, C[X] <: Seq[X]](val c: Seq[A] with FromIterator[C])
  extends AnyVal with MonoTransformsOfSeqs[A, C[A]] {
    protected def iter = c.iterator
    protected def fromIter(it: => Iterator[A]) = c.fromIterator(it)
  }

  /* --------- Concrete collection types ------------------------------- */

  /** Concrete collection type: List */
  sealed trait List[+A] extends Seq[A] with FromIterator[List] {
    def isEmpty: Boolean
    def head: A
    def tail: List[A]
    def iterator = new ListIterator[A](this)
    def fromIterator[B](it: Iterator[B]): List[B] = List.fromIterator(it)
    def apply(i: Int): A = {
      require(!isEmpty)
      if (i == 0) head else tail.apply(i - 1)
    }
    def length: Int =
      if (isEmpty) 0 else 1 + tail.length
  }

  case class Cons[+A](x: A, xs: List[A]) extends List[A] {
    def isEmpty = false
    def head = x
    def tail = xs
  }

  case object Nil extends List[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  object List extends IterableFactory[List] {
    def fromIterator[B](it: Iterator[B]): List[B] = it match {
      case it: ListIterator[B] => it.toList
      case _ => if (it.hasNext) Cons(it.next, fromIterator(it)) else Nil
    }
  }

  class ListIterator[+A](xs: List[A]) extends Iterator[A] {
    private[this] var current = xs
    def hasNext = !current.isEmpty
    def next = { val r = current.head; current = current.tail; r }
    def toList = current
  }

  /** Concrete collection type: ArrayBuffer */
  class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int) extends Seq[A] with FromIterator[ArrayBuffer] {
    def this() = this(new Array[AnyRef](16), 0)
    private var elems: Array[AnyRef] = initElems
    private var start = 0
    private var limit = initLength
    def apply(i: Int) = elems(start + i).asInstanceOf[A]
    def length = limit - start
    def iterator = new ArrayBufferIterator[A](elems, start, length)
    def fromIterator[B](it: Iterator[B]): ArrayBuffer[B] =
      ArrayBuffer.fromIterator(it)
    def +=(elem: A): this.type = {
      if (limit == elems.length) {
        if (start > 0) {
          Array.copy(elems, start, elems, 0, length)
          limit -= start
          start = 0
        }
        else {
          val newelems = new Array[AnyRef](limit * 2)
          Array.copy(elems, 0, newelems, 0, limit)
          elems = newelems
        }
      }
      elems(limit) = elem.asInstanceOf[AnyRef]
      limit += 1
      this
    }
    def trimStart(n: Int): Unit = start += (n max 0)
    override def toString = s"ArrayBuffer(${elems.slice(start, limit).mkString(", ")})"
  }

  object ArrayBuffer extends IterableFactory[ArrayBuffer] {
    def fromIterator[B](it: Iterator[B]): ArrayBuffer[B] = it match {
      case Iterator.Concat(fst: ArrayBufferIterator[_], snd: ArrayBufferIterator[_]) =>
        val elems = new Array[AnyRef](fst.remaining + snd.remaining)
        Array.copy(fst.elems, fst.start, elems, 0, fst.remaining)
        Array.copy(snd.elems, snd.start, elems, fst.remaining, snd.remaining)
        new ArrayBuffer(elems, elems.length)
      case it @ Iterator.Partition(underlying, _, buf, _) =>
        while (underlying.hasNext) it.distribute()
        buf.asInstanceOf[ArrayBuffer[B]]
      case it if it.remaining >= 0 =>
        val elems = new Array[AnyRef](it.remaining)
        for (i <- 0 until elems.length) elems(i) = it.next.asInstanceOf[AnyRef]
        new ArrayBuffer[B](elems, elems.length)
      case _ =>
        val buf = new ArrayBuffer[B]
        while (it.hasNext) buf += it.next
        buf
    }
  }

  class ArrayBufferIterator[A](val elems: Array[AnyRef], initStart: Int, length: Int) extends RandomAccessIterator[A] {
    val limit = length
    def apply(n: Int) = elems(initStart + n).asInstanceOf[A]
  }

  /** Concrete collection type: View */
  class View[+A](it: => Iterator[A]) extends CanIterate[A] {
    def iterator = it
  }

  implicit class ViewOps[A](val v: View[A]) extends AnyVal with Ops[A] {
    def iterator = v.iterator
    def cache = to(ArrayBuffer).view
  }

  implicit class ViewMonoTransforms[A](val v: View[A])
  extends AnyVal with MonoTransforms[A, View[A]] {
    protected def iter = v.iterator
    protected def fromIter(it: => Iterator[A]): View[A] = new View(it)
  }

  implicit class ViewPolyTransforms[A](val v: View[A])
  extends AnyVal with PolyTransforms[A, View] {
    protected def iter = v.iterator
    protected def fromIter[B](it: => Iterator[B]) = new View(it)
  }

  /** Concrete collection type: String */
  implicit class StringOps(val s: String) extends AnyVal with Ops[Char] {
    def iterator: Iterator[Char] = new RandomAccessIterator[Char] {
      override val limit = s.length
      def apply(n: Int) = s.charAt(n)
    }
  }

  implicit class StringMonoTransforms(val s: String)
  extends AnyVal with MonoTransformsOfSeqs[Char, String] {
    protected def iter = StringOps(s).iterator
    protected def fromIter(it: => Iterator[Char]) = {
      val sb = new StringBuilder
      for (ch <- it) sb.append(ch)
      sb.toString
    }
  }

  implicit class StringPolyTransforms(val s: String)
  extends AnyVal with PolyTransforms[Char, Seq] {
    protected def iter = StringOps(s).iterator
    protected def fromIter[B](it: => Iterator[B]) = List.fromIterator(it)
    def map(f: Char => Char): String = {
      val sb = new StringBuilder
      for (ch <- s) sb.append(f(ch))
      sb.toString
    }
    def flatMap(f: Char => String) = {
      val sb = new StringBuilder
      for (ch <- s) sb.append(f(ch))
      sb.toString
    }
    def ++(xs: CanIterate[Char]): String = {
      val sb = new StringBuilder(s)
      for (ch <- xs.iterator) sb.append(ch)
      sb.toString
    }
    def ++(xs: String): String = s + xs
  }

/* ---------- Iterators --------------------------------------------------- */

  /** A core Iterator class */
  trait Iterator[+A] extends CanIterate[A] { self =>
    def hasNext: Boolean
    def next: A
    def iterator = this
    def foldLeft[B](z: B)(op: (B, A) => B): B =
      if (hasNext) foldLeft(op(z, next))(op) else z
    def foldRight[B](z: B)(op: (A, B) => B): B =
      if (hasNext) op(next, foldRight(z)(op)) else z
    def foreach(f: A => Unit): Unit =
      while (hasNext) f(next)
    def indexWhere(p: A => Boolean): Int = {
      var i = 0
      while (hasNext) {
        if (p(next)) return i
        i += 1
      }
      -1
    }
    def map[B](f: A => B): Iterator[B] = Iterator.Map(this, f)
    def flatMap[B](f: A => CanIterate[B]): Iterator[B] = Iterator.FlatMap(this, f)
    def ++[B >: A](xs: CanIterate[B]): Iterator[B] = Iterator.Concat(this, xs.iterator)
    def partition(p: A => Boolean): (Iterator[A], Iterator[A]) = {
      val lookaheadTrue, lookaheadFalse = new ArrayBuffer[A]
      (Iterator.Partition(this, p, lookaheadTrue, lookaheadFalse),
       Iterator.Partition[A](this, !p(_), lookaheadFalse, lookaheadTrue))
    }
    def drop(n: Int): Iterator[A] = Iterator.Drop(this, n)
    def zip[B](that: CanIterate[B]): Iterator[(A, B)] = Iterator.Zip(this, that.iterator)
    def reverse: Iterator[A] = {
      var elems: List[A] = Nil
      while (hasNext) elems = Cons(next, elems)
      elems.iterator
    }

    /** If this iterator results from applying a transfomation to another iterator,
     *  that other iterator, otherwise the iterator itself.
     */
    def underlying: Iterator[_] = this

    /** If the number of elements still to be returned by this iterator is known,
     *  that number, otherwise -1.
     */
    def remaining = -1
  }

  object Iterator {
    val empty: Iterator[Nothing] = new Iterator[Nothing] {
      def hasNext = false
      def next = ???
      override def remaining = 0
    }
    def apply[A](xs: A*): Iterator[A] = new RandomAccessIterator[A] {
      override val limit = xs.length
      def apply(n: Int) = xs(n)
    }
    def nextOnEmpty = throw new NoSuchElementException("next on empty iterator")

    case class Map[A, B](override val underlying: Iterator[A], f: A => B) extends Iterator[B] {
      def hasNext = underlying.hasNext
      def next = f(underlying.next)
      override def remaining = underlying.remaining
    }
    case class FlatMap[A, B](override val underlying: Iterator[A], f: A => CanIterate[B]) extends Iterator[B] {
      private var myCurrent: Iterator[B] = Iterator.empty
      private def current = {
        while (!myCurrent.hasNext && underlying.hasNext)
          myCurrent = f(underlying.next).iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next = current.next
    }
    case class Concat[A](override val underlying: Iterator[A], other: Iterator[A]) extends Iterator[A] {
      private var myCurrent = underlying
      private def current = {
        if (!myCurrent.hasNext && myCurrent.eq(underlying)) myCurrent = other
        myCurrent
      }
      def hasNext = current.hasNext
      def next = current.next
      override def remaining =
        if (underlying.remaining >= 0 && other.remaining >= 0)
          underlying.remaining + other.remaining
        else -1
    }
    case class Partition[A](override val underlying: Iterator[A], p: A => Boolean, lookahead: ArrayBuffer[A], dual: ArrayBuffer[A]) extends Iterator[A] {
      def distribute() = {
        val elem = underlying.next
        (if (p(elem)) lookahead else dual) += elem
      }
      final def hasNext: Boolean =
        !lookahead.isEmpty || underlying.hasNext && { distribute(); hasNext }
      final def next =
        if (hasNext) {
          val r = lookahead.head
          lookahead.trimStart(1)
          r
        } else Iterator.nextOnEmpty
    }
    case class Drop[A](override val underlying: Iterator[A], n: Int) extends Iterator[A] {
      var toSkip = n
      def hasNext: Boolean = underlying.hasNext && (
        toSkip == 0 || { underlying.next; toSkip -= 1; hasNext })
      def next = if (hasNext) underlying.next else nextOnEmpty
      override def remaining = (underlying.remaining - toSkip) max -1
    }
    case class Zip[A, B](override val underlying: Iterator[A], other: Iterator[B]) extends Iterator[(A, B)] {
      def hasNext = underlying.hasNext && other.hasNext
      def next = (underlying.next, other.next)
      override def remaining = underlying.remaining min other.remaining
    }
    case class Reverse[A](override val underlying: RandomAccessIterator[A]) extends RandomAccessIterator[A] {
      def apply(n: Int) = underlying.apply(underlying.limit - 1 - n)
      def limit = underlying.remaining
    }
  }

  trait RandomAccessIterator[+A] extends Iterator[A] { self =>
    def apply(n: Int): A
    def limit: Int
    var start = 0
    override def remaining = (limit - start) max 0
    def hasNext = start < limit
    def next: A = { val r = this(start); start += 1; r }
    override def drop(n: Int): Iterator[A] = { start += (n max 0); this }
    override def reverse: Iterator[A] = new Iterator.Reverse(this)
  }
}

