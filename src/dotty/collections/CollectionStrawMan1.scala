package strawman.collections

import Predef.{augmentString => _, wrapString => _, _}
import scala.reflect.ClassTag
import collection.mutable.ListBuffer

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

  /** Iterator guaranteed to be usable multiple times */
  trait HasIterator[+A] extends CanIterate[A]

  /** Base trait for instances that can construct a collection from an iterator */
  trait FromIterator[+C[X] <: Iterable[X]] {
    def fromIterator[B](it: Iterator[B]): C[B]
  }

  /** Base trait for companion objects of collections */
  trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterator[C] {
    def empty[X]: C[X] = fromIterator(Iterator.empty)
    def apply[A](xs: A*): C[A] = fromIterator(Iterator(xs: _*))
  }

  /** Base trait for generic collections */
  trait Iterable[+IA] extends HasIterator[IA] with FromIterator[Iterable] {
    def buildIterator: Iterator[IA] = iterator
  }

  /** Base trait for sequence collections */
  trait Seq[+AA] extends Iterable[AA] with FromIterator[Seq] {
    def apply(i: Int): AA
    def length: Int
  }

  /* ------------ Operations ----------------------------------- */

  /** Operations returning types unrelated to current collection */
  trait Ops[A] extends Any {
    def toIterator: Iterator[A]
    def foreach(f: A => Unit): Unit = toIterator.foreach(f)
    def foldLeft[B](z: B)(op: (B, A) => B): B = toIterator.foldLeft(z)(op)
    def foldRight[B](z: B)(op: (A, B) => B): B = toIterator.foldRight(z)(op)
    def indexWhere(p: A => Boolean): Int = toIterator.indexWhere(p)
    def head: A = toIterator.next
    def view: View[A] = new View(toIterator)
    def collect[C[X] <: Iterable[X]](fi: FromIterator[C]): C[A] = fi.fromIterator(toIterator)
  }

  /** Transforms returning same collection type */
  trait MonoTransforms[A, Repr] extends Any {
    def toIterator: Iterator[A]
    def fromIterator(it: => Iterator[A]): Repr
    def partition(p: A => Boolean): (Repr, Repr) = {
      val (xs, ys) = toIterator.partition(p)
      (fromIterator(xs), fromIterator(ys))
    }
    def drop(n: Int): Repr = fromIterator(toIterator.drop(n))
  }

  /** Transforms returning same collection type constructor */
  trait PolyTransforms[A, C[X]] extends Any {
    def toIterator: Iterator[A]
    def fromIterator[B](it: => Iterator[B]): C[B]
    def map[B](f: A => B): C[B] = fromIterator(toIterator.map(f))
    def flatMap[B](f: A => CanIterate[B]): C[B] = fromIterator(toIterator.flatMap(f(_)))
    def ++[B >: A](xs: CanIterate[B]): C[B] = fromIterator(toIterator ++ xs)
    def zip[B](xs: CanIterate[B]): C[(A, B)] = fromIterator(toIterator.zip(xs.iterator))
  }

  /** Transforms that only apply to Seq */
  trait MonoTransformsOfSeqs[A, Repr] extends Any with MonoTransforms[A, Repr] {
    def reverse: Repr = fromIterator(toIterator.reverse)
  }

  /** Implementation of Ops for all generic collections */
  implicit class IterableOps[A](val c: Iterable[A])
  extends AnyVal with Ops[A] {
    def toIterator = c.iterator
  }

  /** Implementation of MonoTransforms for all generic collections */
  implicit class IterableMonoTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterator[C])
  extends AnyVal with MonoTransforms[A, C[A]] {
    def toIterator = c.buildIterator
    def fromIterator(it: => Iterator[A]): C[A] = c.fromIterator(it)
  }

  /** Implementation of PolyTransforms for all generic collections */
  implicit class IterablePolyTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterator[C])
  extends AnyVal with PolyTransforms[A, C] {
    def toIterator = c.buildIterator
    def fromIterator[B](it: => Iterator[B]) = c.fromIterator(it)
  }

  /** Implementation of MonoTransformsForSeqs for all generic collections */
  implicit class SeqMonoTransforms[A, C[X] <: Seq[X]](val c: Seq[A] with FromIterator[C])
  extends AnyVal with MonoTransformsOfSeqs[A, C[A]] {
    def toIterator = c.buildIterator
    def fromIterator(it: => Iterator[A]) = c.fromIterator(it)
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

  case object List extends IterableFactory[List] {
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

  case object Nil extends List[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  /** Concrete collection type: View */
  class View[+A](it: => Iterator[A]) extends HasIterator[A] {
    def iterator = it
  }

  implicit class ViewOps[A](val v: View[A]) extends AnyVal with Ops[A] {
    def toIterator = v.iterator
    def cache = collect(List).view
  }

  implicit class ViewMonoTransforms[A](val v: View[A])
  extends AnyVal with MonoTransforms[A, View[A]] {
    def toIterator = v.iterator
    def fromIterator(it: => Iterator[A]): View[A] = new View(it)
  }

  implicit class ViewPolyTransforms[A](val v: View[A])
  extends AnyVal with PolyTransforms[A, View] {
    def toIterator = v.iterator
    def fromIterator[B](it: => Iterator[B]) = new View(it)
  }

  /** Concrete collection type: String */
  implicit class StringOps(val s: String) extends AnyVal with Ops[Char] {
    def iterator: Iterator[Char] = new RandomAccessIterator[Char] {
      def length = s.length
      def apply(n: Int) = s.charAt(n)
    }
    def toIterator = iterator
  }

  implicit class StringMonoTransforms(val s: String)
  extends AnyVal with MonoTransformsOfSeqs[Char, String] {
    def toIterator = s.iterator
    def fromIterator(it: => Iterator[Char]) = {
      val sb = new StringBuilder
      for (ch <- it) sb.append(ch)
      sb.toString
    }
  }

  implicit class StringPolyTransforms(val s: String)
  extends AnyVal with PolyTransforms[Char, Seq] {
    def toIterator = s.iterator
    def fromIterator[B](it: => Iterator[B]) = List.fromIterator(it)
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
    def map[B](f: A => B): Iterator[B] = new Iterator[B] {
      def hasNext = self.hasNext
      def next = f(self.next)
    }
    def flatMap[B](f: A => CanIterate[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = Iterator.empty
      private def current = {
        while (!myCurrent.hasNext && self.hasNext) myCurrent = f(self.next).iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next = current.next
    }
    def ++[B >: A](xs: CanIterate[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = self
      private def current = {
        if (!myCurrent.hasNext && myCurrent.eq(self)) myCurrent = xs.iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next = current.next
    }
    def partition(p: A => Boolean): (Iterator[A], Iterator[A]) = {
      val lookaheadTrue, lookaheadFalse = new ListBuffer[A]
      class PartIterator[+A](buf: ListBuffer[A]) extends Iterator[A] {
        final def hasNext: Boolean =
          buf.nonEmpty ||
            self.hasNext && {
              val elem = self.next
              (if (p(elem)) lookaheadTrue else lookaheadFalse) += elem
              hasNext
            }
        final def next =
          if (hasNext) {
            val r = buf.head
            buf.trimStart(1)
            r
          }
          else Iterator.nextOnEmpty
      }
      (new PartIterator(lookaheadTrue), new PartIterator(lookaheadFalse))
    }
    def drop(n: Int): Iterator[A] = {
      if (n <= 0) this
      else {
        next
        drop(n - 1)
      }
    }
    def zip[B](that: CanIterate[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
      val ti = that.iterator
      def hasNext = self.hasNext && ti.hasNext
      def next = (self.next, ti.next)
    }
    def reverse: Iterator[A] = {
      var elems: List[A] = Nil
      while (hasNext) elems = Cons(next, elems)
      elems.iterator
    }
  }

  object Iterator {
    val empty: Iterator[Nothing] = new Iterator[Nothing] {
      def hasNext = false
      def next = ???
    }
    def apply[A](xs: A*): Iterator[A] = new RandomAccessIterator[A] {
      def length = xs.length
      def apply(n: Int) = xs(n)
    }
    def nextOnEmpty = throw new NoSuchElementException("next on empty iterator")
  }

  trait RandomAccessIterator[+A] extends Iterator[A] { self =>
    def apply(n: Int): A
    def length: Int
    protected val len = length
    private var cur = 0
    def hasNext = cur < len
    def next: A = { val r = this(cur); cur += 1; r }
    override def drop(n: Int): Iterator[A] = { cur += (n max 0); this }
    override def reverse = new RandomAccessIterator[A] {
      def length = self.length
      def apply(n: Int) = apply(len - 1 - n)
    }
  }
}

