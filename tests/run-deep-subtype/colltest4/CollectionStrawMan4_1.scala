package colltest4
package strawman.collections

import Predef.{augmentString as _, wrapString as _, *}
import scala.reflect.ClassTag
import annotation.unchecked.uncheckedVariance
import annotation.tailrec

/** A strawman architecture for new collections. It contains some
 *  example collection classes and methods with the intent to expose
 *  some key issues. It would be good to compare this to other
 *  implementations of the same functionality, to get an idea of the
 *  strengths and weaknesses of different collection architectures.
 *
 *  For a test file, see tests/run/CollectionTests.scala.
 */
object CollectionStrawMan4 {

  /* ------------ Base Traits -------------------------------- */

  /** Iterator can be used only once */
  trait IterableOnce[+A] {
    def iterator: Iterator[A]
  }

  /** Base trait for instances that can construct a collection from an iterable */
  trait FromIterable[+C[X] <: Iterable[X]] {
    def fromIterable[B](v: Iterable[B]): C[B]
  }

  /** Base trait for companion objects of collections */
  trait IterableFactory[+C[X] <: Iterable[X]] extends FromIterable[C] {
    def empty[X]: C[X] = fromIterable(View.Empty)
    def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs*))
  }

  /** Base trait for generic collections */
  trait Iterable[+A] extends IterableOnce[A] with FromIterable[Iterable] {
    def view: View[A] = View.fromIterator(iterator) // view is overridden, cannot be defined in ops
    def knownLength: Int = -1
  }

  /** Base trait for sequence collections */
  trait Seq[+A] extends Iterable[A] with FromIterable[Seq] {
    def apply(i: Int): A
    def length: Int
  }

  /** Base trait for collection builders */
  trait Builder[-A, +To] {
    def +=(x: A): this.type
    def result: To

    def ++=(xs: IterableOnce[A]): this.type = {
      xs.iterator.foreach(+=)
      this
    }
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
    def head: A = iterator.next()
  }

  /** Transforms returning same collection type */
  trait MonoTransforms[A, Repr] extends Any {
    protected def coll: Iterable[A]
    protected def fromIterable(it: Iterable[A]): Repr
    def filter(p: A => Boolean): Repr = fromIterable(View.Filter(coll, p))
    def partition(p: A => Boolean): (Repr, Repr) = {
      val pn = View.Partition(coll, p)
      (fromIterable(pn.left), fromIterable(pn.right))
    }
    def drop(n: Int): Repr = fromIterable(View.Drop(coll, n))
    def to[C[X] <: Iterable[X]](fv: FromIterable[C]): C[A] = fv.fromIterable(coll)
  }

  trait PolyTransforms[A, C[X]] extends Any {
    protected def coll: Iterable[A]
    protected def fromIterable[B](it: Iterable[B]): C[B]
    def map[B](f: A => B): C[B] = fromIterable(View.Map(coll, f))
    def flatMap[B](f: A => IterableOnce[B]): C[B] = fromIterable(View.FlatMap(coll, f))
    def ++[B >: A](xs: IterableOnce[B]): C[B] = fromIterable(View.Concat(coll, xs))
    def zip[B](xs: IterableOnce[B]): C[(A, B)] = fromIterable(View.Zip(coll, xs))
  }

  /** Transforms that only apply to Seq */
  trait MonoTransformsOfSeqs[A, Repr] extends Any with MonoTransforms[A, Repr] {
    def reverse: Repr = fromIterable(View.Reverse(coll))
  }

  /** Implementation of Ops for all generic collections */
  implicit class IterableOps[A](val c: Iterable[A])
  extends AnyVal with Ops[A] {
    def iterator = c.iterator
  }

  /** Implementation of MonoTransforms for all generic collections */
  implicit class IterableMonoTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterable[C])
  extends AnyVal with MonoTransforms[A, C[A]] {
    protected def coll = c
    protected def fromIterable(it: Iterable[A]): C[A] = c.fromIterable(it)
  }

  /** Implementation of PolyTransforms for all generic collections */
  implicit class IterablePolyTransforms[A, C[X] <: Iterable[X]](val c: Iterable[A] with FromIterable[C])
  extends AnyVal with PolyTransforms[A, C] {
    protected def coll = c
    protected def fromIterable[B](it: Iterable[B]): C[B] = c.fromIterable(it)
  }

  /** Implementation of MonoTransformsForSeqs for all generic collections */
  implicit class SeqMonoTransforms[A, C[X] <: Seq[X]](val c: Seq[A] with FromIterable[C])
  extends AnyVal with MonoTransformsOfSeqs[A, C[A]] {
    protected def coll = c
    protected def fromIterable(it: Iterable[A]): C[A] = c.fromIterable(it)
  }

  /* --------- Concrete collection types ------------------------------- */

  /** Concrete collection type: List */
  sealed trait List[+A] extends Seq[A] with FromIterable[List] { self =>
    def isEmpty: Boolean
    def head: A
    def tail: List[A]
    def iterator = new Iterator[A] {
      private[this] var current = self
      def hasNext = !current.isEmpty
      def next() = { val r = current.head; current = current.tail; r }
    }
    def fromIterable[B](c: Iterable[B]): List[B] = List.fromIterable(c)
    def apply(i: Int): A = {
      require(!isEmpty)
      if (i == 0) head else tail.apply(i - 1)
    }
    def length: Int =
      if (isEmpty) 0 else 1 + tail.length
    def ++:[B >: A](prefix: List[B]): List[B] =
      if (prefix.isEmpty) this
      else Cons(prefix.head, prefix.tail ++: this)
  }

  case class Cons[+A](x: A, private[collections] var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
    def isEmpty = false
    def head = x
    def tail = next
  }

  case object Nil extends List[Nothing] {
    def isEmpty = true
    def head = ???
    def tail = ???
  }

  object List extends IterableFactory[List] {
    def fromIterator[B](it: Iterator[B]): List[B] =
      if (it.hasNext) Cons(it.next(), fromIterator(it)) else Nil
    def fromIterable[B](c: Iterable[B]): List[B] = c match {
      case View.Concat(xs, ys: List[B]) =>
        fromIterable(xs) ++: ys
      case View.Drop(xs: List[B], n) =>
        @tailrec def loop(xs: List[B], n: Int): List[B] =
          if (n > 0) loop(xs.tail, n - 1) else xs
        loop(xs, n)
      case c: List[B] => c
      case _ => fromIterator(c.iterator)
    }
  }

  /** Concrete collection type: ListBuffer */
  class ListBuffer[A] extends Seq[A] with FromIterable[ListBuffer] with Builder[A, List[A]] {
    private var first, last: List[A] = Nil
    private var aliased = false
    def iterator = first.iterator
    def fromIterable[B](coll: Iterable[B]) = ListBuffer.fromIterable(coll)
    def apply(i: Int) = first.apply(i)
    def length = first.length

    private def copyElems(): Unit = {
      val buf = ListBuffer.fromIterable(result)
      first = buf.first
      last = buf.last
      aliased = false
    }
    def result = {
      aliased = true
      first
    }
    def +=(elem: A) = {
      if (aliased) copyElems()
      val last1 = Cons(elem, Nil)
      last match {
        case last: Cons[A] => last.next = last1
        case _ => first = last1
      }
      last = last1
      this
    }
    override def toString: String =
      if (first.isEmpty) "ListBuffer()"
      else {
        val b = new StringBuilder("ListBuffer(").append(first.head)
        first.tail.foldLeft(b)(_.append(", ").append(_)).append(")").toString
      }
  }

  object ListBuffer extends IterableFactory[ListBuffer] {
    def fromIterable[B](coll: Iterable[B]): ListBuffer[B] = coll match {
      case pd @ View.Partitioned(partition: View.Partition[B]) =>
        partition.distribute(new ListBuffer[B]())
        new ListBuffer[B] ++= pd.forced.get
      case _ =>
        new ListBuffer[B] ++= coll
    }
  }

  /** Concrete collection type: ArrayBuffer */
  class ArrayBuffer[A] private (initElems: Array[AnyRef], initLength: Int)
  extends Seq[A] with FromIterable[ArrayBuffer] with Builder[A, ArrayBuffer[A]] {
    def this() = this(new Array[AnyRef](16), 0)
    private var elems: Array[AnyRef] = initElems
    private var start = 0
    private var end = initLength
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
    def length = end - start
    override def knownLength = length
    override def view = new ArrayBufferView(elems, start, end)
    def iterator = view.iterator
    def fromIterable[B](it: Iterable[B]): ArrayBuffer[B] =
      ArrayBuffer.fromIterable(it)
    def +=(elem: A): this.type = {
      if (end == elems.length) {
        if (start > 0) {
          Array.copy(elems, start, elems, 0, length)
          end -= start
          start = 0
        }
        else {
          val newelems = new Array[AnyRef](end * 2)
          Array.copy(elems, 0, newelems, 0, end)
          elems = newelems
        }
      }
      elems(end) = elem.asInstanceOf[AnyRef]
      end += 1
      this
    }
    def result = this
    def trimStart(n: Int): Unit = start += (n max 0)
    override def toString = s"ArrayBuffer(${elems.slice(start, end).mkString(", ")})"
  }

  object ArrayBuffer extends IterableFactory[ArrayBuffer] {
    def fromIterable[B](c: Iterable[B]): ArrayBuffer[B] = c match {
      case View.Concat(fst: ArrayBuffer[B], snd: ArrayBuffer[B]) =>
        val elems = new Array[AnyRef](fst.length + snd.length)
        Array.copy(fst.elems, fst.start, elems, 0, fst.length)
        Array.copy(snd.elems, snd.start, elems, fst.length, snd.length)
        new ArrayBuffer(elems, elems.length)
      case pd @ View.Partitioned(partition: View.Partition[B]) =>
        partition.distribute(new ArrayBuffer[B]())
        pd.forced.get.asInstanceOf[ArrayBuffer[B]]
      case c if c.knownLength >= 0 =>
        val elems = new Array[AnyRef](c.knownLength)
        val it = c.iterator
        for (i <- 0 until elems.length) elems(i) = it.next().asInstanceOf[AnyRef]
        new ArrayBuffer[B](elems, elems.length)
      case _ =>
        val buf = new ArrayBuffer[B]
        val it = c.iterator
        while (it.hasNext) buf += it.next()
        buf
    }
  }

  class ArrayBufferView[A](val elems: Array[AnyRef], val start: Int, val end: Int) extends RandomAccessView[A] {
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
  }

  /** Concrete collection type: String */
  implicit class StringOps(val s: String) extends AnyVal with Ops[Char] {
    def iterator: Iterator[Char] = new StringView(s).iterator
  }

  implicit class StringMonoTransforms(val s: String)
  extends AnyVal with MonoTransformsOfSeqs[Char, String] {
    protected def coll: Iterable[Char] = StringView(s)
    protected def fromIterable(it: Iterable[Char]): String = {
      val sb = new StringBuilder
      for (ch <- it) sb.append(ch)
      sb.toString
    }
  }

  implicit class StringPolyTransforms(val s: String)
  extends AnyVal with PolyTransforms[Char, Seq] {
    protected def coll = StringView(s)
    protected def fromIterable[B](it: Iterable[B]): Seq[B] = List.fromIterable(it)
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
    def ++(xs: IterableOnce[Char]): String = {
      val sb = new StringBuilder(s)
      for (ch <- xs.iterator) sb.append(ch)
      sb.toString
    }
    def ++(xs: String): String = s + xs
  }

  case class StringView(s: String) extends RandomAccessView[Char] {
    val start = 0
    val end = s.length
    def apply(n: Int) = s.charAt(n)
  }

  /* ------------ Views --------------------------------------- */

  /** A lazy iterable */
  trait View[+A] extends Iterable[A] with FromIterable[View] {
    override def view = this
    override def fromIterable[B](c: Iterable[B]) = c match {
      case c: View[B] => c
      case _ => View.fromIterator(c.iterator)
    }
  }

  /** Iterator defined in terms of indexing a range */
  trait RandomAccessView[+A] extends View[A] {
    def start: Int
    def end: Int
    def apply(i: Int): A
    def iterator: Iterator[A] = new Iterator[A] {
      private var current = start
      def hasNext = current < end
      def next(): A = {
        val r = apply(current)
        current += 1
        r
      }
    }
    override def knownLength = end - start max 0
  }

  object View {
    def fromIterator[A](it: => Iterator[A]): View[A] = new View[A] {
      def iterator = it
    }
    case object Empty extends View[Nothing] {
      def iterator = Iterator.empty
      override def knownLength = 0
    }
    case class Elems[A](xs: A*) extends View[A] {
      def iterator = Iterator(xs*)
      override def knownLength = xs.length
    }
    case class Filter[A](val underlying: Iterable[A], p: A => Boolean) extends View[A] {
      def iterator = underlying.iterator.filter(p)
    }
    case class Partition[A](val underlying: Iterable[A], p: A => Boolean) {
      val left, right = Partitioned(this)
      // `distribute` makes up for the lack of generic push-based functionality.
      // It forces both halves of the partition with a given builder.
      def distribute(bf: => Builder[A, Iterable[A]]) = {
        val lb, rb = bf
        val it = underlying.iterator
        while (it.hasNext) {
          val x = it.next()
          (if (p(x)) lb else rb) += x
        }
        left.forced = Some(lb.result)
        right.forced = Some(rb.result)
      }
    }
    case class Partitioned[A](partition: Partition[A]) extends View[A] {
      private var myForced: Option[Iterable[A]] = None
      def forced: Option[Iterable[A]] = myForced
      private[View] def forced_=(x: Option[Iterable[A]]): Unit = myForced = x
      def underlying = partition.underlying
      def iterator = forced match {
        case Some(c) => c.iterator
        case None =>
          underlying.iterator.filter(
            if (this eq partition.left) partition.p else !partition.p(_))
      }
    }
    case class Drop[A](underlying: Iterable[A], n: Int) extends View[A] {
      def iterator = underlying.iterator.drop(n)
      override def knownLength =
        if (underlying.knownLength >= 0) underlying.knownLength - n max 0 else -1
    }
    case class Map[A, B](underlying: Iterable[A], f: A => B) extends View[B] {
      def iterator = underlying.iterator.map(f)
      override def knownLength = underlying.knownLength
    }
    case class FlatMap[A, B](underlying: Iterable[A], f: A => IterableOnce[B]) extends View[B] {
      def iterator = underlying.iterator.flatMap(f)
    }
    case class Concat[A](underlying: Iterable[A], other: IterableOnce[A]) extends View[A] {
      def iterator = underlying.iterator ++ other
      override def knownLength = other match {
        case other: Iterable[_] if underlying.knownLength >= 0 && other.knownLength >= 0 =>
          underlying.knownLength + other.knownLength
        case _ =>
          -1
      }
    }
    case class Zip[A, B](underlying: Iterable[A], other: IterableOnce[B]) extends View[(A, B)] {
      def iterator = underlying.iterator.zip(other)
      override def knownLength = other match {
        case other: Iterable[_] if underlying.knownLength >= 0 && other.knownLength >= 0 =>
          underlying.knownLength min other.knownLength
        case _ =>
          -1
      }
    }
    case class Reverse[A](underlying: Iterable[A]) extends View[A] {
      def iterator = {
        var xs: List[A] = Nil
        val it = underlying.iterator
        while (it.hasNext) xs = Cons(it.next(), xs)
        xs.iterator
      }
      override def knownLength = underlying.knownLength
    }
  }

/* ---------- Iterators ---------------------------------------------------*/

  /** A core Iterator class */
  trait Iterator[+A] extends IterableOnce[A] { self =>
    def hasNext: Boolean
    def next(): A
    def iterator = this
    def foldLeft[B](z: B)(op: (B, A) => B): B =
      if (hasNext) foldLeft(op(z, next()))(op) else z
    def foldRight[B](z: B)(op: (A, B) => B): B =
      if (hasNext) op(next(), foldRight(z)(op)) else z
    def foreach(f: A => Unit): Unit =
      while (hasNext) f(next())
    def indexWhere(p: A => Boolean): Int = {
      var i = 0
      while (hasNext) {
        if (p(next())) return i
        i += 1
      }
      -1
    }
    def filter(p: A => Boolean): Iterator[A] = new Iterator[A] {
      private var hd: A = compiletime.uninitialized
      private var hdDefined: Boolean = false

      def hasNext: Boolean = hdDefined || {
        while {
          if (!self.hasNext) return false
          hd = self.next()
          !p(hd)
        } do ()
        hdDefined = true
        true
      }

      def next() =
        if (hasNext) {
          hdDefined = false
          hd
        }
        else Iterator.empty.next()
    }

    def map[B](f: A => B): Iterator[B] = new Iterator[B] {
      def hasNext = self.hasNext
      def next() = f(self.next())
    }

    def flatMap[B](f: A => IterableOnce[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = Iterator.empty
      private def current = {
        while (!myCurrent.hasNext && self.hasNext)
          myCurrent = f(self.next()).iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next() = current.next()
    }
    def ++[B >: A](xs: IterableOnce[B]): Iterator[B] = new Iterator[B] {
      private var myCurrent: Iterator[B] = self
      private var first = true
      private def current = {
        if (!myCurrent.hasNext && first) {
          myCurrent = xs.iterator
          first = false
        }
        myCurrent
      }
      def hasNext = current.hasNext
      def next() = current.next()
    }
    def drop(n: Int): Iterator[A] = {
      var i = 0
      while (i < n && hasNext) {
        next()
        i += 1
      }
      this
    }
    def zip[B](that: IterableOnce[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
      val thatIterator = that.iterator
      def hasNext = self.hasNext && thatIterator.hasNext
      def next() = (self.next(), thatIterator.next())
    }
  }

  object Iterator {
    val empty: Iterator[Nothing] = new Iterator[Nothing] {
      def hasNext = false
      def next() = throw new NoSuchElementException("next on empty iterator")
    }
    def apply[A](xs: A*): Iterator[A] = new RandomAccessView[A] {
      val start = 0
      val end = xs.length
      def apply(n: Int) = xs(n)
    }.iterator
  }
}

