package colltest5
package strawman.collections

import Predef.{augmentString as _, wrapString as _, *}
import scala.reflect.ClassTag
import annotation.unchecked.{uncheckedVariance, uncheckedCaptures}
import annotation.tailrec
import caps.any
import caps.unsafe.{unsafeAssumeSeparate, untrackedCaptures}

import language.experimental.captureChecking

/** A strawman architecture for new collections. It contains some
 *  example collection classes and methods with the intent to expose
 *  some key issues. It would be good to compare this to other
 *  implementations of the same functionality, to get an idea of the
 *  strengths and weaknesses of different collection architectures.
 *
 *  For a test file, see tests/run/CollectionTests.scala.
 */
object CollectionStrawMan5 {

  /* ------------ Base Traits -------------------------------- */

  type AnyIterableOnce[A] = IterableOnce[A]^

  /** Iterator can be used only once */
  trait IterableOnce[+A] {
    this: AnyIterableOnce[A] =>
    def iterator: Iterator[A]^{this}
  }

  /** Base trait for instances that can construct a collection from an iterable */
  trait FromIterable {
    type C[X] <: Iterable[X]^
    def fromIterable[B](it: Iterable[B]^{this, any}): C[B]^{it}
  }

  type FromIterableOf[+CC[X] <: Iterable[X]^] = FromIterable {
    type C[X] <: CC[X]
  }

  /** Base trait for companion objects of collections */
  trait IterableFactory extends FromIterable {
    def empty[A]: C[A] = fromIterable(View.Empty)
    def apply[A](xs: A*): C[A] = fromIterable(View.Elems(xs*))
  }

  /** Base trait for generic collections */
  trait Iterable[+A] extends IterableOnce[A] with IterableLike[A] {
    this: Iterable[A]^ =>
    type C[X] <: Iterable[X]^
    protected def coll: Iterable[A]^{this} = this
    def knownLength: Int = -1
  }

  /** Base trait for sequence collections */
  trait Seq[+A] extends Iterable[A] with SeqLike[A] {
    this: Seq[A] =>
    type C[X] <: Seq[X]
    def apply(i: Int): A
    def length: Int
  }

  trait SeqFactory extends IterableFactory {
    type C[X] <: Seq[X]
    def fromIterable[B](it: Iterable[B]^{this, any}): C[B]
  }

  /** Base trait for strict collections */
  trait Buildable[+A] extends Iterable[A] {
    protected def newBuilder: Builder[A, Repr] @uncheckedVariance
    override def partition(p: A => Boolean): (Repr, Repr) =
      val l, r = newBuilder
      iterator.foreach(x => (if (p(x)) l else r) += x)
      (l.result, r.result)
    // one might also override other transforms here to avoid generating
    // iterators if it helps efficiency.
  }

  /** Base trait for collection builders */
  trait Builder[-A, +To] {
    def +=(x: A): this.type
    def result: To

    def ++=(xs: IterableOnce[A]^): this.type = {
      xs.iterator.foreach(+=)
      this
    }
  }

  /* ------------ Operations ----------------------------------- */

  /** Base trait for Iterable operations
   *
   *  VarianceNote
   *  ============
   *
   *  We require that for all child classes of Iterable the variance of
   *  the child class and the variance of the `C` parameter passed to `IterableLike`
   *  are the same. We cannot express this since we lack variance polymorphism. That's
   *  why we have to resort at some places to write `C[A @uncheckedVariance]`.
   *
   */
  trait IterableLike[+A]
    extends FromIterable
       with IterableOps[A]
       with IterablePolyTransforms[A]
       with IterableMonoTransforms[A] { // sound bcs of VarianceNote
    type Repr = C[A] @uncheckedVariance
    protected def fromLikeIterable(coll: Iterable[A] @uncheckedVariance ^ {this, any}): Repr @uncheckedVariance ^{coll} =
      fromIterable(coll)
  }

  /** Base trait for Seq operations */
  trait SeqLike[+A]
  extends IterableLike[A], SeqMonoTransforms[A], SeqPolyTransforms[A]: // sound bcs of VarianceNote
    this: SeqLike[A] =>
    type C[X] <: Seq[X]
    def fromIterable[B](coll: Iterable[B]^): C[B]
    override protected def fromLikeIterable(coll: Iterable[A] @uncheckedVariance ^): Repr =
      fromIterable(coll)

  trait IterableOps[+A] extends Any {
    this: IterableOps[A]^ =>
    def iterator: Iterator[A]^{this}
    def foreach(f: A => Unit): Unit = iterator.foreach(f)
    def foldLeft[B](z: B)(op: (B, A) => B): B = iterator.foldLeft(z)(op)
    def foldRight[B](z: B)(op: (A, B) => B): B = iterator.foldRight(z)(op)
    def indexWhere(p: A => Boolean): Int = iterator.indexWhere(p)
    def isEmpty: Boolean = !iterator.hasNext
    def head: A = iterator.next()
    def view: View[A]^{this} = View.fromIterator(iterator)
  }

  trait IterableMonoTransforms[+A] extends Any {
    this: IterableMonoTransforms[A]^ =>
    type Repr
    protected def coll: Iterable[A]^{this}
    protected def fromLikeIterable(coll: Iterable[A] @uncheckedVariance ^ {this, any}): Repr^{coll}
    def filter(p: A => Boolean): Repr^{this, p} = fromLikeIterable(View.Filter(coll, p))

    def partition(p: A => Boolean): (Repr^{this, p}, Repr^{this, p}) = {
      val pn = View.Partition(coll, p)
      (fromLikeIterable(pn.left), fromLikeIterable(pn.right))
    }
    def drop(n: Int): Repr^{this} = fromLikeIterable(View.Drop(coll, n))

    def to[C[X] <: Iterable[X]](fi: FromIterableOf[C]): C[A @uncheckedVariance]^{this} =
      // variance seems sound because `to` could just as well have been added
      // as a decorator. We should investigate this further to be sure.
      fi.fromIterable(coll)
  }

  trait IterablePolyTransforms[+A] extends Any {
    this: IterablePolyTransforms[A]^ =>
    type C[A]
    protected def coll: Iterable[A]^{this}
    def fromIterable[B](coll: Iterable[B]^{this, any}): C[B]^{coll}
    def map[B](f: A => B): C[B]^{this, f} = fromIterable(View.Map(coll, f))
    def flatMap[B](f: A => IterableOnce[B]^): C[B]^{this, f} = fromIterable(View.FlatMap(coll, f))
    def ++[B >: A](xs: IterableOnce[B]^): C[B]^{this, xs} = fromIterable(View.Concat(coll, xs))
    def zip[B](xs: IterableOnce[B]^): C[(A @uncheckedVariance, B)]^{this, xs} = fromIterable(View.Zip(coll, xs))
       // sound bcs of VarianceNote
  }

  trait SeqMonoTransforms[+A] extends Any, IterableMonoTransforms[A] {
    this: SeqMonoTransforms[A] =>
    def reverse: Repr =
      var xs: List[A] = Nil
      var it = coll.iterator
      while (it.hasNext) xs = new Cons(it.next(), xs)
      fromLikeIterable(xs)

    override protected def fromLikeIterable(coll: Iterable[A] @uncheckedVariance ^): Repr

    override def filter(p: A => Boolean): Repr = fromLikeIterable(View.Filter(coll, p))

    override def partition(p: A => Boolean): (Repr, Repr) = {
      val pn = View.Partition(coll, p)
      (fromLikeIterable(pn.left), fromLikeIterable(pn.right))
    }
    override def drop(n: Int): Repr = fromLikeIterable(View.Drop(coll, n))

    override def to[C[X] <: Iterable[X]](fi: FromIterableOf[C]): C[A @uncheckedVariance] =
      // variance seems sound because `to` could just as well have been added
      // as a decorator. We should investigate this further to be sure.
      fi.fromIterable(coll)
  }

  trait SeqPolyTransforms[+A] extends Any, IterablePolyTransforms[A]:
    this: SeqPolyTransforms[A] =>
    type C[A]
    override def fromIterable[B](coll: Iterable[B]^): C[B]
    override def map[B](f: A => B): C[B] = fromIterable(View.Map(coll, f))
    override def flatMap[B](f: A => IterableOnce[B]^): C[B] = fromIterable(View.FlatMap(coll, f))
    override def ++[B >: A](xs: IterableOnce[B]^): C[B] = fromIterable(View.Concat(coll, xs))
    override def zip[B](xs: IterableOnce[B]^): C[(A @uncheckedVariance, B)] = fromIterable(View.Zip(coll, xs))


  /* --------- Concrete collection types ------------------------------- */

  /** Concrete collection type: List */
  sealed trait List[+A] extends Seq[A] with SeqLike[A] with Buildable[A] { self =>
    type C[X] = List[X]
    def isEmpty: Boolean
    def head: A
    def tail: List[A]
    def iterator = new Iterator[A] {
      @untrackedCaptures private var current = self
      def hasNext = !current.isEmpty
      def next() = { val r = current.head; current = current.tail; r }
    }
    def fromIterable[B](c: Iterable[B]^): List[B] = List.fromIterable(c)
    def apply(i: Int): A = {
      require(!isEmpty)
      if (i == 0) head else tail.apply(i - 1)
    }
    def length: Int =
      if (isEmpty) 0 else 1 + tail.length
    protected def newBuilder = new ListBuffer[A @uncheckedVariance @uncheckedCaptures]
    def ++:[B >: A](prefix: List[B]): List[B] =
      if (prefix.isEmpty) this
      else Cons(prefix.head, prefix.tail ++: this)
    override def ++[B >: A](xs: IterableOnce[B]^): List[B] = xs match {
      case xs: List[B] => this ++: xs
      case _ => fromIterable(View.Concat(this, xs))
    }
    @tailrec final override def drop(n: Int) =
      if (n > 0) tail.drop(n - 1) else this
  }

  case class Cons[+A](x: A, @untrackedCaptures private[collections] var next: List[A @uncheckedVariance @uncheckedCaptures]) // sound because `next` is used only locally
  extends List[A] {
    override def isEmpty = false
    override def head = x
    def tail = next
  }

  case object Nil extends List[Nothing] {
    override def isEmpty = true
    override def head = ???
    def tail = ???
  }

  object List extends SeqFactory {
    type C[X] = List[X]
    def fromIterable[B](coll: Iterable[B]^): List[B] = coll match {
      case coll: List[B] => coll
      case _ => ListBuffer.fromIterable[B @uncheckedCaptures](coll).result
    }
  }

  /** Concrete collection type: ListBuffer */
  class ListBuffer[A] extends Seq[A] with SeqLike[A] with Builder[A, List[A]] {
    type C[X] = ListBuffer[X]
    @untrackedCaptures private var first, last: List[A] = Nil
    @untrackedCaptures private var aliased = false
    def iterator = first.iterator
    def fromIterable[B](coll: Iterable[B]^): ListBuffer[B] = ListBuffer.fromIterable(coll)
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

  object ListBuffer extends SeqFactory {
    type C[X] = ListBuffer[X]
    def fromIterable[B](coll: Iterable[B]^): ListBuffer[B] = new ListBuffer[B] ++= coll
  }

  /** Concrete collection type: ArrayBuffer */
  class ArrayBuffer[A] private (@untrackedCaptures initElems: Array[AnyRef]^, initLength: Int)
  extends Seq[A] with SeqLike[A] with Builder[A, ArrayBuffer[A]] {
    //this: ArrayBuffer[A] =>
    type C[X] = ArrayBuffer[X]
    def this() = this(new Array[AnyRef](16), 0)
    @untrackedCaptures private var elems: Array[AnyRef]^ = initElems
    @untrackedCaptures private var start = 0
    @untrackedCaptures private var end = initLength
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
    def length = end - start
    override def knownLength = length
    override def view = new ArrayBufferView(elems, start, end)
    def iterator = view.iterator
    def fromIterable[B](it: Iterable[B]^): ArrayBuffer[B] =
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
    override def ++[B >: A](xs: IterableOnce[B]^): ArrayBuffer[B] = xs match {
      case xs: ArrayBuffer[B] @unchecked =>
        val elems = new Array[AnyRef](length + xs.length)
        Array.copy(this.elems, this.start, elems, 0, this.length)
        Array.copy(xs.elems, xs.start, elems, this.length, xs.length)
        new ArrayBuffer(elems, elems.length)
      case _ => fromIterable(View.Concat(this, xs))
    }

    override def toString = s"ArrayBuffer(${elems.slice(start, end).mkString(", ")})"
  }

  object ArrayBuffer extends SeqFactory {
    type C[X] = ArrayBuffer[X]
    def fromIterable[B](coll: Iterable[B]^): ArrayBuffer[B] =
      if (coll.knownLength >= 0) {
        val elems = new Array[AnyRef](coll.knownLength)
        val it = coll.iterator
        for (i <- 0 until elems.length) elems(i) = it.next().asInstanceOf[AnyRef]
        new ArrayBuffer[B](elems, elems.length): ArrayBuffer[B]
      }
      else {
        val buf = new ArrayBuffer[B]
        val it = coll.iterator
        while (it.hasNext) buf += it.next()
        buf: ArrayBuffer[B]
      }
  }

  class ArrayBufferView[A](@untrackedCaptures val elems: Array[AnyRef]^, val start: Int, val end: Int) extends RandomAccessView[A] {
    this: ArrayBufferView[A] =>
    def apply(n: Int) = elems(start + n).asInstanceOf[A]
  }

  /** Concrete collection type: String */
  implicit class StringOps(val s: String)
  extends IterableOps[Char]
     with SeqMonoTransforms[Char]
     with IterablePolyTransforms[Char] {
    this: StringOps =>
    type Repr = String
    type C[X] = List[X]
    protected def coll = new StringView(s)
    def iterator = coll.iterator
    protected def fromLikeIterable(coll: Iterable[Char]^): String = {
      val sb = new StringBuilder
      for (ch <- coll) sb.append(ch)
      sb.toString
    }
    def fromIterable[B](coll: Iterable[B]^): List[B] = List.fromIterable(coll)
    def map(f: Char => Char): String = {
      val sb = new StringBuilder
      for (ch <- s) sb.append(f(ch))
      sb.toString
    }
    def flatMap(f: Char => String): String = {
      val sb = new StringBuilder
      for (ch <- s) sb.append(f(ch))
      sb.toString
    }
    def ++(xs: IterableOnce[Char]^): String = {
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

/* ---------- Views -------------------------------------------------------*/

  /** Concrete collection type: View */
  trait View[+A] extends Iterable[A] with IterableLike[A] {
    this: View[A]^ =>
    type C[X] = View[X]^{this}
    override def view: this.type = this
    override def fromIterable[B](c: Iterable[B]^{this, any}): View[B]^{this, c} = {
      c match {
        case c: View[B] => c
        case _ => View.fromIterator(c.iterator)
      }
    }
  }

  /** View defined in terms of indexing a range */
  trait RandomAccessView[+A] extends View[A] {
    def start: Int
    def end: Int
    def apply(i: Int): A
    def iterator: Iterator[A]^{this} = new Iterator[A] {
      @untrackedCaptures private var current = start
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
    def fromIterator[A](it: => Iterator[A]^): View[A]^{it} = new View[A]:
        def iterator: Iterator[A]^{this} = it

    case object Empty extends View[Nothing] {
      def iterator: Iterator[Nothing] = Iterator.empty
      override def knownLength = 0
    }

    case class Elems[A](xs: A*) extends View[A] {
      def iterator: Iterator[A] = Iterator(xs*)
      override def knownLength = xs.length
    }

    case class Filter[A](val underlying: Iterable[A]^, p: A => Boolean) extends View[A] {
      def iterator: Iterator[A]^{this} = underlying.iterator.filter(p)
    }

    object Filter:
      def apply[A](underlying: Iterable[A]^, pp: A => Boolean, isFlipped: Boolean): Filter[A]^{underlying, pp} =
        underlying match
          case filter: Filter[A] =>
            unsafeAssumeSeparate:
              new Filter(filter.underlying, a => filter.p(a) && pp(a))
                .asInstanceOf[Filter[A]^{underlying, pp}]
              // See filter-iterable.scala for a test where a variant of Filter
              // works without the unsafeAssumeSeparate. But it requires significant
              // changes compared to the version here.
          case _ => new Filter(underlying, pp)

    case class Partition[A] (val underlying: Iterable[A]^, p: A => Boolean) {
      class Partitioned(expected: Boolean) extends View[A]
          uses Partition.this.underlying, Partition.this.p:
        this: Partitioned^{Partition.this} =>
        def iterator: Iterator[A]^{this} =
          underlying.iterator.filter((x: A) => p(x) == expected)

      val left: Partitioned^{this} = Partitioned(true)
      val right: Partitioned^{this} = Partitioned(false)
    }

    case class Drop[A](underlying: Iterable[A]^, n: Int) extends View[A] {
      def iterator: Iterator[A]^{this} = underlying.iterator.drop(n)
      override def knownLength =
        if (underlying.knownLength >= 0) underlying.knownLength - n max 0 else -1
    }

    case class Map[A, B](underlying: Iterable[A]^, f: A => B) extends View[B] {
      def iterator: Iterator[B]^{this} = underlying.iterator.map(f)
      override def knownLength = underlying.knownLength
    }

    case class FlatMap[A, B](underlying: Iterable[A]^, f: A => IterableOnce[B]^) extends View[B] {
      def iterator: Iterator[B]^{this} = underlying.iterator.flatMap(f)
    }

    case class Concat[A](underlying: Iterable[A]^, other: IterableOnce[A]^) extends View[A] {
      def iterator: Iterator[A]^{this} = underlying.iterator ++ other
      override def knownLength = other match {
        case other: Iterable[_] if underlying.knownLength >= 0 && other.knownLength >= 0 =>
          underlying.knownLength + other.knownLength
        case _ =>
          -1
      }
    }

    case class Zip[A, B](underlying: Iterable[A]^, other: IterableOnce[B]^) extends View[(A, B)] {
      def iterator: Iterator[(A, B)]^{this} = underlying.iterator.zip(other)
      override def knownLength = other match {
        case other: Iterable[_] if underlying.knownLength >= 0 && other.knownLength >= 0 =>
          underlying.knownLength min other.knownLength
        case _ =>
          -1
      }
    }
  }

/* ---------- Iterators ---------------------------------------------------*/

  /** A core Iterator class */
  trait Iterator[+A] extends IterableOnce[A] { self: Iterator[A]^ =>
    def hasNext: Boolean
    def next(): A
    def iterator: this.type = this
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
    def filter(p: A ->{any, this} Boolean): Iterator[A]^{this, p} = new Iterator[A] {
      @untrackedCaptures private var hd: A = compiletime.uninitialized
      @untrackedCaptures private var hdDefined: Boolean = false

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

    def map[B](f: A => B): Iterator[B]^{this, f} = new Iterator[B] {
      def hasNext = self.hasNext
      def next() = f(self.next())
    }

    def flatMap[B](f: A => IterableOnce[B]^): Iterator[B]^{this, f} = new Iterator[B] {
      @untrackedCaptures private var myCurrent: Iterator[B]^{this, f} = Iterator.empty
      private def current = {
        while (!myCurrent.hasNext && self.hasNext)
          myCurrent = f(self.next()).iterator
        myCurrent
      }
      def hasNext = current.hasNext
      def next() = current.next()
    }
    def ++[B >: A](xs: IterableOnce[B]^): Iterator[B]^{this, xs} = new Iterator[B] {
      @untrackedCaptures private var myCurrent: Iterator[B]^{self, xs} = self
      @untrackedCaptures private var first = true
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
    def drop(n: Int): Iterator[A]^{this} = {
      var i = 0
      while (i < n && hasNext) {
        next()
        i += 1
      }
      this
    }
    def zip[B](that: IterableOnce[B]^): Iterator[(A, B)]^{this, that} = new Iterator[(A, B)] {
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
