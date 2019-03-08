package lst

import collection.mutable.{ListBuffer, StringBuilder}
import collection.immutable.Map
import reflect.ClassTag

/** A lightweight class for lists, optimized for short and medium lengths.
 *  A list is represented at runtime as
 *
 *    If it is empty:                    the value `Lst.Empty`
 *    If it contains one element,
 *    and the element is not an array:   the element itself
 *    Otherwise:                         an Array[Any] containing the elements
 */
class Lst[+T](val elems: Any) extends AnyVal { self =>
  import Lst._

  inline def locally[T](body: => T): T = body

  def length: Int = elems match {
    case null => 0
    case elems: Arr => elems.length
    case elem => 1
  }

  def isEmpty = elems == null
  def nonEmpty = elems != null

  inline def foreach(op: => T => Unit): Unit = {
    def sharedOp(x: T) = op(x)
    elems match {
      case null =>
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = 0
        while (i < elems.length) { sharedOp(elem(i)); i += 1 }
      case elem: T @ unchecked => sharedOp(elem)
    }
  }

  inline def foreachReversed(inline op: T => Unit): Unit = {
    def sharedOp(x: T) = op(x)
    elems match {
      case null =>
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = elems.length
        while (i > 0) { i -= 1; sharedOp(elem(i)) }
      case elem: T @ unchecked => sharedOp(elem)
    }
  }

  /** Like `foreach`, but completely inlines `op`, at the price of generating the code twice.
   *  Should be used only of `op` is small
   */
  inline def foreachInlined(op: => T => Unit): Unit = {
    elems match {
      case null =>
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = 0
        while (i < elems.length) { op(elem(i)); i += 1 }
      case elem: T @unchecked => op(elem)
    }
  }

  def iterator(): Iterator[T] = elems match {
    case null => Iterator.empty
    case elems: Arr => elems.iterator.asInstanceOf[Iterator[T]]
    case elem: T @unchecked => Iterator.single(elem)
  }

  def copyToArray[U >: T](target: Array[U], from: Int) = elems match {
    case null =>
    case elems: Arr => System.arraycopy(elems, 0, target, from, elems.length)
    case elem: T @ unchecked => target(from) = elem
  }

  def toArray[U >: T : ClassTag]: Array[U] = {
    val result = new Array[U](length)
    copyToArray(result, 0)
    result
  }

  def toSet[U >: T]: collection.immutable.Set[U] = {
    var xs = Set[U]()
    foreach(xs += _)
    xs
  }

  def toList: List[T] = {
    val buf = new collection.mutable.ListBuffer[T]
    foreach(buf += _)
    buf.toList
  }

  def toListReversed: List[T] = {
    var result: List[T] = Nil
    foreach(x => result = x :: result)
    result
  }

  def toIterable: Iterable[T] = new Iterable[T] {
    def iterator = self.iterator()
  }

  /** `f` is pulled out, not duplicated */
  inline def map[U](f: => T => U): Lst[U] = {
    def op(x: T) = f(x)
    elems match {
      case null => Empty
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        val newElems = new Arr(elems.length)
        var i = 0
        while (i < elems.length) { newElems(i) = op(elem(i)); i += 1 }
        multi[U](newElems)
      case elem: T @ unchecked => single[U](op(elem))
    }
  }

  def mapConserve[U](f: T => U): Lst[U] = elems match {
    case null => Empty
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      var newElems: Arr = null
      var i = 0
      while (i < elems.length) {
        val x = elem(i)
        val y = f(x)
        if (newElems != null) newElems(i) = y
        else if (!eq(x, y)) {
          newElems = new Arr(elems.length)
          System.arraycopy(elems, 0, newElems, 0, i)
          newElems(i) = y
        }
        i += 1
      }
      if (newElems == null) this.asInstanceOf[Lst[U]] else multi[U](newElems)
    case elem: T @ unchecked => single[U](f(elem))
  }

  def flatMapIterable[U](f: T => Iterable[U]): Lst[U] = flatMap(x => fromIterable(f(x)))

  def flatMap[U](f: T => Lst[U]): Lst[U] = elems match {
    case null => Empty
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      val newElemss: Arr = new Arr(elems.length)
      var i = 0
      var len = 0
      while (i < elems.length) {
        val ys = f(elem(i))
        len += ys.length
        newElemss(i) = ys.elems
        i += 1
      }
      if (len == 0) Empty
      else if (len == 1) {
        i = 0
        while (newElemss(i) == null) i += 1
        new Lst[U](newElemss(i))
      }
      else {
        val newElems = new Arr(len)
        i = 0
        var j = 0
        while (i < newElemss.length) {
          val ys = new Lst[U](newElemss(i))
          ys.copyToArray(newElems, j)
          j += ys.length
          i += 1
        }
        multi[U](newElems)
      }
    case elem: T @ unchecked => f(elem)
  }

  def filter(p: T => Boolean): Lst[T] = elems match {
    case null => Empty
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      val scratch = new Arr(elems.length)
      var i = 0
      var len = 0
      while (i < elems.length) {
        val x = elem(i)
        if (p(x)) { scratch(len) = x; len += 1 }
        i += 1
      }
      if (len == elems.length) this
      else _fromArray(scratch, 0, len)
    case elem: T @unchecked =>
      if (p(elem)) this else Empty
  }
  def filterNot(p: T => Boolean): Lst[T] = filter(!p(_))

  def collect[U](pf: PartialFunction[T, U]): Lst[U] = {
    val buf = new Lst.Buffer[U]
    foreach(x => if (pf.isDefinedAt(x)) buf += pf(x))
    buf.toLst
  }

  inline def exists(p: => T => Boolean): Boolean = {
    def op(x: T) = p(x)
    elems match {
      case null => false
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = 0
        while (i < elems.length && !op(elem(i))) i += 1
        i < elems.length
      case elem: T @unchecked =>
        op(elem)
    }
  }

  inline def forall(p: => T => Boolean): Boolean = {
    def op(x: T) = p(x)
    elems match {
      case null => true
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = 0
        while (i < elems.length && op(elem(i))) i += 1
        i == elems.length
      case elem: T @unchecked =>
        op(elem)
    }
  }

  def contains[U >: T](x: U): Boolean = elems match {
    case null => false
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      var i = 0
      while (i < elems.length && elem(i) != x) i += 1
      i < elems.length
    case elem: T @unchecked =>
      elem == x
  }

  inline def foldLeft[U](z: U)(f: => (U, T) => U) = {
    def op(x: U, y: T) = f(x, y)
    elems match {
      case null => z
      case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
        var i = 0
        var acc = z
        while (i < elems.length) { acc = op(acc, elem(i)); i += 1 }
        acc
      case elem: T @unchecked =>
        op(z, elem)
    }
  }

  inline def /: [U](z: U)(op: => (U, T) => U) = foldLeft(z)(op)

  def reduceLeft[U >: T](op: (U, U) => U) = elems match {
    case null =>
      throw new UnsupportedOperationException("Lst.Empty.reduceLeft")
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      var i = 1
      var acc: U = elem(0)
      while (i < elems.length) { acc = op(acc, elem(i)); i += 1 }
      acc
    case elem: T @unchecked =>
      elem
  }

  def reverse: Lst[T] = elems match {
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      val newElems = new Arr(elems.length)
      var i = 0
      while (i < elems.length) {
        newElems(elems.length - 1 - i) = elem(i)
        i += 1
      }
      multi[T](newElems)
    case _ => this
  }

  def apply(n: Int): T = elems match {
    case null => throw new IndexOutOfBoundsException(n.toString)
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      elem(n)
    case elem: T @unchecked =>
      if (n == 0) elem else throw new IndexOutOfBoundsException(n.toString)
  }

  def head: T = apply(0)
  def last: T = apply(length - 1)

  def headOr[U >: T](alt: => U): U = if (isEmpty) alt else head

  def slice(start: Int, end: Int): Lst[T] =
    if (start < 0) slice(0, end)
    else elems match {
      case null => this
      case elems: Arr => _fromArray(elems, start, end `min` elems.length)
      case elem: T @ unchecked => if (start == 0 && end > 0) this else Empty
    }

  def drop(n: Int): Lst[T] = slice(n, length)
  def tail = drop(1)
  def take(n: Int): Lst[T] = slice(0, n)

  def dropRight(n: Int): Lst[T] = take(length - n)
  def takeRight(n: Int): Lst[T] = drop(length - n)
  def init = dropRight(1)

  def firstIndexOf[U >: T](x: U, from: Int = 0): Int = elems match {
    case null => 0
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      var i = from
      while (i < length && elem(i) != x) i += 1
      i
    case elem: T @ unchecked => if (from == 0 && elem == x) 0 else 1
  }

  def firstIndexWhere(p: T => Boolean, from: Int = 0): Int = elems match {
    case null => 0
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      var i = from
      while (i < length && !p(elem(i))) i += 1
      i
    case elem: T @ unchecked => if (from == 0 && p(elem)) 0 else 1
  }

  def find(p: T => Boolean): Option[T] = {
    val idx = firstIndexWhere(p)
    if (idx < length) Some(apply(idx)) else None
  }

  def takeWhile(p: T => Boolean): Lst[T] = take(firstIndexWhere(!p(_)))
  def dropWhile(p: T => Boolean): Lst[T] = drop(firstIndexWhere(!p(_)))

  def ++ [U >: T](that: Lst[U]): Lst[U] =
    if (elems == null) that
    else if (that.elems == null) this
    else {
      val len1 = this.length
      val len2 = that.length
      val newElems = new Arr(len1 + len2)
      this.copyToArray(newElems, 0)
      that.copyToArray(newElems, len1)
      multi[U](newElems)
    }

  def :: [U >: T](x: U): Lst[U] = elems match {
    case null => Lst(x)
    case elems: Arr =>
      val newElems = new Arr(1 + elems.length)
      newElems(0) = x
      copyToArray(newElems, 1)
      multi[U](newElems)
    case elem: T @unchecked =>
      Lst(x, elem)
  }

  def :+ [U >: T](x: U): Lst[U] = elems match {
    case null => Lst(x)
    case elems: Arr =>
      val newElems = new Arr(elems.length + 1)
      copyToArray(newElems, 0)
      newElems(elems.length) = x
      multi[U](newElems)
    case elem: T @unchecked =>
      Lst(elem, x)
  }

  def union [U >: T](that: Lst[U]): Lst[U] = this ++ that.filterNot(this.contains)
  def intersect [U >: T](that: Lst[U]): Lst[U] = this.filter(that.contains)

  def zipWith[U, V](that: Lst[U])(op: (T, U) => V): Lst[V] =
    this.elems match {
      case null => Empty
      case elems1: Arr => def elem1(i: Int) = elems1(i).asInstanceOf[T]
        that.elems match {
          case null => Empty
          case elems2: Arr => def elem2(i: Int) = elems2(i).asInstanceOf[U]
            val len = elems1.length min elems2.length
            if (len == 0) Empty
            else if (len == 1) single[V](op(elem1(0), elem2(0)))
            else {
              var newElems: Arr = null
              var i = 0
              while (i < len) {
                val x = elem1(i)
                val y = op(x, elem2(i))
                if (newElems != null) newElems(i) = y
                else if (!eq(x, y)) {
                  newElems = new Arr(len)
                  System.arraycopy(elems, 0, newElems, 0, i)
                  newElems(i) = y
                }
                i += 1
              }
              multi[V](newElems)
            }
          case elem2: U @unchecked =>
            single[V](op(elem1(0), elem2))
        }
      case elem1: T @unchecked =>
        that.elems match {
          case null => Empty
          case elems2: Arr => def elem2(i: Int) = elems2(i).asInstanceOf[U]
            single[V](op(elem1, elem2(0)))
          case elem2: U @unchecked => single[V](op(elem1, elem2))
        }
    }

  def zip[U](that: Lst[U]): Lst[(T, U)] = zipWith(that)((_, _))

  def zipWithIndex: Lst[(T, Int)] = elems match {
    case null => Empty
    case elems: Arr => def elem(i: Int) = elems(i).asInstanceOf[T]
      val newElems = new Arr(elems.length)
      var i = 0
      while (i < elems.length) { newElems(i) = (elem(i), i); i += 1 }
      multi[(T, Int)](newElems)
    case elem: T @unchecked =>
      single[(T, Int)]((elem, 0))
  }

  def indices: Lst[Int] = elems match {
    case null => Empty
    case elems: Arr =>
      val newElems: Arr = new Arr(elems.length)
      var i = 0
      while (i < elems.length) { newElems(i) = i; i += 1 }
      multi[Int](newElems)
    case elem =>
      single[Int](0)
  }

  def corresponds[U](that: Lst[U])(p: (T, U) => Boolean): Boolean =
    (this `eqLst` that) || {
      this.elems match {
        case null =>
          that.elems == null
        case elems1: Arr => def elem1(i: Int) = elems1(i).asInstanceOf[T]
          that.elems match {
            case elems2: Arr => def elem2(i: Int) = elems2(i).asInstanceOf[U]
              val len = elems1.length
              len == elems2.length && {
                var i = 0
                while (i < len && p(elem1(i), elem2(i))) i += 1
                i == len
              }
            case _ => false
          }
        case elem1: T @unchecked =>
          that.elems match {
            case null => false
            case elems2: Arr => false
            case elem2: U @unchecked => p(elem1, elem2)
          }
      }
    }

  override def equals(that: Any) = that match {
    case that: Lst[_] =>
      (this `eqLst` that) || {
        elems match {
          case elems1: Arr =>
            that.elems match {
              case elems2: Arr =>
                val len = elems1.length
                len == elems2.length && {
                  var i = 0
                  while (i < len && elems1(i).equals(elems2(i))) i += 1
                  i == len
                }
              case _ => false
            }
          case elem => elem == that.elems
        }
      }
    case _ =>
      false
  }

  def eqLst[U](that: Lst[U]) = eq(elems, that.elems)

  def eqElements[U](that: Lst[U]): Boolean = corresponds(that)(eqFn)

  def mkString: String = mkString(", ")

  def mkString(sep: String): String = mkString("", sep, "")
  def mkString(left: String, sep: String, right: String): String = {
    val b = new StringBuilder(left)
    var first = true
    foreachInlined { elem =>
      if (first) first = false else b ++= sep
      b ++= elem.toString
    }
    b ++= right
    b.toString
  }

  override def toString = mkString("Lst(", ", ", ")")
}

object Lst {

  private type Arr = Array[Any]

  private def eq(x: Any, y: Any) = x.asInstanceOf[AnyRef] `eq` y.asInstanceOf[AnyRef]
  private val eqFn = (x: Any, y: Any) => eq(x, y)

  val Empty = new Lst[Nothing](null)

  def apply[T](): Lst[T] = Empty

  def apply[T](x0: T): Lst[T] = single[T](x0)

  def apply[T](x0: T, x1: T): Lst[T] = {
    val elems = new Arr(2)
    elems(0) = x0
    elems(1) = x1
    new Lst[T](elems)
  }

  def apply[T](x0: T, x1: T, x2: T): Lst[T] = {
    val elems = new Arr(3)
    elems(0) = x0
    elems(1) = x1
    elems(2) = x2
    new Lst[T](elems)
  }

  def apply[T](x0: T, x1: T, x2: T, x3: T): Lst[T] = {
    val elems = new Arr(4)
    elems(0) = x0
    elems(1) = x1
    elems(2) = x2
    elems(3) = x3
    new Lst[T](elems)
  }

  def apply[T](x0: T, x1: T, x2: T, x3: T, x4: T, xs: T*): Lst[T] = {
    val elems = new Arr(5 + xs.length)
    elems(0) = x0
    elems(1) = x1
    elems(2) = x2
    elems(3) = x3
    elems(4) = x4
    xs.copyToArray(elems, 5)
    new Lst[T](elems)
  }

  def fill[T](n: Int)(elem: => T) = {
    val elems = new Arr(n)
    var i = 0
    while (i < n) { elems(i) = elem; i += 1}
    new Lst[T](elems)
  }

  def tabulate[T](n: Int)(elem: Int => T) = {
    val elems = new Arr(n)
    var i = 0
    while (i < n) { elems(i) = elem(i); i += 1}
    new Lst[T](elems)
  }

  def range(from: Int, until: Int): Lst[Int] = {
    val elems = new Arr(until - from)
    var i = from
    while (i < until) { elems(i) = i; i += 1}
    new Lst[Int](elems)
  }

  class Buffer[T] {
    private var len = 0
    private var elem: Any = _
    private var elems: Arr = _

    def size = len

    /** pre: len > 0, n >= 1 */
    private def ensureSize(n: Int) =
      if (len == 1) {
        elems = new Arr(n `max` 16)
        elems(0) = elem
      }
      else if (len < n) {
        val newLen = n `max` len << 2
        val newElems = new Arr(newLen)
        System.arraycopy(elems, 0, newElems, 0, len)
        elems = newElems
      }

    def isEmpty = size == 0

    def += (x: T): this.type = {
      if (len == 0) elem = x
      else {
        ensureSize(len + 1)
        elems(len) = x
      }
      len += 1
      this
    }

    def ++= (xs: Lst[T]): this.type = {
      xs.elems match {
        case null => this
        case elems2: Arr =>
          if (len == 0 && elems2.length != 1)
            // if elems2.length == 1, elems2 is a wrapped single element list, which has to be unpacked
            elems = elems2
          else {
            ensureSize(len + elems2.length)
            System.arraycopy(elems2, 0, elems, len, elems2.length)
          }
          len += elems2.length
        case elem: T @unchecked => this += elem
      }
      this
    }

    def exists(p: T => Boolean): Boolean =
      if (len == 0) false
      else if (len == 1) p(elem.asInstanceOf[T])
      else {
        var i = 0
        while (i < len && !p(elems(i).asInstanceOf[T])) i += 1
        i < len
      }
    def forall(p: T => Boolean): Boolean = !exists(!p(_))

    def contains(x: T): Boolean =
      if (len == 0) false
      else if (len == 1) elem == x
      else {
        var i = 0
        while (i < len && elems(i) != x) i += 1
        i < len
      }

    def toLst: Lst[T] =
      if (len == 0) Empty
      else if (len == 1) single(elem)
      else _fromArray(elems, 0, len)

    def clear() =
      len = 0
  }

  private def single[T](elem: Any): Lst[T] = elem match {
    case null | _: Arr =>
      val wrapped = new Arr(1)
      wrapped(0) = elem
      new Lst[T](wrapped)
    case _ =>
      new Lst[T](elem)
  }

  private def multi[T](elems: Array[Any]) =
    if (elems.length == 1) single(elems(0))
    else new Lst[T](elems)

  private def _fromArray[T](elems: Arr, start: Int, end: Int): Lst[T] = {
    val len = end - start
    if (len <= 0) Empty
    else if (len == 1) single[T](elems(start))
    else if (start == 0 && end == elems.length) new Lst[T](elems)
    else {
      val newElems = new Arr(len)
      System.arraycopy(elems, start, newElems, 0, len)
      new Lst[T](newElems)
    }
  }

  def fromArray[T](elems: Array[T], start: Int, end: Int): Lst[T] =
    _fromArray(elems.asInstanceOf[Arr], start, end)

  def fromIterator[T](it: Iterator[T]): Lst[T] = {
    val buf = new Buffer[T]
    it.foreach(buf += _)
    buf.toLst
  }

  def fromIterable[T](xs: Iterable[T]): Lst[T] = fromIterator(xs.iterator)

  object :: {
    def unapply[T](xs: Lst[T]): Option[(T, Lst[T])] = xs.elems match {
      case null => None
      case elems: Arr =>
        Some((elems(0).asInstanceOf[T], _fromArray[T](elems, 1, elems.length)))
      case elem: T @unchecked =>
        Some((elem, Empty))
    }
  }
}
