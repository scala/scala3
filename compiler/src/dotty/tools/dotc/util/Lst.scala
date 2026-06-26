package dotty.tools
package dotc.util
import java.util.Arrays
import java.lang.System.arraycopy
import collection.mutable.ListBuffer
import reflect.ClassTag
import scala.collection.immutable

class Lst[+T](private val arr: Array[Object]) extends AnyVal {

  def elems: Array[Object] = arr

  private def at(i: Int): T = arr(i).asInstanceOf[T]
  def length: Int = arr.length
  def size: Int = arr.length
  def isEmpty: Boolean = arr.length == 0
  def nonEmpty: Boolean = arr.length != 0

  def apply(i: Int): T = at(i)

  def head: T = at(0)
  def last: T = at(length - 1)

  def headOption: Option[T] =
    if length > 0 then Some(head) else None

  def lastOption: Option[T] =
    if length > 0 then Some(last) else None

  def map[U](f: T => U): Lst[U] =
    val ys = new Array[Object](arr.length)
    var i = 0
    while i < arr.length do
      ys(i) = f(at(i)).asInstanceOf[Object]
      i += 1
    new Lst(ys)

  def mapConserve[U >: T](f: T => U): Lst[U] =
    val ys = new Array[Object](arr.length)
    var i = 0
    var change = false
    while i < length do
      ys(i) = f(at(i)).asInstanceOf[Object]
      if ys(i) `ne` arr(i) then change = true
      i += 1
    if change then new Lst(ys) else this

  def reverse: Lst[T] =
    val buf = Lst.Buffer[T](length)
    var i = length
    while i > 0 do
      i -= 1
      buf += at(i)
    buf.toLst

  def _eq_ [U >: T](ys: Lst[U]) = arr eq ys.arr
  def _ne_ [U >: T](ys: Lst[U]) = arr ne ys.arr

  def zip[U](ys: Lst[U]): Lst[(T, U)] =
    val zs = new Array[Object](length min ys.length)
    var i = 0
    while i < zs.length do
      zs(i) = (at(i), ys.at(i))
      i += 1
    new Lst(zs)

  def zipWithIndex: Lst[(T, Int)] =
    val zs = new Array[Object](length)
    var i = 0
    while i < length do
      zs(i) = (at(i), i)
      i += 1
    new Lst(zs)

  def zipWith[U, V](ys: Lst[U])(f: (T, U) => V): Lst[V] =
    val zs = new Array[Object](length min ys.length)
    var i = 0
    while i < zs.length do
      zs(i) = f(at(i), ys(i)).asInstanceOf[Object]
      i += 1
    new Lst(zs)

  /** Like `xs.zip(ys).map(f)`, but returns list `xs` itself
   *  - instead of a copy - if function `f` maps all elements of
   *  `xs` to themselves. Also, it is required that `ys` is at least
   *  as long as `xs`.
   */
  def zipWithConserve[V >: T, U](ys: Lst[U])(f: (T, U) => V): Lst[V] =
    val zs = new Array[Object](length min ys.length)
    var i = 0
    var change = false
    while i < zs.length do
      zs(i) = f(at(i), ys(i)).asInstanceOf[Object]
      if arr(i) `ne` zs(i) then change = true
      i += 1
    if change then new Lst(zs) else this

  def foldLeft[U](z: U)(f: (U, T) => U): U =
    var acc = z
    var i = 0
    while i < arr.length do
      acc = f(acc, at(i))
      i += 1
    acc

  def foldRight[U](z: U)(f: (T, U) => U): U =
    def recur(start: Int): U =
      if start < length then f(at(start), recur(start + 1)) else z
    recur(0)

  def flatMap[U](f: T => Lst[U]): Lst[U] =
    map[Lst[U]](f).flatten

  def filter(p: T => Boolean): Lst[T] =
    val buf = Lst.Buffer[T](length)
    var i = 0
    while i < length do
      val x = at(i)
      if p(x) then buf += x
      i += 1
    if buf.length == length then this else buf.toLst

  def collect[U](f: PartialFunction[T, U]): Lst[U] =
    val buf = Lst.Buffer[U](length)
    var i = 0
    while i < length do
      if f.isDefinedAt(at(i)) then buf += f(at(i))
      i += 1
    buf.toLst

  def foreach(f: T => Unit): Unit =
    var i = 0
    while i < length do
      f(at(i))
      i += 1

  def exists(p: T => Boolean): Boolean =
    var i = 0
    while i < length && !p(at(i)) do i += 1
    i < length

  def forall(p: T => Boolean): Boolean = !exists(!p(_))

  def contains[U >: T](elem: U): Boolean =
    var i = 0
    while i < length && elem != at(i) do i += 1
    i < length

  def count(p: T => Boolean): Int =
    var c = 0
    var i = 0
    while i < length do
      if p(at(i)) then c += 1
      i += 1
    c

  def find(p: T => Boolean): Option[T] =
    var i = 0
    while i < length do
      if p(at(i)) then return Some(at(i))
      i += 1
    None

  def ++ [U >: T](ys: Lst[U]): Lst[U] =
    if isEmpty then ys
    else if ys.isEmpty then this
    else (new Lst.Buffer[U](length + ys.length) ++= this ++= ys).toLst

  def :+ [U >: T](y: U): Lst[U] =
    (new Lst.Buffer[U](length + 1) ++= this += y).toLst

  def updated[U >: T](idx: Int, x: U): Lst[U] =
    var rs = new Array[Object](length)
    arraycopy(arr, 0, rs, 0, length)
    rs(idx) = x.asInstanceOf[Object]
    new Lst(rs)

  def slice(from: Int, until: Int): Lst[T] =
    if from < 0 then slice(0, until)
    else if until > length then slice(from, length)
    else if from >= until then Lst()
    else
      val rs = new Array[Object](until - from)
      var i = 0
      while i < rs.length do
        rs(i) = at(i + from).asInstanceOf[Object]
        i += 1
      new Lst(rs)

  def drop(n: Int): Lst[T] =
    if n <= 0 then this else slice(n, length)

  def take(n: Int): Lst[T] =
    if n >= length then this else slice(0, n)

  def dropRight(n: Int): Lst[T] =
    if n <= 0 then this else slice(0, length - n)

  def takeRight(n: Int): Lst[T] =
    if n >= length then this else slice(length - n, length)

  def init: Lst[T] =
    slice(0, length - 1)

  def splitAt(n: Int): (Lst[T], Lst[T]) =
    if n <= 0 then (Lst(), this)
    else if n >= length then (this, Lst())
    else (slice(0, n), slice(n, length))

  def indexOf[U >: T](x: U): Int =
    var i = 0
    while i < length do
      if at(i) == x then return i
      i += 1
    -1

  def indexWhere(p: T => Boolean): Int =
    var i = 0
    while i < length do
      if p(at(i)) then return i
      i += 1
    -1

  def dropWhile(p: T => Boolean): Lst[T] =
    var i = 0
    while i < length && p(at(i)) do i += 1
    drop(i)

  def takeWhile(p: T => Boolean): Lst[T] =
    var i = 0
    while i < length && p(at(i)) do i += 1
    take(i)

  def span(p: T => Boolean): (Lst[T], Lst[T]) =
    var i = 0
    while i < length && p(at(i)) do i += 1
    (take(i), drop(i))

  def partition(p: T => Boolean): (Lst[T], Lst[T]) =
    val yes = Lst.Buffer[T]()
    val no = Lst.Buffer[T]()
    var i = 0
    while i < length do
      val x = at(i)
      (if p(x) then yes else no) += x
      i += 1
    (yes.toLst, no.toLst)

  def toList: List[T] =
    var i = length
    var rs: List[T] = Nil
    while i > 0 do
      i -= 1
      rs = at(i) :: rs
    rs

  def reverseToLst: List[T] =
    var i = 0
    var rs: List[T] = Nil
    while i < length do
      rs = at(i) :: rs
      i += 1
    rs

  def iterator: Iterator[T] = new scala.collection.Iterator:
    var cur = 0
    def hasNext: Boolean = cur < arr.length
    def next(): T =
      val elem = at(cur)
      cur += 1
      elem

  def toIterable: Lst.Iterable[T] =
    Lst.Iterable(this)

  def toSet[U >: T]: immutable.Set[U] = immutable.Set.from(toIterable)

  def corresponds[U](ys: Lst[U])(p: (T, U) => Boolean): Boolean =
    length == ys.length
      && {
        var i = 0
        while i < length && p(at(i), ys.at(i)) do i += 1
        i == length
      }

  def copyToArray[U >: T <: AnyRef](ys: Array[U], start: Int = 0): Unit =
    arraycopy(arr, 0, ys, start, length)

  def mapToList[U](f: T => U): List[U] =
    val buf = new ListBuffer[U]
    var i = 0
    while i < length do
      buf += f(at(i))
      i += 1
    buf.toList

  def mkString(prefix: String, sep: String, suffix: String): String =
    val sb = StringBuilder()
    sb ++= prefix
    var i = 0
    while i < length do
      if i > 0 then sb ++= sep
      sb ++= at(i).toString
      i += 1
    sb ++= suffix
    sb.toString

  def mkString(sep: String): String = mkString("", sep, "")
  def mkString: String = mkString("")

  def _toString: String = mkString("List(", ", ", ")")

  def lazyZip[U](that: collection.Iterable[U]): collection.LazyZip2[T, U, ? <: Lst.Iterable[T]] =
    toIterable.lazyZip(that)

  def lazyZip[U](that: Lst[U]): collection.LazyZip2[T, U, ? <: Lst.Iterable[T]] =
    lazyZip(that.toIterable)

  override def equals(ys: Any): Boolean = ys match
    case ys: Lst[_] => Arrays.equals(elems, ys.elems)
    case _ => false

  override def hashCode: Int = Arrays.hashCode(elems)

  override def toString = mkString("List(", ", ", ")")
}
object Lst {

  val genericEmpty: Array[Object] = new Array[Object](0)

  def apply[T](): Lst[T] = new Lst(genericEmpty)

  def apply[T](x: T): Lst[T] =
    val rs = new Array[Object](1)
    rs(0) = x.asInstanceOf[Object]
    new Lst(rs)

  def apply[T](x0: T, x1: T): Lst[T] =
    val rs = new Array[Object](2)
    rs(0) = x0.asInstanceOf[Object]
    rs(1) = x1.asInstanceOf[Object]
    new Lst(rs)

  def apply[T](x0: T, x1: T, x2: T): Lst[T] =
    val rs = new Array[Object](3)
    rs(0) = x0.asInstanceOf[Object]
    rs(1) = x1.asInstanceOf[Object]
    rs(2) = x2.asInstanceOf[Object]
    new Lst(rs)

  def apply[T](x0: T, x1: T, x2: T, x3: T): Lst[T] =
    val rs = new Array[Object](4)
    rs(0) = x0.asInstanceOf[Object]
    rs(1) = x1.asInstanceOf[Object]
    rs(2) = x2.asInstanceOf[Object]
    rs(3) = x3.asInstanceOf[Object]
    new Lst(rs)

  def apply[T](xs: T*): Lst[T] =
    val rs = new Array[Object](xs.size)
    var i = 0
    while i < rs.length do
      rs(i) = xs(i).asInstanceOf[Object]
      i += 1
    new Lst(rs)

  def fill[T](n: Int)(elemFn: => T): Lst[T] =
    val rs = new Array[Object](n max 0)
    var i = 0
    while i < n do
      rs(i) = elemFn.asInstanceOf[Object]
      i += 1
    new Lst(rs)

  def tabulate[T](n: Int)(elemFn: Int => T): Lst[T] =
    val rs = new Array[Object](n)
    var i = 0
    while i < n do
      rs(i) = elemFn(i).asInstanceOf[Object]
      i += 1
    new Lst(rs)

  extension [T <: AnyRef](xs: Lst[T])

    def eqElements[U <: AnyRef](ys: Lst[U]): Boolean =
      (xs.asInstanceOf[Object] `eq` ys.asInstanceOf[Object])
      || xs.length == ys.length
          && {
            var i = 0
            while i < xs.length && (xs(i) `eq` ys(i)) do i += 1
            i == xs.length
          }

  extension [T: ClassTag](xs: Lst[T])
    def toArray: Array[T] =
      val rs = new Array[T](xs.length)
      arraycopy(xs.arr, 0, rs, 0, xs.length)
      rs

  extension [T](x: T)
    def +:(ys: Lst[T]): Lst[T] =
      (Buffer[T](ys.length + 1) += x ++= ys).toLst

  extension [T, U](xs: Lst[(T, U)])
    def unzip: (Lst[T], Lst[U]) =
      val buf1 = Buffer[T](xs.length)
      val buf2 = Buffer[U](xs.length)
      for (x1, x2) <- xs do
        buf1 += x1
        buf2 += x2
      (buf1.toLst, buf2.toLst)

    def toMap: immutable.Map[T, U] = immutable.Map.from(xs.toIterable)

  extension [T](xss: Lst[Lst[T]])

    def flatten: Lst[T] =
      var i = 0
      while i < xss.length && xss.at(i).isEmpty do i += 1
      var j = i + 1
      while j < xss.length && xss.at(j).isEmpty do j += 1
      if i == xss.length then
        Lst()
      else if j == xss.length then
        xss.at(i)
      else
        val totalLength = xss.foldLeft(0)((len, xs) => len + xs.length)
        val buf = Buffer[T](totalLength)
        var i = 0
        while i < xss.length do
          buf ++= xss(i)
          i += 1
        buf.toLst

    def nestedExists(p: T => Boolean): Boolean =
      var i = 0
      while i < xss.length do
        val xs = xss.at(i)
        var j = 0
        while j < xs.length do
          if p(xs(j)) then return true
          j += 1
        i += 1
      false

  class Buffer[T](initSize: Int = 8) {
    private var elems = new Array[Object](initSize)
    private var siz: Int = 0
    private var dirty = false // need a copy after a toLst and before following += or ++=

    private def ensureCapacity(added: Int) =
      val newSize = siz + added
      if newSize > elems.length || dirty then
        require(elems.length > 0)
        var newCapacity = elems.length
        while newSize > newCapacity do newCapacity = newCapacity << 1
        val newElems = new Array[Object](newCapacity)
        arraycopy(elems, 0, newElems, 0, siz)
        elems = newElems
        dirty = false

    def length: Int = siz
    def size: Int = siz

    def at(i: Int) = elems(i).asInstanceOf[T]

    def += (x: T): this.type =
      ensureCapacity(1)
      elems(siz) = x.asInstanceOf[Object]
      siz += 1
      this

    def ++= (xs: Lst[T]): this.type =
      ensureCapacity(xs.length)
      arraycopy(xs.arr, 0, elems, siz, xs.length)
      siz += xs.length
      this

    def foreach(f: T => Unit): Unit =
      var i = 0
      while i < size do
        f(at(i))
        i += 1

    def contains(x: T): Boolean =
      var i = 0
      while i < size && elems(i) != x do i += 1
      i < size

    def toLst: Lst[T] =
      if siz == 0 then Lst()
      else if siz == elems.length then
        dirty = true
        new Lst[T](elems)
      else
        val result = new Array[Object](siz)
        arraycopy(elems, 0, result, 0, siz)
        new Lst[T](result)
  }

  class Iterable[+T](lst: Lst[T]) extends collection.immutable.Iterable[T]:
    def iterator: Iterator[T] = lst.iterator

  /** Extractor for lsts of length 1 */
  object Singleton:
    def unapply[T](xs: Lst[T]): Option[T] =
      if xs.length == 1 then Some(xs(0)) else None

  /** Extractor for lsts of length 2 */
  object Pair:
    def unapply[T](xs: Lst[T]): Option[(T, T)] =
      if xs.length == 2 then Some(xs(0), xs(1)) else None

  /** Extractor for lsts of length 3 */
  object Triple:
    def unapply[T](xs: Lst[T]): Option[(T, T, T)] =
      if xs.length == 3 then Some(xs(0), xs(1), xs(2)) else None

  /** Extractor for nonempty lsts starting with some element pattern */
  object StartingWith:
    def unapply[T](xs: Lst[T]): Option[T] =
      if xs.length >= 1 then Some(xs(0)) else None
}

@main def LstTest() =
  val xs = Lst("hello", "world")
  println(xs)
  val ys = xs.map(_.tail)
  println(ys.mkString("[", ",", "]"))
  val zs = xs.flatMap(x => Lst(x, "?"))
  println(zs)
  val as = zs.filter(_.length > 1)
  println(as.mkString)
  println(as.toList)
  assert(xs.contains("hello"))
  assert(xs.exists(_.startsWith("wor")))
  val arr = new Array[String](2)
  xs.copyToArray(arr)
  val xs1 = Lst(arr*)
  assert(xs1.eqElements(xs))
  assert(xs1.corresponds(xs)(_ == _))
  assert(xs1 == xs)
  var ns: Lst[String] | Null = null
  assert(ns == null)
