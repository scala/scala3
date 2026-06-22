package dotty.tools
package dotc.util
import java.lang.System.arraycopy
import collection.mutable.ListBuffer
import reflect.ClassTag

opaque type Lst[+T <: AnyRef] <: AnyRef = Array[Object]

object Lst {

  val genericEmpty: Array[Object] = new Array[Object](0)

  def apply[T <: AnyRef](): Lst[T] = genericEmpty

  def apply[T <: AnyRef](x: T): Lst[T] =
    val rs = new Array[Object](1)
    rs(0) = x
    rs

  def apply[T <: AnyRef](x0: T, x1: T): Lst[T] =
    val rs = new Array[Object](2)
    rs(0) = x0
    rs(1) = x1
    rs

  def apply[T <: AnyRef](x0: T, x1: T, x2: T): Lst[T] =
    val rs = new Array[Object](3)
    rs(0) = x0
    rs(1) = x1
    rs(2) = x2
    rs

  def apply[T <: AnyRef](x0: T, x1: T, x2: T, x3: T): Lst[T] =
    val rs = new Array[Object](4)
    rs(0) = x0
    rs(1) = x1
    rs(2) = x2
    rs(3) = x3
    rs

  def apply[T <: AnyRef](xs: T*): Lst[T] =
    val rs = new Array[Object](xs.length)
    var i = 0
    while i < xs.length do
      rs(i) = xs(i)
      i += 1
    rs

  def fill[T <: AnyRef](n: Int)(elemFn: => T): Lst[T] =
    val rs = new Array[Object](n)
    var i = 0
    while i < n do
      rs(i) = elemFn
      i += 1
    rs

  def tabulate[T <: AnyRef](n: Int)(elemFn: Int => T): Lst[T] =
    val rs = new Array[Object](n)
    var i = 0
    while i < n do
      rs(i) = elemFn(i)
      i += 1
    rs

  extension [T <: AnyRef](xs: Lst[T])
    private def arr: Array[Object] = xs: Array[Object]
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

    def map[U <: AnyRef](f: T => U): Lst[U] =
      val ys = new Array[Object](arr.length)
      var i = 0
      while i < arr.length do
        ys(i) = f(at(i))
        i += 1
      ys

    def mapConserve[U >: T <: AnyRef](f: T => U): Lst[U] =
      val ys = new Array[Object](arr.length)
      var i = 0
      var change = false
      while i < xs.length do
        ys(i) = f(at(i))
        if ys(i) `ne` at(i) then change = true
        i += 1
      if change then ys else xs

    def zip[U <: AnyRef](ys: Lst[U]): Lst[(T, U)] =
      val zs = new Array[Object](xs.length min ys.length)
      var i = 0
      while i < zs.length do
        zs(i) = (xs.at(i), ys.at(i))
        i += 1
      zs

    def zipWithIndex: Lst[(T, Int)] =
      val zs = new Array[Object](xs.length)
      var i = 0
      while i < xs.length do
        zs(i) = (xs.at(i), i)
        i += 1
      zs

    def zipWith[U <: AnyRef, V <: AnyRef](ys: Lst[U])(f: (T, U) => V): Lst[V] =
      val zs = new Array[Object](xs.length min ys.length)
      var i = 0
      while i < zs.length do
        zs(i) = f(xs.at(i), ys.at(i))
        i += 1
      zs

    /** Like `xs.zip(ys).map(f)`, but returns list `xs` itself
     *  - instead of a copy - if function `f` maps all elements of
     *  `xs` to themselves. Also, it is required that `ys` is at least
     *  as long as `xs`.
     */
    def zipWithConserve[U <: AnyRef](ys: Lst[U])(f: (T, U) => T): Lst[T] =
      val zs = new Array[Object](xs.length min ys.length)
      var i = 0
      var change = false
      while i < zs.length do
        zs(i) = f(xs.at(i), ys.at(i))
        if xs.at(i) `ne` zs.at(i) then change = true
        i += 1
      if change then zs else xs

    def foldLeft[U](z: U)(f: (U, T) => U): U =
      var acc = z
      var i = 0
      while i < arr.length do
        acc = f(acc, at(i))
        i += 1
      acc

    def flatMap[U <: AnyRef](f: T => Lst[U]): Lst[U] = xs.map(f).flatten

    def filter(p: T => Boolean): Lst[T] =
      val buf = LstBuffer(xs.length)
      var i = 0
      while i < xs.length do
        val x = xs.at(i)
        if p(x) then buf += x
        i += 1
      if buf.length == length then xs else buf.toLst

    def collect[U <: AnyRef](f: PartialFunction[T, U]): Lst[U] =
      val buf = LstBuffer(xs.length)
      var i = 0
      while i < xs.length do
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

    def contains(elem: T): Boolean =
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

    def ++(ys: Lst[T]): Lst[T] =
      if xs.isEmpty then ys
      else if ys.isEmpty then xs
      else (LstBuffer(xs.length + ys.length) ++= xs ++= ys).toLst

    def :+(y: T): Lst[T] =
      (LstBuffer(xs.length + 1) ++= xs += y).toLst

    def updated[U >: T <: AnyRef](idx: Int, x: U): Lst[U] =
      var rs = new Array[Object](xs.length)
      arraycopy(xs, 0, rs, 0, xs.length)
      rs(idx) = x
      rs

    def slice(from: Int, until: Int): Lst[T] =
      if from < 0 then slice(0, until)
      else if until > xs.length then slice(from, xs.length)
      else if from >= until then Lst()
      else
        val rs = new Array[Object](until - from)
        var i = 0
        while i < rs.length do
          rs(i) = xs(i + from)
          i += 1
        rs

    def drop(n: Int): Lst[T] =
      if n <= 0 then xs else slice(n, length)

    def take(n: Int): Lst[T] =
      if n >= length then xs else slice(0, n)

    def dropRight(n: Int): Lst[T] =
      if n <= 0 then xs else slice(0, length - n)

    def takeRight(n: Int): Lst[T] =
      if n >= length then xs else slice(length - n, length)

    def init: Lst[T] =
      slice(0, length - 1)

    def splitAt(n: Int): (Lst[T], Lst[T]) =
      if n <= 0 then (Lst(), xs)
      else if n >= length then (xs, Lst())
      else (slice(0, n), slice(n, length))

    def indexWhere(p: T => Boolean): Int =
      var i = 0
      while i < length do
        if p(at(i)) then return i
        i += 1
      -1

    def toList: List[T] =
      var i = xs.length
      var rs: List[T] = Nil
      while i > 0 do
        i -= 1
        rs = at(i) :: rs
      rs

    def iterator: Iterator[T] = new scala.collection.Iterator:
      var cur = 0
      def hasNext: Boolean = cur < arr.length
      def next(): T =
        val elem = at(cur)
        cur += 1
        elem

    def toIterable: LstIterable[T] =
      LstIterable(xs)

    def corresponds[U <: AnyRef](ys: Lst[U])(p: (T, U) => Boolean): Boolean =
      xs.length == ys.length
        && {
          var i = 0
          while i < xs.length && p(xs.at(i), ys.at(i)) do i += 1
          i == xs.length
        }

    def eqElements[U <: AnyRef](ys: Lst[U]): Boolean =
      (xs `eq` ys)
      || xs.length == ys.length
          && {
            var i = 0
            while i < xs.length && (xs(i) `eq` ys(i)) do i += 1
            i == xs.length
          }

    def ===[U <: AnyRef](ys: Lst[U]): Boolean =
      (xs `eq` ys)
      || xs.length == ys.length
          && {
            var i = 0
            while i < xs.length && (xs(i) == ys(i)) do i += 1
            i == xs.length
          }

    def hash: Int =
      java.util.Arrays.hashCode(xs)

    def copyToArray(ys: Array[T], start: Int = 0): Unit =
      arraycopy(xs, 0, ys, start, length)

    def mapToList[U](f: T => U): List[U] =
      val buf = new ListBuffer[U]
      var i = 0
      while i < xs.length do
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

    def lazyZip[U](that: Iterable[U]): collection.LazyZip2[T, U, ? <: LstIterable[T]] =
      toIterable.lazyZip(that)

    def lazyZip[U <: AnyRef](that: Lst[U]): collection.LazyZip2[T, U, ? <: LstIterable[T]] =
      lazyZip(that.toIterable)

  extension [T <: AnyRef](xss: Lst[Lst[T]])
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
        val buf = LstBuffer(totalLength)
        var i = 0
        while i < xss.length do
          buf ++= xss.at(i)
          i += 1
        buf.toLst

  extension [T <: AnyRef: ClassTag](xs: Lst[T])
    def toArray: Array[T] =
      val rs = new Array[T](xs.length)
      arraycopy(xs, 0, rs, 0, xs.length)
      rs

  extension [T <: AnyRef](x: T)
    def +:(ys: Lst[T]): Lst[T] =
      (LstBuffer[T](ys.length + 1) += x ++= ys).toLst
}

class LstBuffer[T <: AnyRef](initSize: Int = 8) {
  import Lst.*

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

  def += (x: T): this.type =
    ensureCapacity(1)
    elems(siz) = x
    siz += 1
    this

  def ++= (xs: Lst[T]): this.type =
    ensureCapacity(xs.length)
    arraycopy(xs, 0, elems, siz, xs.length)
    siz += xs.length
    this

  def toLst: Lst[T] =
    if siz == 0 then Lst()
    else if siz == elems.length then
      dirty = true
      elems.asInstanceOf[Lst[T]]
    else
      val result = new Array[Object](siz)
      arraycopy(elems, 0, result, 0, siz)
      result.asInstanceOf[Lst[T]]
}

class LstIterable[+T <: AnyRef](lst: Lst[T]) extends Iterable[T]:
  def iterator: Iterator[T] = lst.iterator

/** Extractor for lsts of length 1 */
object Lst1:
  def unapply[T <: AnyRef](xs: Lst[T]): Option[T] =
    if xs.length == 1 then Some(xs(0)) else None

@main def LstTest() =
  val xs = Lst("hello", "world")
  println(xs.mkString)
  val ys = xs.map(_.tail)
  println(ys.mkString("[", ",", "]"))
  val zs = xs.flatMap(x => Lst(x, "?"))
  println(zs.mkString)
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
  assert(xs1 === xs)
