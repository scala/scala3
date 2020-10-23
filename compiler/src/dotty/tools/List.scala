package dotty.tools

import collection.mutable.{StringBuilder, ListBuffer}
import collection.immutable.{Map, List => sList, Nil => sNil}
import reflect.ClassTag
import annotation.{infix, tailrec}
import math.Ordering

/** A lightweight class for lists, optimized for short and medium lengths.
 *  A list is represented at runtime as
 *
 *    If it is empty:                    the value `List.Nil`
 *    If it contains one element,
 *    and the element is not an array:   the element itself
 *    Otherwise:                         an Array[Any] containing the elements
 */

type List[+T] = List.Vault.List[T]
val Nil: List[Nothing] = List.Nil
val ScalaNil = sNil
val :: = List.::
val +: = List.+:
val :+ = List.:+
val nullList: List[Null] = List(null)
val nullListList: List[List[Null]] = List(List(null))

extension [T](xs: IterableOnce[T])
  def tolist: List[T] = List.fromIterable(xs)
  def ::: (ys: List[T]): List[T] = List.fromIterable(xs) ++ ys

extension [T](xs: List[T])
  def ::: (ys: List[T]): List[T] = xs ++ ys

extension (x: Any)
  def debugString: String = x match
    case x: List.Arr => x.map(_.debugString).mkString("Array(", ", ", ")")
    case _ => String.valueOf(x)

object List:

  type Arr = Array[Any]

  /** Lists can only be compared with other Lists... */
  given lstEql[T, U] as Eql[List[T], List[U]] = Eql.derived

  /** ... except that locally we can also match with `null` */
  private given lstNullEq[T] as Eql[Null, List[T]] = Eql.derived

  object Vault:
    opaque type List[+T] = Any

    private[List] def Nil: List[Nothing] = null

    /** Create a list of length 1 from given element `x` */
    private[List] def single[T](x: T): List[T] = x match
      case null | _: Arr =>
        val wrapped = new Arr(1)
        wrapped(0) = x
        wrapped
      case _ =>
        x

    /** Create a list of length `xs.length` from the elements in `xs`
     *  @pre  xs is not empty
     */
    private[List] def multi[T](xs: Arr): List[T] =
      if xs.length == 0 then Nil
      else if xs.length == 1 then single(xs(0).asInstanceOf[T])
      else xs

    /** Create a list directly from the given array. Do not convert arrays of size <= 1.
     */
    private[List] def fromArr[T](xs: Arr): List[T] =
      xs
  end Vault

  val Nil = Vault.Nil
  import Vault.{single, multi, fromArr}

  private inline def eq(x: Any, y: Any) = x.asInstanceOf[AnyRef] `eq` y.asInstanceOf[AnyRef]

  def apply[T](): List[T] = Nil

  def apply[T](x0: T): List[T] = single[T](x0)

  def apply[T](x0: T, x1: T): List[T] =
    val xs = new Arr(2)
    xs(0) = x0
    xs(1) = x1
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T): List[T] =
    val xs = new Arr(3)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T, x3: T): List[T] =
    val xs = new Arr(4)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    xs(3) = x3
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T, x3: T, x4: T, others: T*): List[T] =
    val xs = new Arr(5 + others.length)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    xs(3) = x3
    xs(4) = x4
    others.copyToArray(xs, 5)
    fromArr[T](xs)

  def empty[T]: List[T] = Nil

  private def _fromArray[T](xs: Arr, start: Int, end: Int): List[T] =
    val len = end - start
    if len <= 0 then Nil
    else if len == 1 then single[T](xs(start).asInstanceOf[T])
    else if start == 0 && end == xs.length then fromArr(xs)
    else
      val newElems = new Arr(len)
      System.arraycopy(xs, start, newElems, 0, len)
      fromArr(newElems)

  def fromArray[T](xs: Array[T], start: Int, end: Int): List[T] =
    _fromArray(xs.asInstanceOf[Arr], start, end)

  def fromArray[T](xs: Array[T]): List[T] =
    fromArray(xs, 0, xs.length)

  def fromIterable[T](it: IterableOnce[T]): List[T] =
    val buf = new Buffer[T]
    it.iterator.foreach(buf += _)
    buf.toList

  def fromSeq[T](seq: Seq[T]): List[T] =
    val elems = new Arr(seq.length)
    seq.copyToArray(elems)
    multi[T](elems)

  def fill[T](n: Int)(elem: => T) =
    val xs = new Arr(n)
    var i = 0
    while i < n do
      xs(i) = elem
      i += 1
    multi[T](xs)

  def tabulate[T](n: Int)(elem: Int => T) =
    val xs = new Arr(n)
    var i = 0
    while i < n do
      xs(i) = elem(i)
      i += 1
    multi[T](xs)

  def range(from: Int, until: Int): List[Int] =
    val xs = new Arr(until - from)
    var i = from
    while i < until do
      xs(i - from) = i
      i += 1
    multi[Int](xs)

  extension [T](xs: List[T] & Arr)
    private def at(i: Int): T = (xs: Arr)(i).asInstanceOf[T]

  extension[T](xs: List[T]):

    def length: Int = xs match
      case null => 0
      case xs: Arr => xs.length
      case x => 1

    def size: Int = length

    def isEmpty = eq(xs, null)
    def nonEmpty = !eq(xs, null)

    inline def foreach(inline op: T => Unit): Unit =
      def sharedOp(x: T) = op(x)
      xs match
        case null =>
        case xs: Arr =>
          var i = 0
          while i < xs.length do
            sharedOp(xs.at(i))
            i += 1
        case x: T @ unchecked => sharedOp(x)

    inline def foreachReversed(inline op: T => Unit): Unit =
      def sharedOp(x: T) = op(x)
      xs match
        case null =>
        case xs: Arr =>
          var i = xs.length
          while (i > 0) do
            i -= 1
            sharedOp(xs.at(i))
        case x: T @ unchecked => sharedOp(x)

    /** Like `foreach`, but completely inlines `op`, at the price of generating the code twice.
    *  Should be used only of `op` is small
    */
    inline def foreachInlined(inline op: T => Unit): Unit =
      xs match
        case null =>
        case xs: Arr =>
          var i = 0
          while i < xs.length do
            op(xs.at(i))
            i += 1
        case x: T @unchecked => op(x)

    def iterator: Iterator[T] = xs match
      case null => Iterator.empty
      case xs: Arr => (xs: Arr).iterator.asInstanceOf[Iterator[T]]
      case x: T @unchecked => Iterator.single(x)

    def copyToArray(target: Array[T], from: Int = 0) = xs match
      case null =>
      case xs: Arr => Array.copy(xs, 0, target, from, xs.length)
      case x: T @ unchecked => target(from) = x

    def toSet: collection.immutable.Set[T] =
      var xs = Set[T]()
      foreachInlined(xs += _)
      xs

    def toScalaList: scala.List[T] =
      if length == 0 then sNil
      else
        val buf = new collection.mutable.ListBuffer[T]
        foreachInlined(buf += _)
        buf.toList

    def toIterable: Iterable[T] = new Iterable[T]:
      def iterator = xs.iterator

    def toSeq: ListSlice[T] = sliceToSeq(0, length)
    def toIndexedSeq: IndexedSeq[T] = toSeq

    def sliceToSeq(start: Int = 0, end: Int = xs.length): ListSlice[T] = ListSlice[T](xs, start, end)

    def filter(p: T => Boolean): List[T] = xs match
      case null => Nil
      case xs: Arr =>
        val scratch = new Arr(xs.length)
        var i = 0
        var len = 0
        while i < xs.length do
          val x = xs.at(i)
          if p(x) then
            scratch(len) = x
            len += 1
          i += 1
        if len == xs.length then xs
        else _fromArray(scratch, 0, len)
      case x: T @unchecked => if (p(x)) xs else Nil

    def filterNot(p: T => Boolean): List[T] = filter(!p(_))

    def withFilter(p: T => Boolean) = WithFilter(xs, p)

    def partition(p: T => Boolean): (List[T], List[T]) = xs match
      case null => (Nil, Nil)
      case xs: Arr =>
        val hits, misses = new Arr(xs.length)
        var i = 0
        var hitCount = 0
        var missCount = 0
        while i < xs.length do
          val x = xs.at(i)
          if p(x) then
            hits(hitCount) = x
            hitCount += 1
          else
            misses(missCount) = x
            missCount += 1
          i += 1
        if hitCount == xs.length then (xs, Nil)
        else if missCount == xs.length then (Nil, xs)
        else (_fromArray(hits, 0, hitCount), _fromArray(misses, 0, missCount))
      case x: T @unchecked =>
        if (p(x)) (xs, Nil) else (Nil, xs)

    def span(p: T => Boolean): (List[T], List[T]) = xs match
      case null => (Nil, Nil)
      case xs: Arr =>
        var i = 0
        while i < xs.length && p(xs.at(i)) do i += 1
        if i == xs.length then (xs, Nil)
        else if i == 0 then (Nil, xs)
        else (_fromArray(xs, 0, i), _fromArray(xs, i, xs.length))
      case x: T @unchecked =>
        if p(x) then (single[T](x), Nil) else (Nil, single[T](x))

    def splitAt(n: Int): (List[T], List[T]) = (slice(0, n), slice(n, length))

    inline def exists(inline p: T => Boolean): Boolean =
      def op(x: T) = p(x)
      xs match
        case null => false
        case xs: Arr =>
          var i = 0
          while i < xs.length && !op(xs.at(i)) do i += 1
          i < xs.length
        case x: T @unchecked =>
          op(x)

    inline def forall(inline p: T => Boolean): Boolean =
      def op(x: T) = p(x)
      xs match
        case null => true
        case xs: Arr =>
          var i = 0
          while i < xs.length && op(xs.at(i)) do i += 1
          i == xs.length
        case x: T @unchecked =>
          op(x)

    def count(p: T => Boolean): Int =
      var n = 0
      foreachInlined(x => if p(x) then n += 1)
      n

    def contains(x: T): Boolean = xs match
      case null => false
      case xs: Arr =>
        var i = 0
        while i < xs.length && xs.at(i) != x do i += 1
        i < xs.length
      case elem: T @unchecked =>
        elem == x

    def reverse: List[T] = xs match
      case xs: Arr =>
        val newElems = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(xs.length - 1 - i) = xs.at(i)
          i += 1
        multi[T](newElems)
      case _ => xs

    def apply(n: Int): T = xs match
      case null =>
        throw IndexOutOfBoundsException(n.toString)
      case xs: Arr =>
        xs.at(n)
      case x: T @unchecked =>
        if n == 0 then x else throw new IndexOutOfBoundsException(n.toString)

    def head: T = xs.apply(0)
    def last: T = xs.apply(length - 1)

    inline def headOr(inline alt: T): T = if isEmpty then alt else head

    def headOption: Option[T] = if isEmpty then None else Some(head)
    def lastOption: Option[T] = if isEmpty then None else Some(last)

    def slice(start: Int, end: Int): List[T] =
      if start < 0 then slice(0, end)
      else xs match
        case null => xs
        case xs: Arr => _fromArray(xs, start, end min xs.length)
        case x: T @ unchecked => if start == 0 && end > 0 then xs else Nil

    def drop(n: Int): List[T] = slice(n, length)
    def tail: List[T] = drop(1)
    def take(n: Int): List[T] = slice(0, n)

    def dropRight(n: Int): List[T] = take(length - n)
    def takeRight(n: Int): List[T] = drop(length - n)
    def init: List[T] = dropRight(1)

    def firstIndexOf(x: T, from: Int = 0): Int = xs match
      case null => 0
      case xs: Arr =>
        var i = from
        while i < length && xs.at(i) != x do i += 1
        i
      case elem: T @ unchecked => if from == 0 && elem == x then 0 else 1

    def firstIndexWhere(p: T => Boolean, from: Int = 0): Int = xs match
      case null => 0
      case xs: Arr =>
        var i = from
        while i < length && !p(xs.at(i)) do i += 1
        i
      case elem: T @ unchecked => if from == 0 && p(elem) then 0 else 1

    def indexOf(x: T, from: Int = 0) =
      val idx = firstIndexOf(x, from)
      if idx == length then -1 else idx

    def indexWhere(p: T => Boolean): Int =
      val idx = firstIndexWhere(p)
      if idx == length then -1 else idx

    def find(p: T => Boolean): Option[T] =
      val idx = firstIndexWhere(p)
      if idx < length then Some(xs.apply(idx)) else None

    def takeWhile(p: T => Boolean): List[T] = take(firstIndexWhere(!p(_)))
    def dropWhile(p: T => Boolean): List[T] = drop(firstIndexWhere(!p(_)))

    def reduceLeft(op: (T, T) => T) = xs match
      case null =>
        throw new UnsupportedOperationException("List.Nil.reduceLeft")
      case xs: Arr =>
        var i = 1
        var acc = xs.at(0)
        while i < xs.length do
          acc = op(acc, xs.at(i))
          i += 1
        acc
      case x: T @unchecked =>
        x
    def reduce(op: (T, T) => T) = reduceLeft(op)

    def ::: (ys: List[T]): List[T] = xs ++ ys

    def ++ (ys: List[T]): List[T] =
      if xs.isEmpty then ys
      else if ys.isEmpty then xs
      else
        val len1 = xs.length
        val len2 = ys.length
        val newElems = new Arr(len1 + len2)
        xs.copyToArray(newElems, 0)
        ys.copyToArray(newElems, len1)
        multi[T](newElems)

    def ++ (ys: IterableOnce[T]): List[T] =
      val yit = ys.iterator
      if yit.isEmpty then xs
      else if xs.isEmpty then fromIterable(yit)
      else (Buffer[T]() ++= xs ++= yit).toList

    def ++ (ys: Buffer[T]): List[T] =
      if xs.isEmpty then ys.toList
      else if ys.isEmpty then xs
      else (Buffer[T]() ++= xs ++= ys).toList

    def :+ (x: T): List[T] = xs match
      case null => List(x)
      case xs: Arr =>
        val newElems = new Arr(xs.length + 1)
        xs.copyToArray(newElems, 0)
        newElems(xs.length) = x
        multi[T](newElems)
      case elem: T @unchecked =>
        List(elem, x)

    def updated(idx: Int, x: T): List[T] =
      xs match
        case xs: Arr => multi[T]((xs: Arr).updated(idx, x))
        case elem: T @unchecked if idx == 0 => single[T](x)
        case _ => throw IndexOutOfBoundsException(idx.toString)

    def union(ys: List[T]): List[T] = xs ++ ys.filterNot(xs.contains(_))
    def intersect(ys: List[T]): List[T] = xs.filter(ys.contains(_))

    def zipWithIndex: List[(T, Int)] = xs match
      case null => Nil
      case xs: Arr =>
        val newElems = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(i) = (xs.at(i), i)
          i += 1
        multi[(T, Int)](newElems)
      case x: T @unchecked =>
        single[(T, Int)]((x, 0))

    def indices: List[Int] = xs match
      case null => Nil
      case xs: Arr =>
        val newElems: Arr = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(i) = i
          i += 1
        multi[Int](newElems)
      case elem =>
        single[Int](0)

    def distinct: List[T] =
      var seen = Set.empty[T]
      val buf = Buffer[T].sizeHint(length)
      foreachInlined { x =>
        if !seen.contains(x) then
          seen += x
          buf += x
      }
      buf.toList

    def lengthCompare(n: Int): Int =
      if length < n then -1
      else if length > n then 1
      else 0

    def maxBy(f: T => Int): T =
      var maxElem = head
      var maxVal = f(head)
      xs.foreachInlined { x =>
        val elemVal = f(x)
        if elemVal > maxVal then
          maxElem = x
          maxVal = elemVal
      }
      maxElem

    def sortWith(lt: (T, T) => Boolean): List[T] =
      xs.sorted(using Ordering.fromLessThan(lt))

    def startsWith(ys: List[T]): Boolean =
      var i = 0
      while i < length && i < ys.length && xs(i) == ys(i) do i += 1
      i == ys.length

    def endsWith(ys: List[T]): Boolean =
      var i = xs.length - 1
      var j = ys.length - 1
      while i >= 0 && j >= 0 && xs(i) == ys(i) do { i -= 1; j -= 1 }
      j < 0

    def sorted(using ord: Ordering[T]): List[T] =
      if xs.length <= 1 then xs
      else
        val arr = new Arr(xs.length)
        xs.copyToArray(arr)
        java.util.Arrays.sort(arr.asInstanceOf[Array[AnyRef]], ord.asInstanceOf[Ordering[AnyRef]])
        fromArr(arr)
  end extension

  extension [T: ClassTag](xs: List[T]):
    def toArray: Array[T] =
      val result = new Array[T](xs.length)
      xs.copyToArray(result, 0)
      result

  extension [T, U](xs: List[T]):

    /** `f` is pulled out, not duplicated */
    inline def map(inline f: T => U): List[U] =
      def op(x: T) = f(x)
      xs match
        case null => Nil
        case xs: Arr =>
          val newElems = new Arr(xs.length)
          var i = 0
          while i < xs.length do
            newElems(i) = op(xs.at(i))
            i += 1
          multi[U](newElems)
        case x: T @ unchecked => single[U](op(x))

    def mapConserve(f: T => U): List[U] = xs match
      case null => Nil
      case xs: Arr =>
        var newElems: Arr = null
        var i = 0
        while i < xs.length do
          val x = xs.at(i)
          val y = f(x)
          if newElems != null then newElems(i) = y
          else if !eq(x, y) then
            newElems = new Arr(xs.length)
            System.arraycopy(xs, 0, newElems, 0, i)
            newElems(i) = y
          i += 1
        if newElems == null then xs.asInstanceOf[List[U]] else multi[U](newElems)
      case x: T @ unchecked => single[U](f(x))

    def flatMap(f: T => List[U]): List[U] = xs match
      case null => Nil
      case xs: Arr =>
        val newElemss = new Array[List[U]](xs.length)
        var i = 0
        var len = 0
        while i < xs.length do
          val ys = f(xs.at(i))
          len += ys.length
          newElemss(i) = ys
          i += 1
        if len == 0 then Nil
        else if len == 1 then
          i = 0
          while eq(newElemss(i), null) do i += 1
          newElemss(i)
        else
          val newElems = new Arr(len)
          i = 0
          var j = 0
          while i < newElemss.length do
            val ys = newElemss(i)
            ys.copyToArray(newElems, j)
            j += ys.length
            i += 1
          fromArr[U](newElems)
      case x: T @ unchecked => f(x)

    def flatMapIterable(f: T => IterableOnce[U]): List[U] =
      flatMap(x => fromIterable(f(x)))

    def collect(pf: PartialFunction[T, U]): List[U] =
      val buf = new Buffer[U].sizeHint(xs.length)
      xs.foreachInlined(x => if pf.isDefinedAt(x) then buf += pf(x))
      buf.toList

    def collectFirst(pf: PartialFunction[T, U]): Option[U] =
      var idx = xs.firstIndexWhere(pf.isDefinedAt(_))
      if idx == xs.length then None else Some(pf(xs(idx)))

    inline def foldLeft(z: U)(inline f: (U, T) => U) =
      def op(x: U, y: T) = f(x, y)
      xs match
        case null => z
        case xs: Arr =>
          var i = 0
          var acc = z
          while i < xs.length do
            acc = op(acc, xs.at(i))
            i += 1
          acc
        case x: T @unchecked =>
          op(z, x)

    inline def foldRight(z: U)(inline f: (T, U) => U) =
      def op(x: T, y: U) = f(x, y)
      xs match
        case null => z
        case xs: Arr =>
          var i = xs.length
          var acc = z
          while i > 0 do
            i -= 1
            acc = op(xs.at(i), acc)
          acc
        case x: T @unchecked =>
          op(x, z)

    def zipped(ys: List[U]) = LazyZip2(xs, ys.iterator)
    def zipped(ys: IterableOnce[U]) = LazyZip2(xs, ys)

    def zip(ys: List[U]): List[(T, U)] =
      val buf = Buffer[(T, U)].sizeHint(xs.length)
      var i = 0
      while i < xs.length && i < ys.length do
        buf += (xs(i), ys(i))
        i += 1
      buf.toList

    def zip(ys: IterableOnce[U]): List[(T, U)] =
      val buf = Buffer[(T, U)].sizeHint(xs.length)
      val yit = ys.iterator
      var i = 0
      while i < xs.length && yit.hasNext do
        buf += (xs(i), yit.next())
        i += 1
      buf.toList

    @infix def eqLst(ys: List[U]) = eq(xs, ys)
    @infix def neLst(ys: List[U]) = !eq(xs, ys)

    inline def corresponds(ys: List[U])(p: (T, U) => Boolean): Boolean =
      xs.length == ys.length
      && {
        var i = 0
        while i < xs.length && p(xs(i), ys(i)) do i += 1
        i == xs.length
      }

    def eqElements(ys: List[U]): Boolean =
      (xs `eqLst` ys) || corresponds(ys)(eq(_, _))

    def groupBy(f: T => U): Map[U, List[T]] =
      val m = dotc.util.HashMap[U, Buffer[T]]()
      xs.foreach { x =>
        val key = f(x)
        val buf = m.getOrElseUpdate(key, Buffer[T]())
        buf += x
      }
      var result = Map.empty[U, List[T]]
      for (key, buf) <- m.iterator do
        result = result.updated(key, buf.toList)
      result

    def sortBy(f: T => U)(using ord: Ordering[U]): List[T] =
      xs.sorted(using ord.on(f))
  end extension

  extension [T](x: T)
    def :: (xs: List[T]): List[T] = xs match
      case null => List(x)
      case xs: Arr =>
        val newElems = new Arr(1 + xs.length)
        newElems(0) = x
        xs.copyToArray(newElems, 1)
        multi[T](newElems)
      case elem: T @unchecked =>
        List(x, elem)
    def +: (xs: List[T]): List[T] = x :: xs

  extension [T, U] (xs: List[(T, U)])
    def toMap: Map[T, U] = xs.iterator.toMap
    def unzip: (List[T], List[U]) =
      val buf1 = Buffer[T]().sizeHint(xs.length)
      val buf2 = Buffer[U]().sizeHint(xs.length)
      xs.foreachInlined { (x, y) =>
        buf1 += x
        buf2 += y
      }
      (buf1.toList, buf2.toList)

  extension [T](xss: List[List[T]])
    def flatten: List[T] =
      val buf = Buffer[T]()
      xss.foreachInlined(buf ++= _)
      buf.toList

    def transpose: List[List[T]] =
      val bufs = xss.head.map(_ => Buffer[T].sizeHint(xss.length))
      xss.foreach { xs =>
        for i <- 0 until bufs.length do
          bufs(i) += xs(i)
      }
      bufs.map(_.toList)

  extension [T](xs: List[Iterable[T]])
    def flattenIterable: List[T] =
      val buf = Buffer[T]()
      xs.foreach(buf ++= _)
      buf.toList

  extension [T](xs: List[Option[T]])
    def flattenOption: List[T] =
      val buf = Buffer[T]()
      xs.foreach(xo => xo.foreach(x => buf += x))
      buf.toList

  extension [T](xs: List[Int])
    def min: Int = xs.reduceLeft(_ min _)
    def max: Int = xs.reduceLeft(_ max _)
    def sum: Int = xs.reduceLeft(_ + _)

  extension [T: Show](xs: List[T])
    def mkString(left: String, sep: String, right: String): String =
      val b = new StringBuilder(left)
      xs match
        case null =>
        case xs: Arr =>
          var i = 0
          while
            b ++= xs.at(i).show
            i += 1
            i < xs.length
          do
            b ++= sep
        case x: T @unchecked =>
          b ++= x.toString
      (b ++= right).toString
    def mkString(sep: String): String = mkString("", sep, "")
    def mkString: String = mkString(", ")

  class Buffer[T]:
    thisBuffer =>

    private var len = 0
    private var elem: T = _
    private var elems: Arr = _
    private var initialSize = 16

    def size = len
    def length = len
    def isEmpty = len == 0
    def nonEmpty = len != 0

    def sizeHint(n: Int): this.type =
      initialSize = n max 2
      this

    /** pre: len > 0, n >= 1 */
    private def ensureSize(n: Int): Unit =
      if len < n then
        if len == 1 then
          elems = new Arr(n max initialSize)
          elems(0) = elem
        else
          val newLen = n max len * 2
          val newElems = new Arr(newLen)
          System.arraycopy(elems, 0, newElems, 0, len)
          elems = newElems

    def += (x: T): this.type =
      if len == 0 then elem = x
      else
        ensureSize(len + 1)
        elems(len) = x
      len += 1
      this

    def ++= (xs: List[T]): this.type = appendSlice(xs, 0, xs.length)

    def appendSlice(xs: List[T], start: Int, end: Int): this.type =
      val start1 = start max 0
      val end1 = end min xs.length
      val copiedLength = end - start
      if copiedLength > 1 then appendArr(xs.asInstanceOf[Arr], start1, end1)
      else if copiedLength == 1 then this += xs(start)
      this

    /** Copy array. @pre 0 <= start; start + 1 < end <= xs.length */
    private def appendArr(xs: Arr, start: Int, end: Int): Unit =
      val copiedLength = end - start
      if len == 0 then
        elems = xs.slice(start, end)
      else
        ensureSize(len + copiedLength)
        System.arraycopy(xs, 0, elems, len, copiedLength)
      len += copiedLength

    def ++= (xs: IterableOnce[T]): this.type =
      xs.iterator.foreach(this += _)
      this

    def ++= (xs: Buffer[T]): this.type =
      if xs.length == 1 then
        this += xs(0)
      else if xs.length > 1 then
        appendArr(xs.elems, 0, xs.length)
      this

    def mapInPlace(f: T => T): this.type =
      if len == 1 then
        elem = f(elem)
      else if len > 1 then
        var i = 0
        while i < len do
          elems(i) = f(elems(i).asInstanceOf[T])
          i += 1
      this

    def dropInPlace(n : Int): this.type =
      if n <= 0 then this
      else
        val remaining = (len - n) max 0
        if n >= len - 1 then
          if n == len - 1 then elem = elems(n).asInstanceOf[T]
          elems = null
        else if len > 1 then
          System.arraycopy(elems, n, elems, 0, remaining)
        len = remaining
        this

    def trimEnd(n: Int): this.type =
      val remaining = (len - n) max 0
      if len > 1 && remaining <= 1 then
        if remaining == 1 then elem = elems(0).asInstanceOf[T]
        elems = null
      len = remaining
      this

    def apply(i: Int) =
      if len == 1 then elem
      else elems(i).asInstanceOf[T]

    def toList: List[T] =
      if len == 0 then Nil
      else if len == 1 then single(elem)
      else _fromArray(elems, 0, len)

    def reversedToList: List[T] =
      if len <= 1 then toList
      else
        val elems1 = new Arr(len)
        var i = 0
        while i < len do
          elems1(len - 1 - i) = elems(i)
          i += 1
        fromArr(elems1)

    def clear() =
      len = 0

    // SeqOps that have to be duplicated since Buffers cannot be Seqs (since toList is different)

    def iterator: Iterator[T] = new Iterator[T]:
      var idx = 0
      def hasNext = idx < len
      def next(): T =
        val result = thisBuffer.apply(idx)
        idx += 1
        result

    def foreach(f: T => Unit): Unit =
      if length == 1 then
        f(elem)
      else if length > 1 then
        var i = 0
        while i < len do
          f(elems(i).asInstanceOf[T])
          i += 1

    def foreachReversed(f: T => Unit): Unit =
      if length == 1 then
        f(elem)
      else if length > 1 then
        var i = len
        while i > 0 do
          i -= 1
          f(elems(i).asInstanceOf[T])

    def head = apply(0)
    def last = apply(len - 1)

    class Seq extends IndexedSeq[T]:
      def length = len
      def apply(x: Int) = thisBuffer.apply(x)

    def toSeq: Seq = this.Seq()
    def find(p: T => Boolean): Option[T] = toSeq.find(p)
    def exists(p: T => Boolean): Boolean = toSeq.exists(p)
    def forall(p: T => Boolean): Boolean = toSeq.forall(p)
    def contains(x: T): Boolean = toSeq.contains(x)
    def reduceLeft(f: (T, T) => T): T = toSeq.reduceLeft(f)

  end Buffer

  class BufSet

  class ListSlice[T](xs: List[T], start: Int, end: Int) extends IndexedSeq[T]:
    val length = (end min xs.length) - (start max 0) max 0
    def apply(i: Int) = xs.apply(i + start)
    def tolist: List[T] = xs.slice(start, end)
    override def drop(n: Int) = xs.sliceToSeq((start + n) max start, end)
    override def take(n: Int) = xs.sliceToSeq(start, (start + n) min end)
    override def dropRight(n: Int) = xs.sliceToSeq(start, (end - n) min end)
    override def takeRight(n: Int) = xs.sliceToSeq((end - n) max start, end)
    override def fromSpecific(coll: IterableOnce[T]) = fromIterable(coll).toSeq

  class WithFilter[T](xs: List[T], p: T => Boolean):
    def zipped[U](ys: List[U]): LazyZip2[T, U] = zipped(ys.iterator)
    def zipped[U](ys: IterableOnce[U]): LazyZip2[T, U] =
      LazyZip2(xs, ys).withFilter((x, y) => p(x))

    def map[U](f: T => U): List[U] =
      val buf = Buffer[U].sizeHint(xs.length)
      foreach(x => buf += f(x))
      buf.toList

    def flatMap[U](f: T => List[U]): List[U] =
      val buf = Buffer[U]()
      foreach(x => buf ++= f(x))
      buf.toList

    def foreach(f: T => Unit) =
      xs.foreachInlined(x => if p(x) then f(x))

    def withFilter(q: T => Boolean): WithFilter[T] = new WithFilter(xs, x => p(x) && q(x))
  end WithFilter

  class LazyZip2[T, U](xs: List[T], ys: IterableOnce[U]):
    self =>

    def zipped[V](zs: List[V]): LazyZip3[T, U, V] = zipped(zs.iterator)
    def zipped[V](zs: IterableOnce[V]): LazyZip3[T, U, V] = new LazyZip3(xs, ys, zs):
      override def include(x: T, y: U, z: V) = self.include(x, y)

    def map[V](f: (T, U) => V): List[V] =
      val buf = Buffer[V]().sizeHint(xs.length)
      foreach((x, y) => buf += f(x, y))
      buf.toList

    def flatMap[V](f: (T, U) => List[V]): List[V] =
      val buf = Buffer[V]()
      foreach((x, y) => buf ++= f(x, y))
      buf.toList

    def foreach(f: (T, U) => Unit): Unit =
      val yit = ys.iterator
      xs.foreachInlined { x =>
        if yit.hasNext then
          val y = yit.next()
          if include(x, y) then f(x, y)
      }

    def exists(p: (T, U) => Boolean): Boolean =
      val xit = xs.iterator
      val yit = ys.iterator
      while xit.hasNext && yit.hasNext do
        if p(xit.next(), yit.next()) then return true
      false

    protected def include(x: T, y: U): Boolean = true

    def withFilter(p: (T, U) => Boolean): LazyZip2[T, U] = new LazyZip2[T, U](xs, ys):
      override def include(x: T, y: U) = self.include(x, y) && p(x, y)
  end LazyZip2

  class LazyZip3[T, U, V](xs: List[T], ys: IterableOnce[U], zs: IterableOnce[V]):
    self =>

    def map[W](f: (T, U, V) => W): List[W] =
      val buf = Buffer[W].sizeHint(xs.length)
      foreach((x, y, z) => buf += f(x, y, z))
      buf.toList

    def flatMap[W](f: (T, U, V) => List[W]): List[W] =
      val buf = Buffer[W]()
      foreach((x, y, z) => buf ++= f(x, y, z))
      buf.toList

    def foreach(f: (T, U, V) => Unit): Unit =
      val yit = ys.iterator
      val zit = zs.iterator
      xs.foreachInlined { x =>
        if yit.hasNext && zit.hasNext then
          val y = yit.next()
          val z = zit.next()
          if include(x, y, z) then f(x, y, z)
      }

    def exists(p: (T, U, V) => Boolean): Boolean =
      val xit = xs.iterator
      val yit = ys.iterator
      val zit = zs.iterator
      while xit.hasNext && yit.hasNext && zit.hasNext do
        if p(xit.next(), yit.next(), zit.next()) then return true
      false

    protected def include(x: T, y: U, z: V): Boolean = true

    def withFilter(p: (T, U, V) => Boolean): LazyZip3[T, U, V] = new LazyZip3[T, U, V](xs, ys, zs):
      override def include(x: T, y: U, z: V) = self.include(x, y, z) && p(x, y, z)
  end LazyZip3

  final class UnapplyLeftWrapper[+T](val xs: List[T]) extends AnyVal:
    def isEmpty = xs.isEmpty
    def get = UnapplyLeftResultWrapper(xs)

  final class UnapplyLeftResultWrapper[+T](xs: List[T]) extends AnyVal with Product:
    def canEqual(that: Any) = true
    def productArity = 2
    def productElement(n: Int): Any = if n == 0 then _1 else _2
    def _1: T = xs.head
    def _2: List[T] = xs.tail

  final class UnapplyRightWrapper[+T](val xs: List[T]) extends AnyVal:
    def isEmpty = xs.isEmpty
    def get = UnapplyRightResultWrapper(xs)

  final class UnapplyRightResultWrapper[+T](val xs: List[T]) extends AnyVal with Product:
    def canEqual(that: Any) = true
    def productArity = 2
    def productElement(n: Int): Any = if n == 0 then _1 else _2
    def _1: List[T] = xs.init
    def _2: T = xs.last

  object :: :
    def unapply[T](xs: List[T]): UnapplyLeftWrapper[T] = UnapplyLeftWrapper(xs)

  object +: :
    def unapply[T](xs: List[T]): UnapplyLeftWrapper[T] = UnapplyLeftWrapper(xs)

  object :+ :
    def unapply[T](xs: List[T]): UnapplyRightWrapper[T] = UnapplyRightWrapper(xs)

  final class UnapplySeqWrapper[T](private val xs: List[T]) extends AnyVal:
    def isEmpty: Boolean = xs.isEmpty
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = xs.length - len
    def apply(i: Int): T = xs(i)
    def drop(n: Int): Seq[T] = xs.sliceToSeq(n)
    def toSeq: Seq[T] = xs.toSeq

  def unapplySeq[T](xs: List[T]): UnapplySeqWrapper[T] = new UnapplySeqWrapper(xs)

end List

trait Show[-T]:
  extension (x: T) def show: String

trait Eq[-T]:
  extension (x: T)
    def === (y: T): Boolean
    def =/= (y: T): Boolean = !(x === y)

trait Hashed[-T]:
  extension (x: T) def hashcode: Int

given lstShow[T: Show] as Show[List[T]]:
  extension (xs: List[T]) def show: String =
    val b = new StringBuilder("List(")
    if xs.length != 0 then
      var i = 0
      while
        b ++= xs(i).show
        i += 1
        i < xs.length
      do
        b ++= ", "
    (b ++= ")").toString

given lstEq[T: Eq] as Eq[List[T]]:
  extension (xs: List[T]) def === (ys: List[T]): Boolean =
    (xs eqLst ys)
    || {
      var i = 0
      while i < xs.length && i < ys.length && xs(i) === ys(i) do
        i += 1
      i == xs.length && i == ys.length
    }

given lstHash[T: Hashed] as Hashed[List[T]]:
  extension (xs: List[T]) def hashcode: Int =
    xs.foldLeft(12345)((h, x) => h * 41 + x.hashcode) // should be improved

given anyShow[T] as Show[T]:
  extension (x: T) def show: String = String.valueOf(x)

given anyEq[T] as Eq[T]:
  extension (x: T) def === (y: T): Boolean = x == y

given anyHashed[T] as Hashed[T]:
  extension (x: T) def hashcode = x.hashCode
