package dotty.tools
package dotc
package util

import collection.mutable.{ListBuffer, StringBuilder}
import collection.immutable.Map
import reflect.ClassTag
import annotation.infix

/** A lightweight class for lists, optimized for short and medium lengths.
 *  A list is represented at runtime as
 *
 *    If it is empty:                    the value `Lst.Empty`
 *    If it contains one element,
 *    and the element is not an array:   the element itself
 *    Otherwise:                         an Array[Any] containing the elements
 */

type Lst[+T] = Lst.Vault.Lst[T]
object Lst:

  private type Arr = Array[Any]

  object Vault:
    opaque type Lst[+T] = Any

    private[Lst] def Empty: Lst[Nothing] = null

    /** Create a list of length 1 from given element `x` */
    private[Lst] def single[T](x: T): Lst[T] = x match
      case null | _: Arr =>
        val wrapped = new Arr(1)
        wrapped(0) = x
        wrapped
      case _ =>
        x

    /** Create a list of length `xs.length` from the elements in `xs`
     *  @pre  xs is not empty
     */
    private[Lst] def multi[T](xs: Arr): Lst[T] =
      if xs.length == 0 then Empty
      else if xs.length == 1 then single(xs(0).asInstanceOf[T])
      else xs

    /** Create a list directly from the given array. Do not convert arrays of size <= 1.
     */
    private[Lst] def fromArr[T](xs: Arr): Lst[T] =
      xs
  end Vault

  val Empty = Vault.Empty
  import Vault.{single, multi, fromArr}

  private inline def eq(x: Any, y: Any) = x.asInstanceOf[AnyRef] `eq` y.asInstanceOf[AnyRef]
  private val eqFn = (x: Any, y: Any) => eq(x, y)

  def apply[T](): Lst[T] = Empty

  def apply[T](x0: T): Lst[T] = single[T](x0)

  def apply[T](x0: T, x1: T): Lst[T] =
    val xs = new Arr(2)
    xs(0) = x0
    xs(1) = x1
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T): Lst[T] =
    val xs = new Arr(3)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T, x3: T): Lst[T] =
    val xs = new Arr(4)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    xs(3) = x3
    fromArr[T](xs)

  def apply[T](x0: T, x1: T, x2: T, x3: T, x4: T, others: T*): Lst[T] =
    val xs = new Arr(5 + others.length)
    xs(0) = x0
    xs(1) = x1
    xs(2) = x2
    xs(3) = x3
    xs(4) = x4
    others.copyToArray(xs, 5)
    fromArr[T](xs)


  private def _fromArray[T](xs: Arr, start: Int, end: Int): Lst[T] =
    val len = end - start
    if len <= 0 then Empty
    else if len == 1 then single[T](xs(start).asInstanceOf[T])
    else if start == 0 && end == xs.length then fromArr(xs)
    else
      val newElems = new Arr(len)
      System.arraycopy(xs, start, newElems, 0, len)
      fromArr(newElems)


  def fromArray[T](xs: Array[T], start: Int, end: Int): Lst[T] =
    _fromArray(xs.asInstanceOf[Arr], start, end)

  def fromIterator[T](it: Iterator[T]): Lst[T] =
    val buf = new Buffer[T]
    it.foreach(buf += _)
    buf.toLst

  def fromIterable[T](xs: Iterable[T]): Lst[T] = fromIterator(xs.iterator)

  extension [T](xs: Lst[T] & Arr)
    private def at(i: Int): T = (xs: Arr)(i).asInstanceOf[T]

  extension[T](xs: Lst[T]):

    def length: Int = xs match
      case null => 0
      case xs: Arr => xs.length
      case x => 1

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
      case xs: Arr => xs.iterator.asInstanceOf[Iterator[T]]
      case x: T @unchecked => Iterator.single(x)

    def copyToArray(target: Array[T], from: Int) = xs match
      case null =>
      case xs: Arr => System.arraycopy(xs, 0, target, from, xs.length)
      case x: T @ unchecked => target(from) = x

    def toSet: collection.immutable.Set[T] =
      var xs = Set[T]()
      foreachInlined(xs += _)
      xs

    def toList: List[T] =
      val buf = new collection.mutable.ListBuffer[T]
      foreachInlined(buf += _)
      buf.toList

    def toListReversed: List[T] =
      var result: List[T] = Nil
      foreachInlined(x => result = x :: result)
      result

    def toIterable: Iterable[T] = new Iterable[T]:
      def iterator = xs.iterator

    def filter(p: T => Boolean): Lst[T] = xs match
      case null => Empty
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
      case x: T @unchecked => if (p(x)) xs else Empty

    def filterNot(p: T => Boolean): Lst[T] = filter(!p(_))

    def withFilter(p: T => Boolean): Lst[T] = filter(p)

    def partition(p: T => Boolean): (Lst[T], Lst[T]) = xs match
      case null => (Empty, Empty)
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
        if hitCount == xs.length then (xs, Empty)
        else if missCount == xs.length then (Empty, xs)
        else (_fromArray(hits, 0, hitCount), _fromArray(misses, 0, missCount))
      case x: T @unchecked =>
        if (p(x)) (xs, Empty) else (Empty, xs)

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

    def contains(x: T): Boolean = xs match
      case null => false
      case xs: Arr =>
        var i = 0
        while i < xs.length && xs.at(i) != x do i += 1
        i < xs.length
      case elem: T @unchecked =>
        elem == x

    def reverse: Lst[T] = xs match
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
        throw new IndexOutOfBoundsException(n.toString)
      case xs: Arr =>
        xs.at(n)
      case x: T @unchecked =>
        if n == 0 then x else throw new IndexOutOfBoundsException(n.toString)

    def head: T = xs.apply(0)
    def last: T = xs.apply(length - 1)

    inline def headOr(inline alt: T): T = if isEmpty then alt else head

    def slice(start: Int, end: Int): Lst[T] =
      if start < 0 then slice(0, end)
      else xs match
        case null => xs
        case xs: Arr => _fromArray(xs, start, end min xs.length)
        case x: T @ unchecked => if start == 0 && end > 0 then xs else Empty

    def drop(n: Int): Lst[T] = slice(n, length)
    def tail: Lst[T] = drop(1)
    def take(n: Int): Lst[T] = slice(0, n)

    def dropRight(n: Int): Lst[T] = take(length - n)
    def takeRight(n: Int): Lst[T] = drop(length - n)
    def init: Lst[T] = dropRight(1)

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

    def find(p: T => Boolean): Option[T] =
      val idx = firstIndexWhere(p)
      if idx < length then Some(xs.apply(idx)) else None

    def takeWhile(p: T => Boolean): Lst[T] = take(firstIndexWhere(!p(_)))
    def dropWhile(p: T => Boolean): Lst[T] = drop(firstIndexWhere(!p(_)))

    def reduceLeft(op: (T, T) => T) = xs match
      case null =>
        throw new UnsupportedOperationException("Lst.Empty.reduceLeft")
      case xs: Arr =>
        var i = 1
        var acc = xs.at(0)
        while i < xs.length do
          acc = op(acc, xs.at(i))
          i += 1
        acc
      case x: T @unchecked =>
        x

    def ++ (ys: Lst[T]): Lst[T] =
      if xs.isEmpty then ys
      else if ys.isEmpty then xs
      else
        val len1 = xs.length
        val len2 = ys.length
        val newElems = new Arr(len1 + len2)
        xs.copyToArray(newElems, 0)
        ys.copyToArray(newElems, len1)
        multi[T](newElems)

    def :+ (x: T): Lst[T] = xs match
      case null => Lst(x)
      case xs: Arr =>
        val newElems = new Arr(xs.length + 1)
        xs.copyToArray(newElems, 0)
        newElems(xs.length) = x
        multi[T](newElems)
      case elem: T @unchecked =>
        Lst(elem, x)

    def union(ys: Lst[T]): Lst[T] = xs ++ ys.filterNot(xs.contains(_))
    def intersect(ys: Lst[T]): Lst[T] = xs.filter(ys.contains(_))

    def zipWithIndex: Lst[(T, Int)] = xs match
      case null => Empty
      case xs: Arr =>
        val newElems = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(i) = (xs.at(i), i)
          i += 1
        multi[(T, Int)](newElems)
      case x: T @unchecked =>
        single[(T, Int)]((x, 0))

    def indices: Lst[Int] = xs match
      case null => Empty
      case xs: Arr =>
        val newElems: Arr = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(i) = i
          i += 1
        multi[Int](newElems)
      case elem =>
        single[Int](0)

  extension [T: ClassTag](xs: Lst[T]):
    def toArray: Array[T] =
      val result = new Array[T](xs.length)
      xs.copyToArray(result, 0)
      result

  extension [T, U](xs: Lst[T]):

    /** `f` is pulled out, not duplicated */
    inline def map(inline f: T => U): Lst[U] =
      def op(x: T) = f(x)
      xs match
        case null => Empty
        case xs: Arr =>
          val newElems = new Arr(xs.length)
          var i = 0
          while i < xs.length do
            newElems(i) = op(xs.at(i))
            i += 1
          multi[U](newElems)
        case x: T @ unchecked => single[U](op(x))

    def mapConserve(f: T => U): Lst[U] = xs match
      case null => Empty
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
        if newElems == null then xs.asInstanceOf[Lst[U]] else multi[U](newElems)
      case x: T @ unchecked => single[U](f(x))

    def flatMap(f: T => Lst[U]): Lst[U] = xs match
      case null => Empty
      case xs: Arr =>
        val newElemss = new Array[Lst[U]](xs.length)
        var i = 0
        var len = 0
        while i < xs.length do
          val ys = f(xs.at(i))
          len += ys.length
          newElemss(i) = ys
          i += 1
        if len == 0 then Empty
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

    def flatMap(f: T => Iterable[U], dummy: Null = null): Lst[U] =
      flatMap(x => fromIterable(f(x)))

    def collect(pf: PartialFunction[T, U]): Lst[U] =
      val buf = new Buffer[U]
      xs.foreach(x => if pf.isDefinedAt(x) then buf += pf(x))
      buf.toLst

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

    def zip(ys: Lst[U]): Lst[(T, U)] = xs.zipWith(ys)((_, _))

    @infix def eqLst(ys: Lst[U]) = eq(xs, ys)

    def corresponds(ys: Lst[U])(p: (T, U) => Boolean): Boolean =
      (xs `eqLst` ys)
      || xs.match
          case null =>
            ys.isEmpty
          case xs: Arr =>
            ys match
              case ys: Arr =>
                val len = xs.length
                len == ys.length
                && {
                  var i = 0
                  while i < len && p(xs.at(i), ys.at(i)) do i += 1
                  i == len
                }
              case y: U @unchecked =>
                xs.length == 1 && p(xs.at(0), y)
          case x: T @unchecked =>
            ys match
              case null => false
              case ys: Arr => ys.length == 1 && p(x, ys.at(0))
              case y: U @unchecked => p(x, y)

    def eqElements(ys: Lst[U]): Boolean = corresponds(ys)(eqFn)

  extension [T](x: T)
    def :: (xs: Lst[T]): Lst[T] = xs match
      case null => Lst(x)
      case xs: Arr =>
        val newElems = new Arr(1 + xs.length)
        newElems(0) = x
        xs.copyToArray(newElems, 1)
        multi[T](newElems)
      case elem: T @unchecked =>
        Lst(x, elem)

  extension [T, U](z: T)
    inline def /: (xs: Lst[U])(inline f: (T, U) => T): T =
      xs.foldLeft(z)(f)

  extension [T, U, V](xs: Lst[T]):
    def zipWith(ys: Lst[U])(op: (T, U) => V): Lst[V] = xs match
      case null => Empty
      case xs: Arr =>
        ys match
          case null => Empty
          case ys: Arr =>
            val len = xs.length min ys.length
            if len == 0 then Empty
            else if len == 1 then single[V](op(xs.at(0), ys.at(0)))
            else
              var newElems: Arr = null
              var i = 0
              while i < len do
                val x = xs.at(i)
                val y = op(x, ys.at(i))
                if newElems != null then
                  newElems(i) = y
                else if !eq(x, y) then
                  newElems = new Arr(len)
                  System.arraycopy(xs, 0, newElems, 0, i)
                  newElems(i) = y
                i += 1
              if newElems == null then fromArr[V](xs)
              else multi[V](newElems)
          case y: U @unchecked =>
            single[V](op(xs.at(0), y))
      case x: T @unchecked =>
        ys match
          case null => Empty
          case ys: Arr => single[V](op(x, ys.at(0)))
          case y: U @unchecked => single[V](op(x, y))
  end extension

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

  def range(from: Int, until: Int): Lst[Int] =
    val xs = new Arr(until - from)
    var i = from
    while i < until do
      xs(i) = i
      i += 1
    multi[Int](xs)

  class Buffer[T]:
    private var len = 0
    private var elem: T = _
    private var elems: Arr = _

    def size = len

    /** pre: len > 0, n >= 1 */
    private def ensureSize(n: Int): Unit =
      if len == 1 then
        elems = new Arr(n max 16)
        elems(0) = elem
      else if len < n then
        val newLen = n max len * 2
        val newElems = new Arr(newLen)
        System.arraycopy(elems, 0, newElems, 0, len)
        elems = newElems

    def isEmpty = size == 0

    def += (x: T): this.type =
      if len == 0 then elem = x
      else
        ensureSize(len + 1)
        elems(len) = x
      len += 1
      this

    def ++= (xs: Lst[T]): this.type =
      xs match
        case null =>
        case xs: Arr =>
          if len == 0 && xs.length != 1 then
            // if xs.length == 1, xs is a wrapped single element list, which has to remain packed
            elems = xs
          else
            ensureSize(len + xs.length)
            System.arraycopy(xs, 0, elems, len, xs.length)
          len += xs.length
        case x: T @unchecked => this += x
      this

    def exists(p: T => Boolean): Boolean =
      if len == 0 then false
      else if len == 1 then p(elem)
      else
        var i = 0
        while i < len && !p(elems(i).asInstanceOf[T]) do i += 1
        i < len

    def forall(p: T => Boolean): Boolean = !exists(!p(_))

    def contains(x: T): Boolean =
      if len == 0 then false
      else if len == 1 then elem == x
      else
        var i = 0
        while i < len && elems(i) != x do i += 1
        i < len

    def toLst: Lst[T] =
      if len == 0 then Empty
      else if len == 1 then single(elem)
      else _fromArray(elems, 0, len)

    def clear() =
      len = 0
  end Buffer

  object :: :
    def unapply[T](xs: Lst[T]): Option[(T, Lst[T])] = xs match
      case null => None
      case xs: Arr =>
        Some((xs(0).asInstanceOf[T], _fromArray[T](xs, 1, xs.length)))
      case x: T @unchecked =>
        Some((x, Empty))
  end ::

  trait Show[T]:
    extension (x: T) def show: String

  given lstShow[T: Show] as Show[Lst[T]]:
    extension (xs: Lst[T]) def show: String =
      val b = new StringBuilder("Lst(")
      xs match
        case null =>
        case xs: Arr =>
          var i = 0
          while
            b ++= xs.at(i).show
            i += 1
            i < xs.length
          do
            b ++= ", "
        case x: T @unchecked =>
          b ++= summon[Show[T]].extension_show(x)
      (b ++= ")").toString

  given anyShow[T] as Show[T]:
    extension (x: T) def show: String = String.valueOf(x)

  trait Eq[T]:
    extension (x: T) def === (y: T): Boolean

  given lstEq[T: Eq] as Eq[Lst[T]]:
    extension (xs: Lst[T]) def === (ys: Lst[T]): Boolean =
      (xs eqLst ys)
      || xs.match
          case xs: Arr =>
            ys match
              case ys: Arr =>
                val len = xs.length
                len == ys.length
                && {
                  var i = 0
                  while i < len && xs.at(i) === ys.at(i) do i += 1
                  i == len
                }
              case _ => false
          case x => x.equals(ys)

  given anyEq[T] as Eq[T]:
    extension (x: T) def === (y: T): Boolean = x == y

  extension [T: Show](xs: Lst[T])
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

  extension (x: Any)
    def debugString: String = x match
      case x: Arr => x.map(_.debugString).mkString("Array(", ", ", ")")
      case _ => String.valueOf(x)

end Lst