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
 *    If it is empty:                    the value `Lst.NIL`
 *    If it contains one element,
 *    and the element is not an array:   the element itself
 *    Otherwise:                         an Array[Any] containing the elements
 */

type Lst[+T] = Lst.Vault.Lst[T]
object Lst:

  type Arr = Array[Any]

  /** Lsts can only be compared with other Lsts... */
  given lstEql[T, U] as Eql[Lst[T], Lst[U]] = Eql.derived

  /** ... except that locally we can also match with `null` */
  private given lstNullEq[T] as Eql[Null, Lst[T]] = Eql.derived

  object Vault:
    opaque type Lst[+T] = Any

    private[Lst] def NIL: Lst[Nothing] = null

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
      if xs.length == 0 then NIL
      else if xs.length == 1 then single(xs(0).asInstanceOf[T])
      else xs

    /** Create a list directly from the given array. Do not convert arrays of size <= 1.
     */
    private[Lst] def fromArr[T](xs: Arr): Lst[T] =
      xs
  end Vault

  val NIL = Vault.NIL
  import Vault.{single, multi, fromArr}

  private inline def eq(x: Any, y: Any) = x.asInstanceOf[AnyRef] `eq` y.asInstanceOf[AnyRef]

  def apply[T](): Lst[T] = NIL

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
    if len <= 0 then NIL
    else if len == 1 then single[T](xs(start).asInstanceOf[T])
    else if start == 0 && end == xs.length then fromArr(xs)
    else
      val newElems = new Arr(len)
      System.arraycopy(xs, start, newElems, 0, len)
      fromArr(newElems)


  def fromArray[T](xs: Array[T], start: Int, end: Int): Lst[T] =
    _fromArray(xs.asInstanceOf[Arr], start, end)

  def fromArray[T](xs: Array[T]): Lst[T] =
    fromArray(xs, 0, xs.length)

  def fromIterable[T](it: IterableOnce[T]): Lst[T] =
    val buf = new Buffer[T]
    it.iterator.foreach(buf += _)
    buf.toLst

  def fromSeq[T](seq: Seq[T]): Lst[T] =
    val elems = new Arr(seq.length)
    seq.copyToArray(elems)
    multi[T](elems)

  extension [T](xs: Lst[T] & Arr)
    private def at(i: Int): T = (xs: Arr)(i).asInstanceOf[T]

  extension[T](xs: Lst[T]):

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

    def copyToArray(target: Array[T], from: Int) = xs match
      case null =>
      case xs: Arr => System.arraycopy(xs, 0, target, from, xs.length)
      case x: T @ unchecked => target(from) = x

    def toSet: collection.immutable.Set[T] =
      var xs = Set[T]()
      foreachInlined(xs += _)
      xs

    def toList: List[T] =
      if length == 0 then Nil
      else
        val buf = new collection.mutable.ListBuffer[T]
        foreachInlined(buf += _)
        buf.toList

    def toListReversed: List[T] =
      var result: List[T] = Nil
      foreachInlined(x => result = x :: result)
      result

    def toIterable: Iterable[T] = new Iterable[T]:
      def iterator = xs.iterator

    def toSeq: LstSlice[T] = sliceToSeq(0, length)

    def sliceToSeq(start: Int = 0, end: Int = xs.length): LstSlice[T] = LstSlice[T](xs, start, end)

    def filter(p: T => Boolean): Lst[T] = xs match
      case null => NIL
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
      case x: T @unchecked => if (p(x)) xs else NIL

    def filterNot(p: T => Boolean): Lst[T] = filter(!p(_))

    def withFilter(p: T => Boolean) = WithFilter(xs, p)

    def partition(p: T => Boolean): (Lst[T], Lst[T]) = xs match
      case null => (NIL, NIL)
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
        if hitCount == xs.length then (xs, NIL)
        else if missCount == xs.length then (NIL, xs)
        else (_fromArray(hits, 0, hitCount), _fromArray(misses, 0, missCount))
      case x: T @unchecked =>
        if (p(x)) (xs, NIL) else (NIL, xs)

    def span(p: T => Boolean): (Lst[T], Lst[T]) = xs match
      case null => (NIL, NIL)
      case xs: Arr =>
        var i = 0
        while i < xs.length && p(xs.at(i)) do i += 1
        if i == xs.length then (xs, NIL)
        else if i == 0 then (NIL, xs)
        else (_fromArray(xs, 0, i), _fromArray(xs, i, xs.length))
      case x: T @unchecked =>
        if p(x) then (single[T](x), NIL) else (NIL, single[T](x))

    def splitAt(n: Int): (Lst[T], Lst[T]) = (slice(0, n), slice(n, length))

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

    def slice(start: Int, end: Int): Lst[T] =
      if start < 0 then slice(0, end)
      else xs match
        case null => xs
        case xs: Arr => _fromArray(xs, start, end min xs.length)
        case x: T @ unchecked => if start == 0 && end > 0 then xs else NIL

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

    def indexWhere(p: T => Boolean): Int =
      val idx = firstIndexWhere(p)
      if idx == length then -1 else idx

    def find(p: T => Boolean): Option[T] =
      val idx = firstIndexWhere(p)
      if idx < length then Some(xs.apply(idx)) else None

    def takeWhile(p: T => Boolean): Lst[T] = take(firstIndexWhere(!p(_)))
    def dropWhile(p: T => Boolean): Lst[T] = drop(firstIndexWhere(!p(_)))

    def reduceLeft(op: (T, T) => T) = xs match
      case null =>
        throw new UnsupportedOperationException("Lst.NIL.reduceLeft")
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

    def ::: (ys: Lst[T]): Lst[T] = xs ++ ys

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

    def ++ (ys: IterableOnce[T]): Lst[T] =
      val yit = ys.iterator
      if yit.isEmpty then xs
      else if xs.isEmpty then fromIterable(yit)
      else (Buffer[T]() ++= xs ++= yit).toLst

    def :+ (x: T): Lst[T] = xs match
      case null => Lst(x)
      case xs: Arr =>
        val newElems = new Arr(xs.length + 1)
        xs.copyToArray(newElems, 0)
        newElems(xs.length) = x
        multi[T](newElems)
      case elem: T @unchecked =>
        Lst(elem, x)

    def updated(idx: Int, x: T): Lst[T] =
      xs match
        case xs: Arr => multi[T]((xs: Arr).updated(idx, x))
        case elem: T @unchecked if idx == 0 => single[T](x)
        case _ => throw IndexOutOfBoundsException(idx.toString)

    def union(ys: Lst[T]): Lst[T] = xs ++ ys.filterNot(xs.contains(_))
    def intersect(ys: Lst[T]): Lst[T] = xs.filter(ys.contains(_))

    def zipWithIndex: Lst[(T, Int)] = xs match
      case null => NIL
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
      case null => NIL
      case xs: Arr =>
        val newElems: Arr = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          newElems(i) = i
          i += 1
        multi[Int](newElems)
      case elem =>
        single[Int](0)

  extension [T: ClassTag, U: math.Ordering](xs: Lst[T])
    def sortBy(f: T => U): Lst[T] = fromArray(xs.toArray.sortBy(f))

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
        case null => NIL
        case xs: Arr =>
          val newElems = new Arr(xs.length)
          var i = 0
          while i < xs.length do
            newElems(i) = op(xs.at(i))
            i += 1
          multi[U](newElems)
        case x: T @ unchecked => single[U](op(x))

    def mapConserve(f: T => U): Lst[U] = xs match
      case null => NIL
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
      case null => NIL
      case xs: Arr =>
        val newElemss = new Array[Lst[U]](xs.length)
        var i = 0
        var len = 0
        while i < xs.length do
          val ys = f(xs.at(i))
          len += ys.length
          newElemss(i) = ys
          i += 1
        if len == 0 then NIL
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

    def flatMapIterable(f: T => IterableOnce[U]): Lst[U] =
      flatMap(x => fromIterable(f(x)))

    def collect(pf: PartialFunction[T, U]): Lst[U] =
      val buf = new Buffer[U](xs.length)
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

    def zipped(ys: Lst[U]) = LazyZip2(xs, ys.iterator)
    def zipped(ys: IterableOnce[U]) = LazyZip2(xs, ys)

    def zip(ys: Lst[U]): Lst[(T, U)] = xs.zipWith(ys)((_, _))
    def zip(ys: IterableOnce[U]): Lst[(T, U)] = xs.zipWith(ys)((_, _))

    @infix def eqLst(ys: Lst[U]) = eq(xs, ys)

    inline def corresponds(ys: Lst[U])(p: (T, U) => Boolean): Boolean =
      xs.length == ys.length
      && {
        var i = 0
        while i < xs.length && p(xs(i), ys(i)) do i += 1
        i == xs.length
      }

    def eqElements(ys: Lst[U]): Boolean =
      (xs `eqLst` ys) || corresponds(ys)(eq(_, _))

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
    def +: (xs: Lst[T]): Lst[T] = x :: xs

  extension [T](xs: Iterable[T])
    def toLst: Lst[T] =
      if xs.isEmpty then NIL
      else (xs: IterableOnce[T]).toLst

  extension [T](xs: IterableOnce[T])
    def toLst: Lst[T] = (Buffer[T]() ++= xs).toLst

    def ::: (ys: Lst[T]): Lst[T] =
      val xit = xs.iterator
      if xit.isEmpty then ys
      else if ys.isEmpty then fromIterable(xit)
      else (Buffer[T]() ++= xit ++= ys).toLst

  extension [T, U](z: T)
    inline def /: (xs: Lst[U])(inline f: (T, U) => T): T =
      xs.foldLeft(z)(f)

  extension [T, U, V](xs: Lst[T]):
    def zipWith(ys: Lst[U])(op: (T, U) => V): Lst[V] = xs match
      case null => NIL
      case xs: Arr =>
        ys match
          case null => NIL
          case ys: Arr =>
            val len = xs.length min ys.length
            if len == 0 then NIL
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
              _fromArray[V](if newElems == null then xs else newElems, 0, i)
          case y: U @unchecked =>
            single[V](op(xs.at(0), y))
      case x: T @unchecked =>
        ys match
          case null => NIL
          case ys: Arr => single[V](op(x, ys.at(0)))
          case y: U @unchecked => single[V](op(x, y))

    def zipWith(ys: IterableOnce[U])(op: (T, U) => V): Lst[V] =
      val yit = ys.iterator
      xs match
        case null => NIL
        case xs: Arr =>
          var newElems: Arr = null
          var i = 0
          while i < xs.length && yit.hasNext do
            val x = xs.at(i)
            val y = op(x, yit.next)
            if newElems != null then
              newElems(i) = y
            else if !eq(x, y) then
              newElems = new Arr(xs.length)
              System.arraycopy(xs, 0, newElems, 0, i)
              newElems(i) = y
            i += 1
          _fromArray[V](if newElems == null then xs else newElems, 0, i)
        case x: T @unchecked =>
          if yit.hasNext then single[V](op(x, yit.next))
          else NIL
  end extension

  extension [T, U, V, W](xs: Lst[T]):
    def zipWith(ys: Lst[U], zs: Lst[V])(op: (T, U, V) => W): Lst[W] =
      zipWith(ys.iterator, zs.iterator)(op)
    def zipWith(ys: IterableOnce[U], zs: IterableOnce[V])(op: (T, U, V) => W): Lst[W] =
      val yit = ys.iterator
      val zit = zs.iterator
      xs match
        case null => NIL
        case xs: Arr =>
          var newElems: Arr = null
          var i = 0
          while i < xs.length && yit.hasNext && zit.hasNext do
            val x = xs.at(i)
            val y = op(x, yit.next, zit.next)
            if newElems != null then
              newElems(i) = y
            else if !eq(x, y) then
              newElems = new Arr(xs.length)
              System.arraycopy(xs, 0, newElems, 0, i)
              newElems(i) = y
            i += 1
          _fromArray[W](if newElems == null then xs else newElems, 0, i)
        case x: T @unchecked =>
          if yit.hasNext && zit.hasNext then single[W](op(x, yit.next, zit.next))
          else NIL

  extension [T, U] (xs: Lst[(T, U)])
    def toMap: Map[T, U] = Map() ++ xs.iterator

    def unzip: (Lst[T], Lst[U]) = xs match
      case null => (Lst.NIL, Lst.NIL)
      case xs: Arr =>
        val fst, snd = new Arr(xs.length)
        var i = 0
        while i < xs.length do
          val p = xs.at(i)
          fst(i) = p._1
          snd(i) = p._2
          i += 1
        (multi[T](fst), multi[U](snd))
      case x: (T, U) @unchecked =>
        (single[T](x._1), single[U](x._2))
  end extension

  extension [T](xs: Lst[Iterable[T]])
    def flatten: Lst[T] =
      val buf = Buffer[T]()
      xs.foreach(buf ++= _)
      buf.toLst

  extension [T](xs: Iterable[Lst[T]])
    def flattenLst: Lst[T] =
      val buf = Buffer[T]()
      xs.foreach(buf ++= _)
      buf.toLst

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

  class Buffer[T](minArraySize: Int = 16):
    private var len = 0
    private var elem: T = _
    private var elems: Arr = _

    def size = len

    /** pre: len > 0, n >= 1 */
    private def ensureSize(n: Int): Unit =
      if len < n then
        if len == 1 then
          elems = new Arr(n max minArraySize)
          elems(0) = elem
        else
          val newLen = n max len * 2
          val newElems = new Arr(newLen)
          System.arraycopy(elems, 0, newElems, 0, len)
          elems = newElems

    def isEmpty = size == 0
    def nonEmpty = size != 0

    def += (x: T): this.type =
      if len == 0 then elem = x
      else
        ensureSize(len + 1)
        elems(len) = x
      len += 1
      this

    def ++= (xs: Lst[T]): this.type = appendSlice(xs, 0, xs.length)

    def appendSlice(xs: Lst[T], start: Int, end: Int): this.type =
      val copiedLength = (end - start) min xs.length
      if copiedLength > 1 then
        xs match
          case xs: Arr =>
            if len == 0 then
              elems = (xs: Arr).slice(start, end)
            else
              ensureSize(len + copiedLength)
              System.arraycopy(xs, 0, elems, len, copiedLength)
        len += copiedLength
      else if copiedLength == 1 then
        this += xs(start)
      this

    def ++= (xs: IterableOnce[T]): this.type =
      xs.iterator.foreach(this += _)
      this

    def find(p: T => Boolean): Option[T] =
      if len == 0 then None
      else if len == 1 then
        if p(elem) then Some(elem) else None
      else
        var i = 0
        while i < len && !p(elems(i).asInstanceOf[T]) do i += 1
        if i < len then Some(elems(i).asInstanceOf[T]) else None

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

    def apply(idx: Int): T =
      if 0 <= idx && idx < len then
        if len == 1 then elem
        else elems(idx).asInstanceOf[T]
      else throw IndexOutOfBoundsException(idx.toString)

    def head = apply(0)
    def last = apply(len - 1)

    def mapInPlace(f: T => T): this.type =
      if len == 1 then
        elem = f(elem)
      else if len > 1 then
        var i = 0
        while i < len do
          elems(i) = f(elems(i).asInstanceOf[T])
          i += 1
      this

    def trimEnd(n: Int) =
      val remaining = (len - n) max 0
      if len > 1 && remaining <= 1 then
        if remaining == 1 then elem = elems(0).asInstanceOf[T]
        elems = null
      len = remaining

    def toLst: Lst[T] =
      if len == 0 then NIL
      else if len == 1 then single(elem)
      else _fromArray(elems, 0, len)

    def clear() =
      len = 0
  end Buffer

  class WithFilter[T](xs: Lst[T], p: T => Boolean):
    def zipped[U](ys: Lst[U]): LazyZip2[T, U] = zipped(ys.iterator)
    def zipped[U](ys: IterableOnce[U]): LazyZip2[T, U] =
      LazyZip2(xs, ys).withFilter((x, y) => p(x))

    def map[U](f: T => U): Lst[U] =
      val buf = Buffer[U](xs.length)
      foreach(x => buf += f(x))
      buf.toLst

    def flatMap[U](f: T => Lst[U]): Lst[U] =
      val buf = Buffer[U]()
      foreach(x => buf ++= f(x))
      buf.toLst

    def foreach(f: T => Unit) =
      xs.foreachInlined(x => if p(x) then f(x))

    def withFilter(q: T => Boolean): WithFilter[T] = new WithFilter(xs, x => p(x) && q(x))
  end WithFilter

  class LazyZip2[T, U](xs: Lst[T], ys: IterableOnce[U]):
    self =>

    def zipped[V](zs: Lst[V]): LazyZip3[T, U, V] = zipped(zs.iterator)
    def zipped[V](zs: IterableOnce[V]): LazyZip3[T, U, V] = new LazyZip3(xs, ys, zs):
      override def include(x: T, y: U, z: V) = self.include(x, y)

    def map[V](f: (T, U) => V): Lst[V] =
      val buf = Buffer[V](xs.length)
      foreach((x, y) => buf += f(x, y))
      buf.toLst

    def flatMap[V](f: (T, U) => Lst[V]): Lst[V] =
      val buf = Buffer[V]()
      foreach((x, y) => buf ++= f(x, y))
      buf.toLst

    def foreach(f: (T, U) => Unit): Unit =
      val yit = ys.iterator
      xs.foreachInlined { x =>
        if yit.hasNext then
          val y = yit.next()
          if include(x, y) then f(x, y)
      }

    protected def include(x: T, y: U): Boolean = true

    def withFilter(p: (T, U) => Boolean): LazyZip2[T, U] = new LazyZip2[T, U](xs, ys):
      override def include(x: T, y: U) = self.include(x, y) && p(x, y)
  end LazyZip2

  class LazyZip3[T, U, V](xs: Lst[T], ys: IterableOnce[U], zs: IterableOnce[V]):
    self =>

    def map[W](f: (T, U, V) => W): Lst[W] =
      val buf = Buffer[W](xs.length)
      foreach((x, y, z) => buf += f(x, y, z))
      buf.toLst

    def flatMap[W](f: (T, U, V) => Lst[W]): Lst[W] =
      val buf = Buffer[W]()
      foreach((x, y, z) => buf ++= f(x, y, z))
      buf.toLst

    def foreach(f: (T, U, V) => Unit): Unit =
      val yit = ys.iterator
      val zit = zs.iterator
      xs.foreachInlined { x =>
        if yit.hasNext && zit.hasNext then
          val y = yit.next()
          val z = zit.next()
          if include(x, y, z) then f(x, y, z)
      }

    protected def include(x: T, y: U, z: V): Boolean = true

    def withFilter(p: (T, U, V) => Boolean): LazyZip3[T, U, V] = new LazyZip3[T, U, V](xs, ys, zs):
      override def include(x: T, y: U, z: V) = self.include(x, y, z) && p(x, y, z)
  end LazyZip3

  final class UnapplyLeftWrapper[T](val xs: Lst[T]) extends AnyVal with Product:
    def canEqual(that: Any) = true
    def isEmpty = xs.isEmpty
    def productArity = 2
    def productElement(n: Int): Any = if n == 0 then _1 else _2
    def _1: T = xs.head
    def _2: Lst[T] = xs.tail

  final class UnapplyRightWrapper[T](val xs: Lst[T]) extends AnyVal with Product:
    def canEqual(that: Any) = true
    def isEmpty = xs.isEmpty
    def productArity = 2
    def productElement(n: Int): Any = if n == 0 then _1 else _2
    def _1: Lst[T] = xs.init
    def _2: T = xs.last

  object +: :
    def unapply[T](xs: Lst[T]): UnapplyLeftWrapper[T] = UnapplyLeftWrapper(xs)

  object :+ :
    def unapply[T](xs: Lst[T]): UnapplyRightWrapper[T] = UnapplyRightWrapper(xs)

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

  class LstSlice[T](xs: Lst[T], start: Int, end: Int) extends IndexedSeq[T]:
    val length = (end min xs.length) - (start max 0) max 0
    def apply(i: Int) = xs.apply(i + start)
    def toLst: Lst[T] = xs.slice(start, end)
    override def drop(n: Int) = xs.sliceToSeq(start + n, end)
    override def take(n: Int) = xs.sliceToSeq(start, (start + n) min end)
    override def dropRight(n: Int) = xs.sliceToSeq(start, end - n)
    override def takeRight(n: Int) = xs.sliceToSeq((end - n) max start, end)
    override def fromSpecific(coll: IterableOnce[T]) = fromIterable(coll).toSeq

  def unapplySeq[T](xs: Lst[T]): UnapplySeqWrapper[T] = new UnapplySeqWrapper(xs)

  final class UnapplySeqWrapper[T](private val xs: Lst[T]) extends AnyVal:
    def isEmpty: Boolean = xs.isEmpty
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = xs.length - len
    def apply(i: Int): T = xs(i)
    def drop(n: Int): Seq[T] = xs.sliceToSeq(n)
    def toSeq: Seq[T] = xs.toSeq

// -------- Temporary poison pill to detect mistakes when matching Lst selectors against List patterns

  object :: :
    def unapply[T](xs: List[T]): Option[(T, List[T])] = xs match
      case xs : ::[_] => Some((xs.head, xs.tail))
      case _ => None

    def unapply[T](xs: Lst[T]): Option[(Impossible, Impossible)] = None
  end ::
  final class Impossible
end Lst
