package dotty.tools

import collection.mutable.{StringBuilder, ListBuffer}
import collection.immutable.{Map, List => sList, Nil => sNil}
import reflect.ClassTag
import annotation.{infix, tailrec}

/** A lightweight class for lists, optimized for short and medium lengths.
 *  A list is represented at runtime as
 *
 *    If it is empty:                    the value `List.Nil`
 *    If it contains one element,
 *    and the element is not an array:   the element itself
 *    Otherwise:                         an Array[Any] containing the elements
 */

type List[+T] = List.List[T]
val Nil: List[Nothing] = List.Nil
val ScalaNil = sNil
val :: = List.::
val +: = List.+:
val :+ = List.:+
val nullList: List[Null] = List(null)
val nullListList: List[List[Null]] = List(List(null))

extension [T](xs: IterableOnce[T])
  def tolist: List[T] = List.fromIterable(xs)
  def ::: (ys: List[T]): List[T] = List.extension_:::[T](ys)(List.fromIterable(xs))

extension [T](xs: List[T])
  def ::: (ys: List[T]): List[T] = List.extension_:::[T](ys)(xs)

object List:

  /** Lists can only be compared with other Lists... */
  given lstEql[T, U] as Eql[List[T], List[U]] = Eql.derived

  /** ... except that locally we can also match with `null` */
  private given lstNullEq[T] as Eql[Null, List[T]] = Eql.derived

  opaque type List[+T] = sList[T]
  val Nil: List[Nothing] = sNil

  private def eq(x: Any, y: Any) = x.asInstanceOf[AnyRef] `eq` y.asInstanceOf[AnyRef]

  def apply[T](): List[T] = Nil

  def apply[T](x0: T): List[T] = x0 :: Nil

  def apply[T](x0: T, x1: T): List[T] = x0 :: x1 :: Nil

  def apply[T](x0: T, x1: T, x2: T): List[T] = x0 :: x1 :: x2 :: Nil

  def apply[T](x0: T, x1: T, x2: T, x3: T): List[T] = x0 :: x1 :: x2 :: x3 :: Nil

  def apply[T](x0: T, x1: T, x2: T, x3: T, x4: T, others: T*): List[T] = x0 :: x1 :: x2 :: x3 :: x4 :: others.toList

  def empty[T]: List[T] = Nil

  def fromArray[T](xs: Array[T], start: Int, end: Int): List[T] =
    xs.slice(start, end).toList

  def fromArray[T](xs: Array[T]): List[T] =
    xs.toList

  def fromIterable[T](it: IterableOnce[T]): List[T] =
    it.iterator.toList

  def fromSeq[T](seq: Seq[T]): List[T] =
    seq.toList

  extension[T](xs: List[T]):
    def length: Int = xs.length
    def size: Int = xs.length
    def isEmpty = xs.isEmpty
    def nonEmpty = xs.nonEmpty
    def foreach(op: T => Unit): Unit = xs.foreach(op)
    def foreachReversed(op: T => Unit): Unit = xs.reverse.foreach(op)
    def iterator: Iterator[T] = xs.iterator
    def copyToArray(target: Array[T], from: Int = 0) = xs.copyToArray(target, from)
    def toSet: collection.immutable.Set[T] = xs.toSet
    def toIterable: Iterable[T] = xs
    def toSeq: Seq[T] = xs
    def toScalaList: sList[T] = xs
    def toIndexedSeq: IndexedSeq[T] = xs.toIndexedSeq
    def filter(p: T => Boolean): List[T] = xs.filter(p)
    def filterNot(p: T => Boolean): List[T] = xs.filterNot(p)
    def withFilter(p: T => Boolean): WithFilter[T] = new WithFilter(xs, p)
    def partition(p: T => Boolean): (List[T], List[T]) = xs.partition(p)
    def span(p: T => Boolean): (List[T], List[T]) = xs.span(p)
    def splitAt(n: Int): (List[T], List[T]) = xs.splitAt(n)
    def exists(p: T => Boolean): Boolean = xs.exists(p)
    def forall(p: T => Boolean): Boolean = xs.forall(p)
    def count(p: T => Boolean): Int = xs.count(p)
    def contains(x: T): Boolean = xs.contains(x)
    def reverse: List[T] = xs.reverse
    def apply(n: Int): T = xs.apply(n)
    def head: T = xs.head
    def last: T = xs.last
    def headOr(alt: T): T = if xs.isEmpty then alt else xs.head
    def headOption: Option[T] = xs.headOption
    def lastOption: Option[T] = xs.lastOption
    def slice(start: Int, end: Int): List[T] = xs.slice(start, end)
    def drop(n: Int): List[T] = xs.drop(n)
    def tail: List[T] = xs.tail
    def take(n: Int): List[T] = xs.take(n)
    def dropRight(n: Int): List[T] = xs.dropRight(n)
    def takeRight(n: Int): List[T] = xs.takeRight(n)
    def init: List[T] = xs.init
    def firstIndexOf(x: T, from: Int = 0): Int = xs.indexOf(x, from) match
      case -1 => xs.length
      case idx => idx
    def firstIndexWhere(p: T => Boolean, from: Int = 0): Int = xs.indexWhere(p, from) match
      case -1 => xs.length
      case idx => idx
    def indexOf(x: T): Int = xs.indexOf(x)
    def indexWhere(p: T => Boolean): Int = xs.indexWhere(p)
    def find(p: T => Boolean): Option[T] = xs.find(p)
    def takeWhile(p: T => Boolean): List[T] = xs.takeWhile(p)
    def dropWhile(p: T => Boolean): List[T] = xs.dropWhile(p)
    def reduceLeft(op: (T, T) => T) = xs.reduceLeft(op)
    def reduce(op: (T, T) => T) = xs.reduceLeft(op)
    def ++ (ys: IterableOnce[T]): List[T] = xs ++ ys
    def ++ (ys: List[T]): List[T] = xs ++ ys
    def ++ (ys: Buffer[T]): List[T] = xs ++ ys
    def :+ (x: T): List[T] = xs :+ x
    def ::: (ys: List[T]): List[T] = xs ::: ys
    def updated(idx: Int, x: T): List[T] = xs.updated(idx, x)
    def union(ys: List[T]): List[T] = xs ++ ys.filterNot(xs.contains(_))
    def intersect(ys: List[T]): List[T] = xs.intersect(ys)
    def zipWithIndex: List[(T, Int)] = xs.zipWithIndex
    def indices: Range = xs.indices
    def indicesAsList = List.range(0, xs.length)
    def distinct: List[T] = xs.distinct
    def lengthCompare(n: Int): Int = xs.lengthCompare(n)
    def maxBy(f: T => Int): T = xs.maxBy(f)
    def sortWith(lt: (T, T) => Boolean): List[T] = xs.sortWith(lt)
    def startsWith(ys: List[T]): Boolean = xs.startsWith(ys)
    def endsWith(ys: List[T]): Boolean = xs.endsWith(ys)

  extension [T: math.Ordering](xs: List[T])
    def sorted: List[T] = xs.sorted

  extension [T, U: math.Ordering](xs: List[T])
    def sortBy(f: T => U): List[T] = xs.sortBy(f)

  extension [T: ClassTag](xs: List[T]):
    def toArray: Array[T] = xs.toArray

  extension [T, U](xs: List[T]):

    /** `f` is pulled out, not duplicated */
    def map(f: T => U): List[U] = xs.map(f)

    def mapconserve(f: T => U): List[U] = {
      @tailrec
      def loop(mapped: Buffer[U], unchanged: List[U], pending: List[T]): List[U] =
        if (pending.isEmpty)
          if (mapped eq null) unchanged
          else mapped.toList ++ unchanged
        else {
          val head0 = pending.head
          val head1 = f(head0)

          if (head1.asInstanceOf[AnyRef] eq head0.asInstanceOf[AnyRef])
            loop(mapped, unchanged, pending.tail)
          else {
            val b = if (mapped eq null) new Buffer[U] else mapped
            var xc = unchanged
            while (xc ne pending) {
              b += xc.head
              xc = xc.tail
            }
            b += head1
            val tail0 = pending.tail
            loop(b, tail0.asInstanceOf[List[U]], tail0)
          }
        }
      loop(null, xs.asInstanceOf[List[U]], xs)
    }
    def mapConserve(f: T => U): List[U] = xs.mapconserve(f)
    def flatMap(f: T => List[U]): List[U] = xs.flatMap(f)
    def flatMapIterable(f: T => IterableOnce[U]): List[U] = xs.flatMap(f)
    def collect(pf: PartialFunction[T, U]): List[U] = xs.collect(pf)
    def collectFirst(pf: PartialFunction[T, U]): Option[U] = xs.collectFirst(pf)
    def foldLeft(z: U)(f: (U, T) => U) = xs.foldLeft(z)(f)
    def foldRight(z: U)(f: (T, U) => U) = xs.foldRight(z)(f)
    def zip(ys: IterableOnce[U]): List[(T, U)] = xs.zip(ys)
    def zip(ys: List[U]): List[(T, U)] = xs.zip(ys)
    def zipped(ys: List[U]) = LazyZip2(xs, ys.iterator)
    def zipped(ys: IterableOnce[U]) = LazyZip2(xs, ys)
    def eqLst(ys: List[U]) = xs.eq(ys)
    def neLst(ys: List[U]) = xs.ne(ys)
    def corresponds(ys: List[U])(p: (T, U) => Boolean): Boolean = xs.corresponds(ys)(p)
    def eqElements(ys: List[U]): Boolean = (xs `eqLst` ys) || xs.corresponds(ys)(eq(_, _))
    def groupBy(f: T => U): Map[U, List[T]] = xs.groupBy(f)

  extension [T](x: T)
    def :: (xs: List[T]): List[T] = x :: xs
    def +: (xs: List[T]): List[T] = x +: xs

  extension [T, U](z: T)
    def /: (xs: List[U])(f: (T, U) => T): T = xs.foldLeft(z)(f)

  extension [T, U] (xs: List[(T, U)])
    def toMap: Map[T, U] = xs.toMap
    def unzip: (List[T], List[U]) = xs.unzip

  extension [T](xs: List[List[T]])
    def flatten: List[T] = xs.flatten
    def transpose: List[List[T]] = xs.transpose

  extension [T](xs: List[Iterable[T]])
    def flattenIterable: List[T] = xs.flatten

  extension [T](xs: List[Option[T]])
    def flattenOption: List[T] = xs.flatten

  extension [T](xs: List[Int])
    def min: Int = xs.min
    def max: Int = xs.max
    def sum: Int = xs.sum

  def fill[T](n: Int)(elem: => T): List[T] = sList.fill[T](n)(elem)
  def tabulate[T](n: Int)(elem: Int => T): List[T] = sList.tabulate[T](n)(elem)
  def range(from: Int, until: Int): List[Int] = sList.range(from, until)

  opaque type Buffer[T] >: Null = ListBuffer[T]
  object Buffer:
    def apply[T](): Buffer[T] = new ListBuffer[T]()

  extension [T](buf: Buffer[T])
    def size = buf.size
    def length = buf.length
    def isEmpty = buf.isEmpty
    def nonEmpty = buf.nonEmpty
    def += (x: T): buf.type = buf += x
    def ++= (xs: IterableOnce[T]): buf.type = buf ++= xs
    def ++= (xs: List[T]): buf.type = buf ++= xs
    def ++= (xs: Array[T]): buf.type = buf ++= xs
    def ++= (xs: Buffer[T]): buf.type = buf ++= xs
    def find(p: T => Boolean): Option[T] = buf.find(p)
    def exists(p: T => Boolean): Boolean = buf.exists(p)
    def forall(p: T => Boolean): Boolean = buf.forall(p)
    def contains(x: T): Boolean = buf.contains(x)
    def apply(idx: Int): T = buf.apply(idx)
    def head = buf.head
    def last = buf.last
    def mapInPlace(f: T => T): buf.type = buf.mapInPlace(f)
    def trimEnd(n: Int) = buf.trimEnd(n)
    def tolist: List[T] = buf.toList
    def toSeq: Seq[T] = buf.toSeq
    def remove(n: Int): T =  buf.remove(n)
    def dropInPlace(n: Int): buf.type = buf.dropInPlace(n)
    def foreach(f: T => Unit): Unit = buf.foreach(f)
    def clear() = buf.clear()
    def reduceLeft(f: (T, T) => T): T = buf.reduceLeft(f)
    def iterator: Iterator[T] = buf.iterator

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


  extension [T](xs: List[T])
    def mkString(left: String, sep: String, right: String): String = xs.mkString(left, sep, right)
    def mkString(sep: String): String = xs.mkString("", sep, "")
    def mkString: String = xs.mkString(", ")

  extension (x: Any)
    def debugString: String = x.toString

  def unapplySeq[T](xs: List[T]): UnapplySeqWrapper[T] = new UnapplySeqWrapper(xs)

  final class UnapplySeqWrapper[T](private val xs: List[T]) extends AnyVal:
    def isEmpty: Boolean = xs.isEmpty
    def get: UnapplySeqWrapper[T] = this
    def lengthCompare(len: Int): Int = xs.length - len
    def apply(i: Int): T = xs(i)
    def drop(n: Int): Seq[T] = xs.drop(n)
    def toSeq: Seq[T] = xs.toSeq

  class WithFilter[T](xs: List[T], p: T => Boolean):

    def zipped[U](ys: List[U]): LazyZip2[T, U] = zipped(ys.iterator)
    def zipped[U](ys: IterableOnce[U]): LazyZip2[T, U] =
      LazyZip2(xs, ys).withFilter((x, y) => p(x))

    def map[U](f: T => U): List[U] =
      val buf = Buffer[U]()//(xs.length)
      foreach(x => buf += f(x))
      buf.tolist

    def flatMap[U](f: T => List[U]): List[U] =
      val buf = Buffer[U]()
      foreach(x => buf ++= f(x))
      buf.tolist

    def foreach(f: T => Unit) =
      xs.foreach(x => if p(x) then f(x))

    def withFilter(q: T => Boolean): WithFilter[T] = new WithFilter(xs, x => p(x) && q(x))
  end WithFilter

  class LazyZip2[T, U](xs: List[T], ys: IterableOnce[U]):
    self =>

    def zipped[V](zs: List[V]): LazyZip3[T, U, V] = zipped(zs.iterator)
    def zipped[V](zs: IterableOnce[V]): LazyZip3[T, U, V] = new LazyZip3(xs, ys, zs):
      override def include(x: T, y: U, z: V) = self.include(x, y)

    def map[V](f: (T, U) => V): List[V] =
      val buf = Buffer[V]()//(xs.length)
      foreach((x, y) => buf += f(x, y))
      buf.tolist

    def flatMap[V](f: (T, U) => List[V]): List[V] =
      val buf = Buffer[V]()
      foreach((x, y) => buf ++= f(x, y))
      buf.tolist

    def foreach(f: (T, U) => Unit): Unit =
      val yit = ys.iterator
      xs.foreach { x =>
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
      val buf = Buffer[W]()//(xs.length)
      foreach((x, y, z) => buf += f(x, y, z))
      buf.tolist

    def flatMap[W](f: (T, U, V) => List[W]): List[W] =
      val buf = Buffer[W]()
      foreach((x, y, z) => buf ++= f(x, y, z))
      buf.tolist

    def foreach(f: (T, U, V) => Unit): Unit =
      val yit = ys.iterator
      val zit = zs.iterator
      xs.foreach { x =>
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
end List
