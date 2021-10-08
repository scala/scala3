package scala
import reflect.ClassTag

import scala.collection.{LazyZip2, SeqView, Searching, Stepper, StepperShape}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.{ArrayBuilder, Builder}

opaque type IArray[+T] = Array[_ <: T]

/** An immutable array. An `IArray[T]` has the same representation as an `Array[T]`,
 *  but it cannot be updated. Unlike regular arrays, immutable arrays are covariant.
 */
object IArray:

  /** The selection operation on an immutable array.
    *
    *  @param arr the immutable array
    *  @param n   the index of the element to select
    *  @return    the element of the array at the given index
    */
  extension (arr: IArray[Byte]) def apply(n: Int): Byte = arr.asInstanceOf[Array[Byte]].apply(n)
  extension (arr: IArray[Short]) def apply(n: Int): Short = arr.asInstanceOf[Array[Short]].apply(n)
  extension (arr: IArray[Char]) def apply(n: Int): Char = arr.asInstanceOf[Array[Char]].apply(n)
  extension (arr: IArray[Int]) def apply(n: Int): Int = arr.asInstanceOf[Array[Int]].apply(n)
  extension (arr: IArray[Long]) def apply(n: Int): Long = arr.asInstanceOf[Array[Long]].apply(n)
  extension (arr: IArray[Float]) def apply(n: Int): Float = arr.asInstanceOf[Array[Float]].apply(n)
  extension (arr: IArray[Double]) def apply(n: Int): Double = arr.asInstanceOf[Array[Double]].apply(n)
  extension [T <: Object](arr: IArray[T]) def apply (n: Int): T = arr.asInstanceOf[Array[T]].apply(n)
  extension [T](arr: IArray[T]) def apply (n: Int): T = arr.asInstanceOf[Array[T]].apply(n)

  /** The number of elements in an immutable array
    *  @param arr  the immutable array
    */
  extension (arr: IArray[Byte]) def length: Int = arr.asInstanceOf[Array[Byte]].length
  extension (arr: IArray[Short]) def length: Int = arr.asInstanceOf[Array[Short]].length
  extension (arr: IArray[Char]) def length: Int = arr.asInstanceOf[Array[Char]].length
  extension (arr: IArray[Int]) def length: Int = arr.asInstanceOf[Array[Int]].length
  extension (arr: IArray[Long]) def length: Int = arr.asInstanceOf[Array[Long]].length
  extension (arr: IArray[Float]) def length: Int = arr.asInstanceOf[Array[Float]].length
  extension (arr: IArray[Double]) def length: Int = arr.asInstanceOf[Array[Double]].length
  extension (arr: IArray[Object]) def length: Int = arr.asInstanceOf[Array[Object]].length
  extension [T](arr: IArray[T]) def length: Int = arr.asInstanceOf[Array[T]].length


  /** Tests whether this array contains a given value as an element. */
  extension [T](arr: IArray[T]) def contains(elem: T): Boolean =
    genericArrayOps(arr).contains(elem.asInstanceOf)

  /** Copy elements of this array to another array. */
  extension [T](arr: IArray[T]) def copyToArray[U >: T](xs: Array[U]): Int =
    genericArrayOps(arr).copyToArray(xs)

  /** Copy elements of this array to another array. */
  extension [T](arr: IArray[T]) def copyToArray[U >: T](xs: Array[U], start: Int): Int =
    genericArrayOps(arr).copyToArray(xs, start)

  /** Copy elements of this array to another array. */
  extension [T](arr: IArray[T]) def copyToArray[U >: T](xs: Array[U], start: Int, len: Int): Int =
    genericArrayOps(arr).copyToArray(xs, start, len)

  /** Counts the number of elements in this array which satisfy a predicate */
  extension [T](arr: IArray[T]) def count(p: T => Boolean): Int =
    genericArrayOps(arr).count(p)

  /** The rest of the array without its `n` first elements. */
  extension [T](arr: IArray[T]) def drop(n: Int): IArray[T] =
    genericArrayOps(arr).drop(n)

  /** The rest of the array without its `n` last elements. */
  extension [T](arr: IArray[T]) def dropRight(n: Int): IArray[T] =
    genericArrayOps(arr).dropRight(n)

  /** Drops longest prefix of elements that satisfy a predicate. */
  extension [T](arr: IArray[T]) def dropWhile(p: T => Boolean): IArray[T] =
    genericArrayOps(arr).dropWhile(p)

  /** Tests whether a predicate holds for at least one element of this array. */
  extension [T](arr: IArray[T]) def exists(p: T => Boolean): Boolean =
    genericArrayOps(arr).exists(p)

  /** Selects all elements of this array which satisfy a predicate. */
  extension [T](arr: IArray[T]) def filter(p: T => Boolean): IArray[T] =
    genericArrayOps(arr).filter(p)

  /** Selects all elements of this array which do not satisfy a predicate. */
  extension [T](arr: IArray[T]) def filterNot(p: T => Boolean): IArray[T] =
    genericArrayOps(arr).filterNot(p)

  /** Finds the first element of the array satisfying a predicate, if any. */
  extension [T](arr: IArray[T]) def find(p: T => Boolean): Option[T] =
    genericArrayOps(arr).find(p)

  /** Builds a new array by applying a function to all elements of this array
    * and using the elements of the resulting collections. */
  extension [T](arr: IArray[T]) def flatMap[U: ClassTag](f: T => IterableOnce[U]): IArray[U] =
    genericArrayOps(arr).flatMap(f)

  /** Flattens a two-dimensional array by concatenating all its rows
    * into a single array. */
  extension [T](arr: IArray[T]) def flatten[U](using asIterable: T => Iterable[U], ct: ClassTag[U]): IArray[U] =
    genericArrayOps(arr).flatten

  /** Folds the elements of this array using the specified associative binary operator. */
  extension [T](arr: IArray[T]) def fold[U >: T](z: U)(op: (U, U) => U): U =
    genericArrayOps(arr).fold(z)(op)

  /** Applies a binary operator to a start value and all elements of this array,
    * going left to right. */
  extension [T](arr: IArray[T]) def foldLeft[U](z: U)(op: (U, T) => U): U =
    genericArrayOps(arr).foldLeft(z)(op)

  /** Applies a binary operator to all elements of this array and a start value,
    * going right to left. */
  extension [T](arr: IArray[T]) def foldRight[U](z: U)(op: (T, U) => U): U =
    genericArrayOps(arr).foldRight(z)(op)

  /** Tests whether a predicate holds for all elements of this array. */
  extension [T](arr: IArray[T]) def forall(p: T => Boolean): Boolean =
    genericArrayOps(arr).forall(p)

  /** Apply `f` to each element for its side effects. */
  extension [T](arr: IArray[T]) def foreach[U](f: T => U): Unit =
    genericArrayOps(arr).foreach(f)

  /** Selects the first element of this array. */
  extension [T](arr: IArray[T]) def head: T =
    genericArrayOps(arr).head

  /** Optionally selects the first element. */
  extension [T](arr: IArray[T]) def headOption: Option[T] =
    genericArrayOps(arr).headOption

  /** Finds index of first occurrence of some value in this array after or at some start index. */
  extension [T](arr: IArray[T]) def indexOf(elem: T, from: Int = 0): Int =
    // `asInstanceOf` needed because `elem` does not have type `arr.T`
    // We could use `arr.iterator.indexOf(elem, from)` or `arr.indexWhere(_ == elem, from)`
    // but these would incur some overhead.
    genericArrayOps(arr).indexOf(elem.asInstanceOf, from)

  /** Finds index of the first element satisfying some predicate after or at some start index. */
  extension [T](arr: IArray[T]) def indexWhere(p: T => Boolean, from: Int = 0): Int =
    genericArrayOps(arr).indexWhere(p, from)

  /** Produces the range of all indices of this sequence. */
  extension [T](arr: IArray[T]) def indices: Range =
    genericArrayOps(arr).indices

  /** The initial part of the array without its last element. */
  extension [T](arr: IArray[T]) def init: IArray[T] =
    genericArrayOps(arr).init

  /** Tests whether the array is empty. */
  extension [T](arr: IArray[T]) def isEmpty: Boolean =
    genericArrayOps(arr).isEmpty

  /** An iterator yielding the elemenst of this array. */
  extension [T](arr: IArray[T]) def iterator: Iterator[T] =
    genericArrayOps(arr).iterator

  /** Selects the last element. */
  extension [T](arr: IArray[T]) def last: T =
    genericArrayOps(arr).last

  /** Optionally selects the last element. */
  extension [T](arr: IArray[T]) def lastOption: Option[T] =
    genericArrayOps(arr).lastOption

  /** Finds index of last occurrence of some value in this array before or at a given end index. */
  extension [T](arr: IArray[T]) def lastIndexOf(elem: T, end: Int = arr.length - 1): Int =
    // see: same issue in `indexOf`
    genericArrayOps(arr).lastIndexOf(elem.asInstanceOf, end)

  /** Finds index of last element satisfying some predicate before or at given end index. */
  extension [T](arr: IArray[T]) def lastIndexWhere(p: T => Boolean, end: Int = arr.length - 1): Int =
    genericArrayOps(arr).lastIndexWhere(p, end)

  /** Builds a new array by applying a function to all elements of this array. */
  extension [T](arr: IArray[T]) def map[U: ClassTag](f: T => U): IArray[U] =
    genericArrayOps(arr).map(f)

  /** Tests whether the array is not empty. */
  extension [T](arr: IArray[T]) def nonEmpty: Boolean =
    genericArrayOps(arr).nonEmpty

  /** A pair of, first, all elements that satisfy predicate `p` and, second, all elements that do not. */
  extension [T](arr: IArray[T]) def partition(p: T => Boolean): (IArray[T], IArray[T]) =
    genericArrayOps(arr).partition(p)

  /** Returns a new array with the elements in reversed order. */
  extension [T](arr: IArray[T]) def reverse: IArray[T] =
    genericArrayOps(arr).reverse

  /** Computes a prefix scan of the elements of the array. */
  extension [T](arr: IArray[T]) def scan[U >: T: ClassTag](z: U)(op: (U, U) => U): IArray[U] =
    genericArrayOps(arr).scan(z)(op)

  /** Produces an array containing cumulative results of applying the binary
    * operator going left to right. */
  extension [T](arr: IArray[T]) def scanLeft[U: ClassTag](z: U)(op: (U, T) => U): IArray[U] =
    genericArrayOps(arr).scanLeft(z)(op)

  /** Produces an array containing cumulative results of applying the binary
    * operator going right to left. */
  extension [T](arr: IArray[T]) def scanRight[U: ClassTag](z: U)(op: (T, U) => U): IArray[U] =
    genericArrayOps(arr).scanRight(z)(op)

  /** The size of this array. */
  extension [T](arr: IArray[T]) def size: Int =
    arr.length

  /** Selects the interval of elements between the given indices. */
  extension [T](arr: IArray[T]) def slice(from: Int, until: Int): IArray[T] =
    genericArrayOps(arr).slice(from, until)

  /** Sorts this array according to the Ordering which results from transforming
    * an implicitly given Ordering with a transformation function. */
  extension [T](arr: IArray[T]) def sortBy[U](f: T => U)(using math.Ordering[U]): IArray[T] =
    genericArrayOps(arr).sortBy(f)

  /** Sorts this array according to a comparison function. */
  extension [T](arr: IArray[T]) def sortWith(f: (T, T) => Boolean): IArray[T] =
    genericArrayOps(arr).sortWith(f)

  /** Sorts this array according to an Ordering. */
  extension [T](arr: IArray[T]) def sorted(using math.Ordering[T]): IArray[T] =
    genericArrayOps(arr).sorted

  /** Splits this array into a prefix/suffix pair according to a predicate. */
  extension [T](arr: IArray[T]) def span(p: T => Boolean): (IArray[T], IArray[T]) =
    genericArrayOps(arr).span(p)

  /** Splits this array into two at a given position. */
  extension [T](arr: IArray[T]) def splitAt(n: Int): (IArray[T], IArray[T]) =
    genericArrayOps(arr).splitAt(n)

  /** The rest of the array without its first element. */
  extension [T](arr: IArray[T]) def tail: IArray[T] =
    genericArrayOps(arr).tail

  /** An array containing the first `n` elements of this array. */
  extension [T](arr: IArray[T]) def take(n: Int): IArray[T] =
    genericArrayOps(arr).take(n)

  /** An array containing the last `n` elements of this array. */
  extension [T](arr: IArray[T]) def takeRight(n: Int): IArray[T] =
    genericArrayOps(arr).takeRight(n)

  /** Takes longest prefix of elements that satisfy a predicate. */
  extension [T](arr: IArray[T]) def takeWhile(p: T => Boolean): IArray[T] =
    genericArrayOps(arr).takeWhile(p)

  extension [T](arr: IArray[T])
    /** Returns a mutable copy of this immutable array. */
    @deprecated("This method implementation is incorrect and calling it can crash your program, please use `IArray.genericWrapArray(myIArray).toArray` instead.", "3.0.1")
    def toArray: Array[T] =
      arr.clone.asInstanceOf[Array[T]]

  extension [T](arr: IArray[T])
    def ++[U >: T: ClassTag](suffix: IArray[U]): IArray[U] = genericArrayOps(arr) ++ suffix.toSeq
    def ++[U >: T: ClassTag](suffix: IterableOnce[U]): IArray[U] = genericArrayOps(arr) ++ suffix
    def :+ [U >: T: ClassTag](x: U): IArray[U] = genericArrayOps(arr) :+ x
    def :++ [U >: T: ClassTag](suffix: IArray[U]): IArray[U] = genericArrayOps(arr) :++ suffix
    def :++ [U >: T: ClassTag](suffix: IterableOnce[U]): IArray[U] = genericArrayOps(arr) :++ suffix
    def appended[U >: T: ClassTag](x: U): IArray[U] = genericArrayOps(arr).appended(x)
    def appendedAll[U >: T: ClassTag](suffix: IArray[U]): IArray[U] = genericArrayOps(arr).appendedAll(suffix)
    def appendedAll[U >: T: ClassTag](suffix: IterableOnce[U]): IArray[U] = genericArrayOps(arr).appendedAll(suffix)
    def collect[U: ClassTag](pf: PartialFunction[T, U]): IArray[U] = genericArrayOps(arr).collect(pf)
    def collectFirst[U](f: PartialFunction[T, U]): Option[U] = genericArrayOps(arr).collectFirst(f)
    def combinations(n: Int): Iterator[IArray[T]] = genericArrayOps(arr).combinations(n)
    def concat[U >: T: ClassTag](suffix: IArray[U]): IArray[U] = genericArrayOps(arr).concat(suffix)
    def concat[U >: T: ClassTag](suffix: IterableOnce[U]): IArray[U] = genericArrayOps(arr).concat(suffix)
    def diff[U >: T](that: IArray[U]): IArray[T] = genericArrayOps(arr).diff(that.toSeq)
    def diff[U >: T](that: Seq[U]): IArray[T] = genericArrayOps(arr).diff(that)
    def distinct: IArray[T] = genericArrayOps(arr).distinct
    def distinctBy[U](f: T => U): IArray[T] = genericArrayOps(arr).distinctBy(f)
    def startsWith[U >: T](that: IArray[U]): Boolean = genericArrayOps(arr).startsWith(that, 0)
    def startsWith[U >: T](that: IArray[U], offset: Int): Boolean = genericArrayOps(arr).startsWith(that, offset)
    def startsWith[U >: T](that: IterableOnce[U]): Boolean = genericArrayOps(arr).startsWith(that, 0)
    def startsWith[U >: T](that: IterableOnce[U], offset: Int): Boolean = genericArrayOps(arr).startsWith(that, offset)
    def endsWith[U >: T](that: IArray[U]): Boolean = genericArrayOps(arr).endsWith(that)
    def endsWith[U >: T](that: Iterable[U]): Boolean = genericArrayOps(arr).endsWith(that)
    def groupBy[K](f: T => K): Map[K, IArray[T]] = genericArrayOps(arr).groupBy(f)
    def groupMap[K, U: ClassTag](key: T => K)(f: T => U): Map[K, IArray[U]] = genericArrayOps(arr).groupMap(key)(f)
    def grouped(size: Int): Iterator[IArray[T]] = genericArrayOps(arr).grouped(size)
    def inits: Iterator[IArray[T]] = genericArrayOps(arr).inits
    def intersect[U >: T](that: IArray[U]): IArray[T] = genericArrayOps(arr).intersect(that)
    def intersect[U >: T](that: Seq[U]): IArray[T] = genericArrayOps(arr).intersect(that)
    def lazyZip[U](that: IArray[U]): LazyZip2[T, U, IArray[T]] = genericArrayOps(arr).lazyZip[U](that).asInstanceOf[LazyZip2[T, U, IArray[T]]]
    def lazyZip[U](that: Iterable[U]): LazyZip2[T, U, IArray[T]] = genericArrayOps(arr).lazyZip[U](that).asInstanceOf[LazyZip2[T, U, IArray[T]]]
    def lengthCompare(len: Int): Int = genericArrayOps(arr).lengthCompare(len)
    def padTo[U >: T: ClassTag](len: Int, elem: U): IArray[U] = genericArrayOps(arr).padTo(len, elem)
    def partitionMap[T1: ClassTag, T2: ClassTag](f: T => Either[T1, T2]): (IArray[T1], IArray[T2]) = genericArrayOps(arr).partitionMap(f)
    def patch[U >: T: ClassTag](from: Int, other: IterableOnce[U], replaced: Int): IArray[U] = genericArrayOps(arr).patch(from, other, replaced)
    def permutations: Iterator[IArray[T]] = genericArrayOps(arr).permutations
    def prepended[U >: T: ClassTag](x: U): IArray[U] = genericArrayOps(arr).prepended(x)
    def prependedAll[U >: T: ClassTag](prefix: IterableOnce[U]): IArray[U] = genericArrayOps(arr).prependedAll(prefix)
    def reverseIterator: Iterator[T] = genericArrayOps(arr).reverseIterator
    def search[U >: T](elem: U)(using Ordering[U]): Searching.SearchResult = arr.toSeq.search(elem)
    def search[U >: T](elem: U, from: Int, to: Int)(using Ordering[U]): Searching.SearchResult = arr.toSeq.search(elem, from, to)
    def sizeCompare(that: IArray[Any]): Int = arr.toSeq.sizeCompare(that)
    def sizeCompare(that: Iterable[_]): Int = arr.toSeq.sizeCompare(that)
    def sizeCompare(otherSize: Int): Int = genericArrayOps(arr).sizeCompare(otherSize)
    def sliding(size: Int, step: Int = 1): Iterator[IArray[T]] = genericArrayOps(arr).sliding(size, step)
    def stepper[S <: Stepper[_]](using StepperShape[T, S]): S = genericArrayOps(arr).stepper[S]
    def tails: Iterator[IArray[T]] = genericArrayOps(arr).tails
    def tapEach[U](f: (T) => U): IArray[T] =
      arr.toSeq.foreach(f)
      arr
    def transpose[U](implicit asArray: T => IArray[U]): IArray[IArray[U]] =
      genericArrayOps(arr).transpose(using asArray.asInstanceOf[T => Array[U]])
    def unzip[T1, T2](using asPair: T => (T1, T2), ct1: ClassTag[T1], ct2: ClassTag[T2]): (IArray[T1], IArray[T2]) = genericArrayOps(arr).unzip
    def unzip3[T1, T2, T3](using asTriple: T => (T1, T2, T3), ct1: ClassTag[T1], ct2: ClassTag[T2], ct3: ClassTag[T3]): (IArray[T1], IArray[T2], IArray[T3]) = genericArrayOps(arr).unzip3
    def updated[U >: T: ClassTag](index: Int, elem: U): IArray[U] = genericArrayOps(arr).updated(index, elem)
    def view: SeqView[T] = genericArrayOps(arr).view
    def withFilter(p: T => Boolean): WithFilter[T] = new WithFilter(p, arr)
    def zip[U](that: IArray[U]): IArray[(T, U)] = genericArrayOps(arr).zip(that)
    def zip[U](that: IterableOnce[U]): IArray[(T, U)] = genericArrayOps(arr).zip(that)
    def zipAll[T1 >: T, U](that: IArray[U], thisElem: T1, thatElem: U): IArray[(T1, U)] = genericArrayOps(arr).zipAll(that, thisElem, thatElem)
    def zipAll[T1 >: T, U](that: Iterable[U], thisElem: T1, thatElem: U): IArray[(T1, U)] = genericArrayOps(arr).zipAll(that, thisElem, thatElem)
    def zipWithIndex: IArray[(T, Int)] = genericArrayOps(arr).zipWithIndex

  extension [T, U >: T: ClassTag](prefix: IterableOnce[T])
    def ++:(arr: IArray[U]): IArray[U] = genericArrayOps(arr).prependedAll(prefix)

  extension [T, U >: T: ClassTag](prefix: IArray[T])
    def ++:(arr: IArray[U]): IArray[U] = genericArrayOps(arr).prependedAll(prefix)

  extension [T, U >: T: ClassTag](x: T)
    def +:(arr: IArray[U]): IArray[U] = genericArrayOps(arr).prepended(x)

  // For backwards compatibility with code compiled without -Yexplicit-nulls
  private def mapNull[A, B](a: A, f: =>B): B =
    if((a: A|Null) == null) null.asInstanceOf[B] else f

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def genericWrapArray[T](arr: IArray[T]): ArraySeq[T] =
    mapNull(arr, ArraySeq.unsafeWrapArray(arr))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapRefArray[T <: AnyRef](arr: IArray[T]): ArraySeq.ofRef[T] =
    // Since the JVM thinks arrays are covariant, one 0-length Array[AnyRef]
    // is as good as another for all T <: AnyRef.  Instead of creating 100,000,000
    // unique ones by way of this implicit, let's share one.
    mapNull(arr,
      if (arr.length == 0) ArraySeq.empty[AnyRef].asInstanceOf[ArraySeq.ofRef[T]]
      else ArraySeq.ofRef(arr.asInstanceOf[Array[T]])
    )

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapIntArray(arr: IArray[Int]): ArraySeq.ofInt =
    mapNull(arr, new ArraySeq.ofInt(arr.asInstanceOf[Array[Int]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapDoubleIArray(arr: IArray[Double]): ArraySeq.ofDouble =
    mapNull(arr, new ArraySeq.ofDouble(arr.asInstanceOf[Array[Double]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapLongIArray(arr: IArray[Long]): ArraySeq.ofLong =
    mapNull(arr, new ArraySeq.ofLong(arr.asInstanceOf[Array[Long]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapFloatIArray(arr: IArray[Float]): ArraySeq.ofFloat =
    mapNull(arr, new ArraySeq.ofFloat(arr.asInstanceOf[Array[Float]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapCharIArray(arr: IArray[Char]): ArraySeq.ofChar =
    mapNull(arr, new ArraySeq.ofChar(arr.asInstanceOf[Array[Char]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapByteIArray(arr: IArray[Byte]): ArraySeq.ofByte =
    mapNull(arr, new ArraySeq.ofByte(arr.asInstanceOf[Array[Byte]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapShortIArray(arr: IArray[Short]): ArraySeq.ofShort =
    mapNull(arr, new ArraySeq.ofShort(arr.asInstanceOf[Array[Short]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapBooleanIArray(arr: IArray[Boolean]): ArraySeq.ofBoolean =
    mapNull(arr, new ArraySeq.ofBoolean(arr.asInstanceOf[Array[Boolean]]))

  /** Conversion from IArray to immutable.ArraySeq */
  implicit def wrapUnitIArray(arr: IArray[Unit]): ArraySeq.ofUnit =
    mapNull(arr, new ArraySeq.ofUnit(arr.asInstanceOf[Array[Unit]]))

  /** Convert an array into an immutable array without copying, the original array
   *   must _not_ be mutated after this or the guaranteed immutablity of IArray will
   *   be violated.
   */
  def unsafeFromArray[T](s: Array[T]): IArray[T] = s

  /** An immutable array of length 0. */
  def empty[T: ClassTag]: IArray[T] = new Array[T](0)

  /** An immutable boolean array of length 0. */
  def emptyBooleanIArray: IArray[Boolean] = Array.emptyBooleanArray
  /** An immutable byte array of length 0. */
  def emptyByteIArray: IArray[Byte]    = Array.emptyByteArray
  /** An immutable char array of length 0. */
  def emptyCharIArray: IArray[Char]    = Array.emptyCharArray
  /** An immutable double array of length 0. */
  def emptyDoubleIArray: IArray[Double]  = Array.emptyDoubleArray
  /** An immutable float array of length 0. */
  def emptyFloatIArray: IArray[Float]   = Array.emptyFloatArray
  /** An immutable int array of length 0. */
  def emptyIntIArray: IArray[Int]     = Array.emptyIntArray
  /** An immutable long array of length 0. */
  def emptyLongIArray: IArray[Long]    = Array.emptyLongArray
  /** An immutable short array of length 0. */
  def emptyShortIArray: IArray[Short]   = Array.emptyShortArray
  /** An immutable object array of length 0. */
  def emptyObjectIArray: IArray[Object]  = Array.emptyObjectArray

  /** An immutable array with given elements. */
  def apply[T](xs: T*)(using ct: ClassTag[T]): IArray[T] = Array(xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Boolean, xs: Boolean*): IArray[Boolean] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Byte, xs: Byte*): IArray[Byte] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Short, xs: Short*): IArray[Short] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Char, xs: Char*): IArray[Char] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Int, xs: Int*): IArray[Int] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Long, xs: Long*): IArray[Long] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Float, xs: Float*): IArray[Float] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Double, xs: Double*): IArray[Double] = Array(x, xs: _*)
  /** An immutable array with given elements. */
  def apply(x: Unit, xs: Unit*): IArray[Unit] = Array(x, xs: _*)

  /** Build an array from the iterable collection.
   *
   *  {{{
   *  scala> val a = IArray.from(Seq(1, 5))
   *  val a: IArray[Int] = IArray(1, 5)
   *
   *  scala> val b = IArray.from(Range(1, 5))
   *  val b: IArray[Int] = IArray(1, 2, 3, 4)
   *  }}}
   *
   *  @param  it the iterable collection
   *  @return    an array consisting of elements of the iterable collection
   */
  def from[A : ClassTag](it: IterableOnce[A]): IArray[A] =
    unsafeFromArray(Array.from(it))

  def newBuilder[T](using t: ClassTag[T]): Builder[T, IArray[T]] =
    ArrayBuilder.make[T].mapResult(IArray.unsafeFromArray)

  /** Concatenates all arrays into a single immutable array.
   *
   *  @param xss the given immutable arrays
   *  @return   the array created from concatenating `xss`
   */
  def concat[T: ClassTag](xss: IArray[T]*): IArray[T] =
    // `Array.concat` should arguably take in a `Seq[Array[_ <: T]]`,
    // but since it currently takes a `Seq[Array[T]]` we have to perform a cast,
    // knowing tacitly that `concat` is not going to do the wrong thing.
    Array.concat[T](xss.asInstanceOf[Seq[Array[T]]]: _*)

  /** Returns an immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n  the number of elements in the array
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n: Int)(elem: => T): IArray[T] =
    Array.fill(n)(elem)

  /** Returns a two-dimensional immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int)(elem: => T): IArray[IArray[T]] =
    Array.fill(n1, n2)(elem)

  /** Returns a three-dimensional immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): IArray[IArray[IArray[T]]] =
    Array.fill(n1, n2, n3)(elem)

  /** Returns a four-dimensional immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.fill(n1, n2, n3, n4)(elem)

  /** Returns a five-dimensional immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.fill(n1, n2, n3, n4, n5)(elem)

  /** Returns an immutable array containing values of a given function over a range of integer
   *  values starting from 0.
   *
   *  @param  n   The number of elements in the array
   *  @param  f   The function computing element values
   */
  def tabulate[T: ClassTag](n: Int)(f: Int => T): IArray[T] =
    Array.tabulate(n)(f)

  /** Returns a two-dimensional immutable array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): IArray[IArray[T]] =
    Array.tabulate(n1, n2)(f)

  /** Returns a three-dimensional immutable array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): IArray[IArray[IArray[T]]] =
    Array.tabulate(n1, n2, n3)(f)

  /** Returns a four-dimensional immutable array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.tabulate(n1, n2, n3, n4)(f)

  /** Returns a five-dimensional immutable array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   n4  the number of elements in the 4th dimension
   *  @param   n5  the number of elements in the 5th dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.tabulate(n1, n2, n3, n4, n5)(f)

  /** Returns an immutable array containing a sequence of increasing integers in a range.
   *
   *  @param start  the start value of the array
   *  @param end    the end value of the array, exclusive (in other words, this is the first value '''not''' returned)
   *  @return  the immutable array with values in range `start, start + 1, ..., end - 1`
   *  up to, but excluding, `end`.
   */
  def range(start: Int, end: Int): IArray[Int] = Array.range(start, end)

  /** Returns an immutable array containing equally spaced values in some integer interval.
   *
   *  @param start the start value of the array
   *  @param end   the end value of the array, exclusive (in other words, this is the first value '''not''' returned)
   *  @param step  the increment value of the array (may not be zero)
   *  @return      the immutable array with values in `start, start + step, ...` up to, but excluding `end`
   */
  def range(start: Int, end: Int, step: Int): IArray[Int] = Array.range(start, end, step)

  /** Returns an immutable array containing repeated applications of a function to a start value.
   *
   *  @param start the start value of the array
   *  @param len   the number of elements returned by the array
   *  @param f     the function that is repeatedly applied
   *  @return      the immutable array returning `len` values in the sequence `start, f(start), f(f(start)), ...`
   */
  def iterate[T: ClassTag](start: T, len: Int)(f: T => T): IArray[T] = Array.iterate(start, len)(f)

  /** Compare two arrays per element.
   *
   *  A more efficient version of `xs.sameElements(ys)`.
   *
   *  @param xs an array of AnyRef
   *  @param ys an array of AnyRef
   *  @return true if corresponding elements are equal
   */
   def equals(xs: IArray[AnyRef], ys: IArray[AnyRef]): Boolean =
    Array.equals(xs.asInstanceOf[Array[AnyRef]], ys.asInstanceOf[Array[AnyRef]])

  /** Returns a decomposition of the array into a sequence. This supports
   *  a pattern match like `{ case IArray(x,y,z) => println('3 elements')}`.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in a [[scala.Some]], if `x` is a Seq, otherwise `None`
   */
  def unapplySeq[T](x: IArray[T]): Array.UnapplySeqWrapper[_ <: T] =
    Array.unapplySeq(x)

  /** A lazy filtered array. No filtering is applied until one of `foreach`, `map` or `flatMap` is called. */
  class WithFilter[T](p: T => Boolean, xs: IArray[T]):

    /** Apply `f` to each element for its side effects.
      * Note: [U] parameter needed to help scalac's type inference.
      */
    def foreach[U](f: T => U): Unit = {
      val len = xs.length
      var i = 0
      while(i < len) {
        val x = xs(i)
        if(p(x)) f(x)
        i += 1
      }
    }

    /** Builds a new array by applying a function to all elements of this array.
      *
      *  @param f      the function to apply to each element.
      *  @tparam U     the element type of the returned array.
      *  @return       a new array resulting from applying the given function
      *                `f` to each element of this array and collecting the results.
      */
    def map[U: ClassTag](f: T => U): IArray[U] = {
      val b = IArray.newBuilder[U]
      var i = 0
      while (i < xs.length) {
        val x = xs(i)
        if(p(x)) b += f(x)
        i = i + 1
      }
      b.result()
    }

    /** Builds a new array by applying a function to all elements of this array
      * and using the elements of the resulting collections.
      *
      *  @param f      the function to apply to each element.
      *  @tparam U     the element type of the returned array.
      *  @return       a new array resulting from applying the given collection-valued function
      *                `f` to each element of this array and concatenating the results.
      */
    def flatMap[U: ClassTag](f: T => IterableOnce[U]): IArray[U] = {
      val b = IArray.newBuilder[U]
      var i = 0
      while(i < xs.length) {
        val x = xs(i)
        if(p(x)) b ++= f(xs(i))
        i += 1
      }
      b.result()
    }

    def flatMap[BS, U](f: T => BS)(using asIterable: BS => Iterable[U], m: ClassTag[U]): IArray[U] =
      flatMap[U](x => asIterable(f(x)))

    /** Creates a new non-strict filter which combines this filter with the given predicate. */
    def withFilter(q: T => Boolean): WithFilter[T] = new WithFilter[T](a => p(a) && q(a), xs)

  end WithFilter

end IArray
