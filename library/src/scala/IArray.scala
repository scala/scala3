package scala
import reflect.ClassTag

import scala.collection.immutable

/** An immutable array. An `IArray[T]` has the same representation as an `Array[T]`,
 *  but it cannot be updated. Unlike regular arrays, immutable arrays are covariant.
 */
object opaques:
  opaque type IArray[+T] = Array[_ <: T]

  private[scala] type Sub[A] >: Array[A] <: IArray[A]
  private[scala] type Sup[A] >: IArray[A] <: Array[_ <: A]

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

  /** Returns this array concatenated with the given array. */
  extension [T](arr: IArray[T]) def ++ [U >: T: ClassTag](that: IArray[U]): IArray[U] =
    genericArrayOps(arr) ++ that

  /** Tests whether this array contains a given value as an element. */
  extension [T](arr: IArray[T]) def contains(elem: T): Boolean =
    // `genericArrayOps(arr).contains(elem)` does not work because `elem` does not have type `arr.T`
    // but we can use `exists` instead, which is how `ArrayOps#contains` itself is implemented:
    genericArrayOps(arr).exists(_ == elem)

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
  extension [T](arr: IArray[T]) def flatten[U: ClassTag](using T => Iterable[U]): IArray[U] =
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

  /** Tests whether this array starts with the given array. */
  extension [T](arr: IArray[T]) def startsWith[U >: T](that: IArray[U], offset: Int = 0): Boolean =
    genericArrayOps(arr).startsWith(that)

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

  /** Returns a mutable copy of this immutable array. */
  extension [T](arr: IArray[T]) def toArray: Array[T] =
    arr.clone.asInstanceOf[Array[T]]

  /** Converts an array of pairs into an array of first elements and an array of second elements. */
  extension [U: ClassTag, V: ClassTag](arr: IArray[(U, V)]) def unzip: (IArray[U], IArray[V]) =
    genericArrayOps(arr).unzip

  /** Returns an array formed from this array and another iterable collection
    * by combining corresponding elements in pairs.
    * If one of the two collections is longer than the other, its remaining elements are ignored. */
  extension [T](arr: IArray[T]) def zip[U](that: IArray[U]): IArray[(T, U)] =
    genericArrayOps(arr).zip(that)

  /** Conversion from IArray to immutable.ArraySeq */
  extension [T](arr: IArray[T]) def toSeq: immutable.ArraySeq[T] =
    immutable.ArraySeq.unsafeWrapArray(arr.asInstanceOf[Array[T]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension [T <: AnyRef](arr: IArray[T]) def toSeq: immutable.ArraySeq[T] =
    immutable.ArraySeq.ofRef(arr.asInstanceOf[Array[T]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Int]) def toSeq: immutable.ArraySeq[Int] =
    immutable.ArraySeq.ofInt(arr.asInstanceOf[Array[Int]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Double]) def toSeq: immutable.ArraySeq[Double] =
    immutable.ArraySeq.ofDouble(arr.asInstanceOf[Array[Double]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Long]) def toSeq: immutable.ArraySeq[Long] =
    immutable.ArraySeq.ofLong(arr.asInstanceOf[Array[Long]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Float]) def toSeq: immutable.ArraySeq[Float] =
    immutable.ArraySeq.ofFloat(arr.asInstanceOf[Array[Float]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Char]) def toSeq: immutable.ArraySeq[Char] =
    immutable.ArraySeq.ofChar(arr.asInstanceOf[Array[Char]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Byte]) def toSeq: immutable.ArraySeq[Byte] =
    immutable.ArraySeq.ofByte(arr.asInstanceOf[Array[Byte]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Short]) def toSeq: immutable.ArraySeq[Short] =
    immutable.ArraySeq.ofShort(arr.asInstanceOf[Array[Short]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Boolean]) def toSeq: immutable.ArraySeq[Boolean] =
    immutable.ArraySeq.ofBoolean(arr.asInstanceOf[Array[Boolean]])

  /** Conversion from IArray to immutable.ArraySeq */
  extension (arr: IArray[Unit]) def toSeq: immutable.ArraySeq[Unit] =
    immutable.ArraySeq.ofUnit(arr.asInstanceOf[Array[Unit]])

end opaques

type IArray[+T] = opaques.IArray[T]

object IArray {
  import opaques.Sub
  import opaques.Sup

  // A convenience to avoid having to cast everything by hand
  private given [A]: Conversion[Array[A], IArray[A]] = identity[Sub[A]]

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
  inline def apply[T](inline xs: T*)(using inline ct: ClassTag[T]): IArray[T] = Array(xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Boolean, inline xs: Boolean*): IArray[Boolean] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Byte, inline xs: Byte*): IArray[Byte] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Short, inline xs: Short*): IArray[Short] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Char, inline xs: Char*): IArray[Char] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Int, inline xs: Int*): IArray[Int] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Long, inline xs: Long*): IArray[Long] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Float, inline xs: Float*): IArray[Float] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Double, inline xs: Double*): IArray[Double] = Array(x, xs: _*).asInstanceOf
  /** An immutable array with given elements. */
  inline def apply(inline x: Unit, inline xs: Unit*): IArray[Unit] = Array(x, xs: _*).asInstanceOf

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
    // We cannot avoid a cast here as Array.fill creates inner arrays out of our control:
    Array.fill(n1, n2)(elem).asInstanceOf

  /** Returns a three-dimensional immutable array that contains the results of some element computation a number
   *  of times. Each element is determined by a separate computation.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3nd dimension
   *  @param   elem the element computation
   */
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): IArray[IArray[IArray[T]]] =
    Array.fill(n1, n2, n3)(elem).asInstanceOf

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
    Array.fill(n1, n2, n3, n4)(elem).asInstanceOf

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
    Array.fill(n1, n2, n3, n4, n5)(elem).asInstanceOf

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
    Array.tabulate(n1, n2)(f).asInstanceOf

  /** Returns a three-dimensional immutable array containing values of a given function
   *  over ranges of integer values starting from `0`.
   *
   *  @param   n1  the number of elements in the 1st dimension
   *  @param   n2  the number of elements in the 2nd dimension
   *  @param   n3  the number of elements in the 3rd dimension
   *  @param   f   The function computing element values
   */
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): IArray[IArray[IArray[T]]] =
    Array.tabulate(n1, n2, n3)(f).asInstanceOf

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
    Array.tabulate(n1, n2, n3, n4)(f).asInstanceOf

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
    Array.tabulate(n1, n2, n3, n4, n5)(f).asInstanceOf

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

  /** Returns a decomposition of the array into a sequence. This supports
   *  a pattern match like `{ case IArray(x,y,z) => println('3 elements')}`.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in a [[scala.Some]], if `x` is a Seq, otherwise `None`
   */
   def unapplySeq[T](x: IArray[T]) =
    // The double type ascription is currently needed,
    // for some reason (see: https://scastie.scala-lang.org/sSsmOhKxSKym405MgNRKqQ)
    Array.unapplySeq((x: Sup[T]): Array[_ <: T])
}
