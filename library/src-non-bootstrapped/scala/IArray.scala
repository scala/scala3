package scala
import reflect.ClassTag

object opaques:
  opaque type IArray[+T] = Array[_ <: T]

  private[scala] type Sub[A] >: Array[A] <: IArray[A]
  private[scala] type Sup[A] >: IArray[A] <: Array[_ <: A]

  given arrayOps: Object with {

    extension (arr: IArray[Byte]) def apply(n: Int): Byte = arr.asInstanceOf[Array[Byte]].apply(n)
    extension (arr: IArray[Short]) def apply(n: Int): Short = arr.asInstanceOf[Array[Short]].apply(n)
    extension (arr: IArray[Char]) def apply(n: Int): Char = arr.asInstanceOf[Array[Char]].apply(n)
    extension (arr: IArray[Int]) def apply(n: Int): Int = arr.asInstanceOf[Array[Int]].apply(n)
    extension (arr: IArray[Long]) def apply(n: Int): Long = arr.asInstanceOf[Array[Long]].apply(n)
    extension (arr: IArray[Float]) def apply(n: Int): Float = arr.asInstanceOf[Array[Float]].apply(n)
    extension (arr: IArray[Double]) def apply(n: Int): Double = arr.asInstanceOf[Array[Double]].apply(n)
    extension [T <: Object](arr: IArray[T]) def apply (n: Int): T = arr.asInstanceOf[Array[T]].apply(n)
    extension [T](arr: IArray[T]) def apply (n: Int): T = arr.asInstanceOf[Array[T]].apply(n)

    extension (arr: IArray[Byte]) def length: Int = arr.asInstanceOf[Array[Byte]].length
    extension (arr: IArray[Short]) def length: Int = arr.asInstanceOf[Array[Short]].length
    extension (arr: IArray[Char]) def length: Int = arr.asInstanceOf[Array[Char]].length
    extension (arr: IArray[Int]) def length: Int = arr.asInstanceOf[Array[Int]].length
    extension (arr: IArray[Long]) def length: Int = arr.asInstanceOf[Array[Long]].length
    extension (arr: IArray[Float]) def length: Int = arr.asInstanceOf[Array[Float]].length
    extension (arr: IArray[Double]) def length: Int = arr.asInstanceOf[Array[Double]].length
    extension (arr: IArray[Object]) def length: Int = arr.asInstanceOf[Array[Object]].length
    extension [T](arr: IArray[T]) def length: Int = arr.asInstanceOf[Array[T]].length

    extension [T, U >: T: ClassTag](arr: IArray[T]) def ++(that: IArray[U]): IArray[U] =
      genericArrayOps(arr) ++ that

    extension [T](arr: IArray[T]) def contains(elem: T): Boolean =
      genericArrayOps(arr).exists(_ == elem)

    extension [T, U >: T](arr: IArray[T]) def copyToArray(xs: Array[U]): Int =
      genericArrayOps(arr).copyToArray(xs)

    extension [T, U >: T](arr: IArray[T]) def copyToArray(xs: Array[U], start: Int): Int =
      genericArrayOps(arr).copyToArray(xs, start)

    extension [T, U >: T](arr: IArray[T]) def copyToArray(xs: Array[U], start: Int, len: Int): Int =
      genericArrayOps(arr).copyToArray(xs, start, len)

    extension [T](arr: IArray[T]) def count(p: T => Boolean): Int =
      genericArrayOps(arr).count(p)

    extension [T](arr: IArray[T]) def drop(n: Int): IArray[T] =
      genericArrayOps(arr).drop(n)

    extension [T](arr: IArray[T]) def dropRight(n: Int): IArray[T] =
      genericArrayOps(arr).dropRight(n)

    extension [T](arr: IArray[T]) def dropWhile(p: T => Boolean): IArray[T] =
      genericArrayOps(arr).dropWhile(p)

    extension [T](arr: IArray[T]) def exists(p: T => Boolean): Boolean =
      genericArrayOps(arr).exists(p)

    extension [T](arr: IArray[T]) def filter(p: T => Boolean): IArray[T] =
      genericArrayOps(arr).filter(p)

    extension [T](arr: IArray[T]) def filterNot(p: T => Boolean): IArray[T] =
      genericArrayOps(arr).filterNot(p)

    extension [T](arr: IArray[T]) def find(p: T => Boolean): Option[T] =
      genericArrayOps(arr).find(p)

    extension [T, U: ClassTag](arr: IArray[T]) def flatMap(f: T => IterableOnce[U]): IArray[U] =
      genericArrayOps(arr).flatMap(f)

    extension [T, U: ClassTag](arr: IArray[T]) def flatten(using T => Iterable[U]): IArray[U] =
      genericArrayOps(arr).flatten

    extension [T, U >: T: ClassTag](arr: IArray[T]) def fold(z: U)(op: (U, U) => U): U =
      genericArrayOps(arr).fold(z)(op)

    extension [T, U: ClassTag](arr: IArray[T]) def foldLeft(z: U)(op: (U, T) => U): U =
      genericArrayOps(arr).foldLeft(z)(op)

    extension [T, U: ClassTag](arr: IArray[T]) def foldRight(z: U)(op: (T, U) => U): U =
      genericArrayOps(arr).foldRight(z)(op)

    extension [T](arr: IArray[T]) def forall(p: T => Boolean): Boolean =
      genericArrayOps(arr).forall(p)

    extension [T, U](arr: IArray[T]) def foreach(f: T => U): Unit =
      genericArrayOps(arr).foreach(f)

    extension [T](arr: IArray[T]) def head: T =
      genericArrayOps(arr).head

    extension [T](arr: IArray[T]) def headOption: Option[T] =
      genericArrayOps(arr).headOption

    extension [T](arr: IArray[T]) def indexOf(elem: T, from: Int = 0): Int =
      // `asInstanceOf` needed because `elem` does not have type `arr.T`
      // We could use `arr.iterator.indexOf(elem, from)` or `arr.indexWhere(_ == elem, from)`
      // but these would incur some overhead.
      genericArrayOps(arr).indexOf(elem.asInstanceOf, from)

    extension [T](arr: IArray[T]) def indexWhere(p: T => Boolean, from: Int = 0): Int =
      genericArrayOps(arr).indexWhere(p, from)

    extension [T](arr: IArray[T]) def indices: Range =
      genericArrayOps(arr).indices

    extension [T](arr: IArray[T]) def init: IArray[T] =
      genericArrayOps(arr).init

    extension [T](arr: IArray[T]) def isEmpty: Boolean =
      genericArrayOps(arr).isEmpty

    extension [T](arr: IArray[T]) def iterator: Iterator[T] =
      genericArrayOps(arr).iterator

    extension [T](arr: IArray[T]) def last: T =
      genericArrayOps(arr).last

    extension [T](arr: IArray[T]) def lastOption: Option[T] =
      genericArrayOps(arr).lastOption

    extension [T](arr: IArray[T]) def lastIndexOf(elem: T, end: Int = arr.length - 1): Int =
      // see: same issue in `indexOf`
      genericArrayOps(arr).lastIndexOf(elem.asInstanceOf, end)

    extension [T](arr: IArray[T]) def lastIndexWhere(p: T => Boolean, end: Int = arr.length - 1): Int =
      genericArrayOps(arr).lastIndexWhere(p, end)

    extension [T, U: ClassTag](arr: IArray[T]) def map(f: T => U): IArray[U] =
      genericArrayOps(arr).map(f)

    extension [T](arr: IArray[T]) def nonEmpty: Boolean =
      genericArrayOps(arr).nonEmpty

    extension [T](arr: IArray[T]) def partition(p: T => Boolean): (IArray[T], IArray[T]) =
      genericArrayOps(arr).partition(p)

    extension [T](arr: IArray[T]) def reverse: IArray[T] =
      genericArrayOps(arr).reverse

    extension [T, U >: T: ClassTag](arr: IArray[T]) def scan(z: U)(op: (U, U) => U): IArray[U] =
      genericArrayOps(arr).scan(z)(op)

    extension [T, U: ClassTag](arr: IArray[T]) def scanLeft(z: U)(op: (U, T) => U): IArray[U] =
      genericArrayOps(arr).scanLeft(z)(op)

    extension [T, U: ClassTag](arr: IArray[T]) def scanRight(z: U)(op: (T, U) => U): IArray[U] =
      genericArrayOps(arr).scanRight(z)(op)

    extension [T](arr: IArray[T]) def size: Int =
      arr.length

    extension [T](arr: IArray[T]) def slice(from: Int, until: Int): IArray[T] =
      genericArrayOps(arr).slice(from, until)

    extension [T, U: ClassTag](arr: IArray[T]) def sortBy(f: T => U)(using math.Ordering[U]): IArray[T] =
      genericArrayOps(arr).sortBy(f)

    extension [T](arr: IArray[T]) def sortWith(f: (T, T) => Boolean): IArray[T] =
      genericArrayOps(arr).sortWith(f)

    extension [T](arr: IArray[T]) def sorted(using math.Ordering[T]): IArray[T] =
      genericArrayOps(arr).sorted

    extension [T](arr: IArray[T]) def span(p: T => Boolean): (IArray[T], IArray[T]) =
      genericArrayOps(arr).span(p)

    extension [T](arr: IArray[T]) def splitAt(n: Int): (IArray[T], IArray[T]) =
      genericArrayOps(arr).splitAt(n)

    extension [T, U >: T: ClassTag](arr: IArray[T]) def startsWith(that: IArray[U], offset: Int = 0): Boolean =
      genericArrayOps(arr).startsWith(that)

    extension [T](arr: IArray[T]) def tail: IArray[T] =
      genericArrayOps(arr).tail

    extension [T](arr: IArray[T]) def take(n: Int): IArray[T] =
      genericArrayOps(arr).take(n)

    extension [T](arr: IArray[T]) def takeRight(n: Int): IArray[T] =
      genericArrayOps(arr).takeRight(n)

    extension [T](arr: IArray[T]) def takeWhile(p: T => Boolean): IArray[T] =
      genericArrayOps(arr).takeWhile(p)

    extension [T](arr: IArray[T]) def toArray: Array[T] =
      arr.clone.asInstanceOf[Array[T]]

    extension [U: ClassTag, V: ClassTag](arr: IArray[(U, V)]) def unzip: (IArray[U], IArray[V]) =
      genericArrayOps(arr).unzip

    extension [T, U: ClassTag](arr: IArray[T]) def zip(that: IArray[U]): IArray[(T, U)] =
      genericArrayOps(arr).zip(that)
  }
end opaques

type IArray[+T] = opaques.IArray[T]

object IArray {
  import opaques.Sub
  import opaques.Sup

  private given [A]: Conversion[Array[A], IArray[A]] = identity[Sub[A]]

  def unsafeFromArray[T](s: Array[T]): IArray[T] = s

  def empty[T: ClassTag]: IArray[T] = new Array[T](0)

  def emptyBooleanIArray: IArray[Boolean] = Array.emptyBooleanArray
  def emptyByteIArray: IArray[Byte]    = Array.emptyByteArray
  def emptyCharIArray: IArray[Char]    = Array.emptyCharArray
  def emptyDoubleIArray: IArray[Double]  = Array.emptyDoubleArray
  def emptyFloatIArray: IArray[Float]   = Array.emptyFloatArray
  def emptyIntIArray: IArray[Int]     = Array.emptyIntArray
  def emptyLongIArray: IArray[Long]    = Array.emptyLongArray
  def emptyShortIArray: IArray[Short]   = Array.emptyShortArray
  def emptyObjectIArray: IArray[Object]  = Array.emptyObjectArray

  inline def apply[T](inline xs: T*)(using inline ct: ClassTag[T]): IArray[T] = Array(xs: _*).asInstanceOf
  inline def apply(inline x: Boolean, inline xs: Boolean*): IArray[Boolean] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Byte, inline xs: Byte*): IArray[Byte] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Short, inline xs: Short*): IArray[Short] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Char, inline xs: Char*): IArray[Char] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Int, inline xs: Int*): IArray[Int] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Long, inline xs: Long*): IArray[Long] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Float, inline xs: Float*): IArray[Float] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Double, inline xs: Double*): IArray[Double] = Array(x, xs: _*).asInstanceOf
  inline def apply(inline x: Unit, inline xs: Unit*): IArray[Unit] = Array(x, xs: _*).asInstanceOf

  def concat[T: ClassTag](xss: IArray[T]*): IArray[T] =
    // `Array.concat` should arguably take in a `Seq[Array[_ <: T]]`,
    // but since it currently takes a `Seq[Array[T]]` we have to perform a cast,
    // knowing tacitly that `concat` is not going to do the wrong thing.
    Array.concat[T](xss.asInstanceOf[Seq[Array[T]]]: _*)

  def fill[T: ClassTag](n: Int)(elem: => T): IArray[T] =
    Array.fill(n)(elem)

  def fill[T: ClassTag](n1: Int, n2: Int)(elem: => T): IArray[IArray[T]] =
    // We cannot avoid a cast here as Array.fill creates inner arrays out of our control:
    Array.fill(n1, n2)(elem).asInstanceOf

  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): IArray[IArray[IArray[T]]] =
    Array.fill(n1, n2, n3)(elem).asInstanceOf

  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.fill(n1, n2, n3, n4)(elem).asInstanceOf

  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.fill(n1, n2, n3, n4, n5)(elem).asInstanceOf

  def tabulate[T: ClassTag](n: Int)(f: Int => T): IArray[T] =
    Array.tabulate(n)(f)

  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): IArray[IArray[T]] =
    Array.tabulate(n1, n2)(f).asInstanceOf

  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): IArray[IArray[IArray[T]]] =
    Array.tabulate(n1, n2, n3)(f).asInstanceOf

  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.tabulate(n1, n2, n3, n4)(f).asInstanceOf

  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.tabulate(n1, n2, n3, n4, n5)(f).asInstanceOf

  def range(start: Int, end: Int): IArray[Int] = Array.range(start, end)

  def range(start: Int, end: Int, step: Int): IArray[Int] = Array.range(start, end, step)

  def iterate[T: ClassTag](start: T, len: Int)(f: T => T): IArray[T] = Array.iterate(start, len)(f)

   def unapplySeq[T](x: IArray[T]) =
    Array.unapplySeq((x: Sup[T]): Array[_ <: T])
}
