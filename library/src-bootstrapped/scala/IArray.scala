package scala
import reflect.ClassTag

/** It would be nice it IArray could be covariant, but unfortunately that's not
 *  possible. The problem is the preculiar behavior of `Array[Any]`.
 *  `Array[Any]` erases to `Object[]`. So, `IArray[Any]` also erases to `Object[]`.
 *  But this means `IArray[Int]`, which erases to `Int[]`, cannot be a subtype of
 *  `IArray[Any]`.
 */
opaque type IArray[T] = Array[T]

object IArray {

  implied arrayOps {
    inline def (arr: IArray[T]) apply[T] (n: Int): T = (arr: Array[T]).apply(n)
    inline def (arr: IArray[T]) length[T] : Int = (arr: Array[T]).length
  }
  def apply[T: ClassTag](xs: T*): IArray[T] = Array(xs: _*)

  def apply(x: Boolean, xs: Boolean*): IArray[Boolean] = Array(x, xs: _*)
  def apply(x: Byte, xs: Byte*): IArray[Byte] = Array(x, xs: _*)
  def apply(x: Short, xs: Short*): IArray[Short] = Array(x, xs: _*)
  def apply(x: Char, xs: Char*): IArray[Char] = Array(x, xs: _*)
  def apply(x: Int, xs: Int*): IArray[Int] = Array(x, xs: _*)
  def apply(x: Long, xs: Long*): IArray[Long] = Array(x, xs: _*)
  def apply(x: Float, xs: Float*): IArray[Float] = Array(x, xs: _*)
  def apply(x: Double, xs: Double*): IArray[Double] = Array(x, xs: _*)
  def apply(x: Unit, xs: Unit*): IArray[Unit] = Array(x, xs: _*)

  def concat[T: ClassTag](xss: IArray[T]*): IArray[T] = Array.concat(xss: _*)

  def fill[T: ClassTag](n: Int)(elem: => T): IArray[T] =
    Array.fill(n)(elem)
  def fill[T: ClassTag](n1: Int, n2: Int)(elem: => T): IArray[IArray[T]] =
    Array.fill(n1, n2)(elem)
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int)(elem: => T): IArray[IArray[IArray[T]]] =
    Array.fill(n1, n2, n3)(elem)
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(elem: => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.fill(n1, n2, n3, n4)(elem)
  def fill[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(elem: => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.fill(n1, n2, n3, n4, n5)(elem)

  def tabulate[T: ClassTag](n: Int)(f: Int => T): IArray[T] =
    Array.tabulate(n)(f)
  def tabulate[T: ClassTag](n1: Int, n2: Int)(f: (Int, Int) => T): IArray[IArray[T]] =
    Array.tabulate(n1, n2)(f)
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int)(f: (Int, Int, Int) => T): IArray[IArray[IArray[T]]] =
    Array.tabulate(n1, n2, n3)(f)
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int)(f: (Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[T]]]] =
    Array.tabulate(n1, n2, n3, n4)(f)
  def tabulate[T: ClassTag](n1: Int, n2: Int, n3: Int, n4: Int, n5: Int)(f: (Int, Int, Int, Int, Int) => T): IArray[IArray[IArray[IArray[IArray[T]]]]] =
    Array.tabulate(n1, n2, n3, n4, n5)(f)

  def range(start: Int, end: Int): IArray[Int] = Array.range(start, end)
  def range(start: Int, end: Int, step: Int): IArray[Int] = Array.range(start, end, step)

  def iterate[T: ClassTag](start: T, len: Int)(f: T => T): IArray[T] = Array.iterate(start, len)(f)

  def unapplySeq[T](x: IArray[T]): Option[IndexedSeq[T]] = Array.unapplySeq(x)
}