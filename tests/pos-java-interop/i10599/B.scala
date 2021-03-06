abstract class Base[M[_], T] extends A[M[T]] {
  override def foo(value: M[T]): Unit = ???
}

class ArrayTest[T] extends Base[Array, T]

class ListTest[T] extends Base[List, T]
