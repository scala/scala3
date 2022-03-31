// like neg-custom-args/isInstanceOf/3324g,
// but verifying the fatal type test/unchecked warning
// emitted during Erasure
// by not being trumped by the fatal refcheck warning on C subclass
class Test {
  trait A[+T]
  class B[T] extends A[T]

  def quux[T](a: A[T]): Unit = a match {
    case _: B[T] => // error
  }
}
