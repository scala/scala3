class Test {
  trait A[+T]
  class B[T] extends A[T]
  class C[T] extends B[Any] with A[T] // error
}
