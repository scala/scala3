trait T {
  object O
}

object Test0 {
  trait A[T]
  instance a[T] of A[T]

  class B[T]
  instance b[T] of B[T]
}

class C extends T

object Test {
  val c = new C
  c.O
}
