trait T {
  object O
}

object Test0 {
  trait A[T]
  implied a[T] for A[T]

  class B[T]
  implied b[T] for B[T]
}

class C extends T

object Test {
  val c = new C
  c.O
}
