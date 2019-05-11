trait T {
  object O
}

object Test0 {
  trait A[T]
  implicit a[T] for A[T]

  class B[T]
  implicit b[T] for B[T]
}

class C extends T

object Test {
  val c = new C
  c.O
}
