trait T {
  object O
}

object Test0 {
  trait A[T]
  given a[T] as A[T]

  class B[T]
  given b[T] as B[T]
}

class C extends T

object Test {
  val c = new C
  c.O
}
