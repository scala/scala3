trait A {
  trait C[+T] {
    protected[this] def f(t: T): Unit = {} // error: covariant type T occurs in contravariant position in type T of value t
  }
  trait D[T] extends C[T] {
    def g(t: T): Unit = { f(t) }
  }
}
