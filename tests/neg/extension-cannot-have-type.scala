object Test {
  extension on[T] (t: T) {
    def f[U](u: U): T = ??? // error : extension method cannot have type params
  }
}