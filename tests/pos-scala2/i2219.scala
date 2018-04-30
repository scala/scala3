object Test {
  type Inv[T[_]] = T[_]

  class Hi[T[_]](x: Inv[T]) {
    def foo[T[_]](value: Inv[T] = x) = {}
  }
}
