object Test {

  trait Gen[T] {
    def map[U](f: T => U): Gen[U] = ???
  }

  def f[T](implicit g: Gen[T]): Gen[() => T] =
    g map ( () => _ )
}
