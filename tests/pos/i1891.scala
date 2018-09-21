object Test {
  class CC2[A, B](a: A, b: B)

  type T2[A, B] = CC2[A, B]

  class ArrowAssoc[A](val self: A) {
    inline def f[B](y: B): CC2[A, B] = new CC2(self, y)
  }

  def foo = (new ArrowAssoc(1)).f(2)
}
