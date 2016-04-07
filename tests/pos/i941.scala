object Test {

  class C[A] {

    def this(y: A) = { this(); foo(y) }

    def foo(x: A): Unit = ()

  }
  class D[A](x: A) {

    var f = x

    def this(y1: A, y2: A) = {
      this(y1)
      f = y2
      val g: A = f
      foo(y2)
    }

    def foo(x: A): Unit = ()

  }
}
