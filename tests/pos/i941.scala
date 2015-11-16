object Test {

  class C[A] {

    def this(y: A) = { this(); foo(y) }

    def foo(x: A): Unit = ()

  }
}
