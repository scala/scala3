package foo {
  object A {
    inline def f(x: Int) = B.f(x)
  }

  private[foo] object B {
    def f(x: Int) = x * 5
  }
}

class Test {
  val x = foo.A.f(4)
}