class Test {
  class A {
    def run = "A"
  }
  object A
  class B[T] {
    def run = "B"
  }
  class C[S, T](x: S, y: T) {
    def run = s"C $x $y"
  }

  val x1 = new Test().A()      // error: not stable
  val x2 = new Test().B()      // error: not stable
  val x3 = new Test().B[Int]() // error: not stable
}

