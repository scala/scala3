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

  val x1 = new Test().A()      // error: object A does not take parameters
  val x2 = new Test().B()      // error: value B is not a member of Test
  val x3 = new Test().B[Int]() // error: value B is not a member of Test
}


object Test2 {
  class A(s: String = "A") {
    def run = s
  }
  object A {
    def apply() = A("X")  // error: recursive method needs return type
  }
}

object Test3 {
  class A(s: String = "A") {
    def run = s
  }
  object A {
    def apply(): A = A("X")  // error too many arguments
  }
}