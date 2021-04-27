
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