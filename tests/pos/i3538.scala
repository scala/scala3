
class Inv[T]

class Test {
  implicit class Wrong(test: Test) {
    def right: Any = ???
  }
  implicit class Right(test: Test) {
    def right(node: Any): Any = ???
  }

  def inv[T](x: T): Inv[T] = ???

  (this).right(inv(1))
}
