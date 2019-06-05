abstract sealed class List[T] extends Enum
object List {
  final class Cons[T](x: T, xs: List[T]) extends List[T] {
    def $ordinal = 0
  }
  object Cons {
    def apply[T](x: T, xs: List[T]): List[T] = new Cons(x, xs)
  }
  final class Nil[T]() extends List[T] {
    def $ordinal = 1
  }
  object Nil {
    def apply[T](): List[T] = new Nil()
  }
}
object Test {
  import List._
  val xs = Cons(1, Cons(2, Cons(3, Nil())))
  def main(args: Array[String]) = println(xs)
}
