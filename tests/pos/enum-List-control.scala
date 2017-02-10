abstract sealed class List[T] extends Enum
object List {
  final case class Cons[T](x: T, xs: List[T]) extends List[T] {
    def enumTag = 0
  }
  final case class Nil[T]() extends List[T] {
    def enumTag = 1
  }
}
object Test {
  import List._
  val xs = Cons(1, Cons(2, Cons(3, Nil())))
  def main(args: Array[String]) = println(xs)
}
