enum List[+T] {
  case Cons[T](x: T, xs: List[T]) extends List[T]
  case Nil
}
object Test {
  import List.*
  val xs = Cons(1, Cons(2, Cons(3, Nil)))
  def main(args: Array[String]) = println(xs)
}
