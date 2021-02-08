enum List[+T] {
  case Cons[T](x: T, xs: List[T]) // error: missing extends
  case Nil extends List[Nothing]
}
object Test {
  import List.*
  val xs = Cons(1, Cons(2, Cons(3, Nil)))
  def main(args: Array[String]) = println(xs)
}

