class List[+T] {
  
  def prepend [U >: T] (x: U): List[U] = null
  
}

object Nil extends List[Nothing]


object Inferred {

  def foo[T](x: T): T = x

  val x = foo(1)

  val y = foo("abc")

  def bar[U](xs: List[U]): List[U] = xs

  val n = Nil

  val nn = bar(Nil)

  val ints: List[Int] = Nil prepend 1

  // val a = if (1 == 0) Nil else ints

}