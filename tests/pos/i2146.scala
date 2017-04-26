object Test {
  case class A()
  case class B()

  def simple[A]: implicit A => A = implicitly[A]

  def foo[A, B]: implicit A => implicit B => (A, B) =
    (implicitly[A], implicitly[B])

  def bar[A, B]: implicit A => implicit B => (A, B) = { implicit a: A =>
    (implicitly[A], implicitly[B])
  }

  implicit val a: A = A()
  implicit val b: B = B()

  def main(args: Array[String]) = {
    println(foo[A, B])
    println(foo[A, B](a))
    println(foo(a)(b))
    val s: implicit A => A = simple[A]
    println(s)
    val x0: implicit A => implicit B => (A, B) = foo[A, B]
    println(x0)
    val x1: implicit B => (A, B) = foo[A, B]
    println(x1)

    println(bar[A, B])
    println(bar[A, B](a))
    println(bar(a)(b))
  }
}
