object Test {
  case class A()
  case class B()

  def simple[A]: A |=> A = implicitly[A]

  def foo[A, B]: A |=> B |=> (A, B) =
    (implicitly[A], implicitly[B])

  def bar[A, B]: A |=> B |=> (A, B) = { a: A |=>
    (implicitly[A], implicitly[B])
  }

  implicit val a: A = A()
  implicit val b: B = B()

  def main(args: Array[String]) = {
    println(foo[A, B])
    println(foo[A, B] with a)
    println(foo with a with b)
    val s: A |=> A = simple[A]
    println(s)
    val x0: A |=> B |=> (A, B) = foo[A, B]
    println(x0)
    val x1: B |=> (A, B) = foo[A, B]
    println(x1)

    println(bar[A, B])
    println(bar[A, B] with a)
    println(bar with a with b)
  }
}
