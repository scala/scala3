object Test {
  case class A()
  case class B()

  def simple[A]: given A => A = implicitly[A]

  def foo[A, B]: given A => given B => (A, B) =
    (implicitly[A], implicitly[B])

  def bar[A, B]: given A => given B => (A, B) = { given (a: A) =>
    (implicitly[A], implicitly[B])
  }

  implicit val a: A = A()
  implicit val b: B = B()

  def main(args: Array[String]) = {
    println(foo[A, B])
    println(foo[A, B] given a)
    println(foo given a given b)
    val s: given A => A = simple[A]
    println(s)
    val x0: given A => given B => (A, B) = foo[A, B]
    println(x0)
    val x1: given B => (A, B) = foo[A, B]
    println(x1)

    println(bar[A, B])
    println(bar[A, B] given a)
    println(bar given a given b)
  }
}
