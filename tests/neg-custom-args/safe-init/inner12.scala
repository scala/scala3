object A {
  val x = 10

  class Inner  {
    val y = x

    def f(n: Int) = x * n * y
  }

  println(new Inner)   // ok

  case class M(x: Int)
  println(M(4))   // ok
}

object B {
  val x = 10

  case class Inner(m: Int)  {
    def f(n: Int) = z * n + m
  }

  println(Inner(x))             // error

  val z = 10
}