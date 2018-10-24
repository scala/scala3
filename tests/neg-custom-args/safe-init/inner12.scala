object A {
  val x = 10

  class Inner  {
    val y = x

    def f(n: Int) = x * n
  }

  println(new Inner)   // ok
}

object B {
  val x = 10

  case class Inner(m: Int)  {
    def f(n: Int) = z * n
  }

  println(Inner(x))             // error

  val z = 10
}