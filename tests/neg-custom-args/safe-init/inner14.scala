final class A {
  val x = 10

  class Inner {
    def f(n: Int) = println(new Inner)
    val y = (n: Int) => f(20)
  }

  println(new Inner)

  val y = z  // error
  val z = 10
}