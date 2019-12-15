final class A {
  val x = 10

  final class Inner {
    def f(n: Int) = println(this)
    val y = (n: Int) => f(20)
  }

  new Inner  // check for recursion
}