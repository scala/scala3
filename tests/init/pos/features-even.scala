class Foo {
  val even: Int => Boolean = (n: Int) => n == 0 || odd(n - 1)
  val odd: Int => Boolean = (n: Int) => n == 1 || even(n - 1)
  val flag: Boolean = odd(6)
}
