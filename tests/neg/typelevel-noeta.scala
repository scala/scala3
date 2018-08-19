
object Test {
  def anyValue[T]: T = ???

  rewrite def test(x: Int) = rewrite x match {
    case _: Byte =>
    case _: Char =>
  }

  rewrite def test2() = rewrite 1 match {
    case _: Byte =>
    case _: Char =>
  }

  erased def test3(x: Int) = x + 1

  def f(g: Int => Int) = g(0)

  f(test3) // OK, so far, normal erased functions can be eta-expanded

  val x: Any = test // error

  test // error

  val x2: Any = test2 // error

  test2 // error
}