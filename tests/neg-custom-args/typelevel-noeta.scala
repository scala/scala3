
object Test {
  def anyValue[T]: T = ???

  inline def test(x: Int) = inline x match {
    case _: Byte =>
    case _: Char =>
  }

  inline def test2() = inline 1 match {
    case _: Byte =>
    case _: Char =>
  }

  erased def test3(x: Int): Int

  def f(g: Int => Int) = g(0)

  f(test3) // OK, so far, normal erased functions can be eta-expanded

  val x: Any = test // error

  test // error

  val x2: Any = test2 // error

  test2 // error
}