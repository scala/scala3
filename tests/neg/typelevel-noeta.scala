
object Test {
  def anyValue[T]: T = ???

  transparent def test(x: Int) = x match {
    case _: Byte =>
    case _: Char =>
  }

  transparent def test2() = 1 match {
    case _: Byte =>
    case _: Char =>
  }

  val x: Any = test // error

  test // error

  val x2: Any = test2 // error

  test2 // error
}