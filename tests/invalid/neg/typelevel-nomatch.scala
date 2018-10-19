
object Test {
  def anyValue[T]: T = ???

  inline def test[T] = inline anyValue[T] match {  // error
    case _: Byte =>
    case _: Char =>
  }

  test[String]

  test
}