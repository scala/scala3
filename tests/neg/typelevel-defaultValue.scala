
object Test {
  def anyValue[T]: T = ???

  transparent def test[T] = anyValue[T] match {  // error
    case _: Byte =>
    case _: Char =>
  }

  test[String]
}