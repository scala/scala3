
object Test {
  def anyValue[T]: T = ???

  rewrite def test[T] = rewrite anyValue[T] match {  // error
    case _: Byte =>
    case _: Char =>
  }

  test[String]

  test
}