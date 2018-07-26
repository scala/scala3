
object typelevel {
  erased def erasedValue[T]: T = ???
}

object Test {

  transparent def test[T] = typelevel.erasedValue[T] match {  // error
    case b: Byte => b
    case c: Char => "A"
  }

  test[Byte]
  test[Char] // ok
}