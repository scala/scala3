
object typelevel {
  erased def erasedValue[T]: T = ???
}

object Test {

  rewrite def test[T] = rewrite typelevel.erasedValue[T] match {  // error
    case b: Byte => b
    case c: Char => "A"
  }

  test[Byte]
  test[Char] // ok
}