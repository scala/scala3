@main def Test =
  "a".isInstanceOf[Nothing] // error
  "a".isInstanceOf[Singleton] // error

  "a" match
  case _: Null => () // error
  case _: Nothing => () // error
  case _: Singleton => () // error
  case _ => ()
