@main def Test =
  "a".isInstanceOf[Null] // error
  null.isInstanceOf[Null]
  "a".isInstanceOf[Nothing] // error

  "a" match
  case _: Null => () // error
  case _ => ()

  null match
  case _: Null => ()
  case _ => ()

