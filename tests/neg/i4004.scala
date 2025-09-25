@main def Test =

  "a" match
  case _: Null => () // error
  case _: Nothing => () // error
  case _ => ()

  null match
  case _: Null => () // error
  case _ => ()

