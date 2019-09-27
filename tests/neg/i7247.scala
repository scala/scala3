val x = "foo" match
  case _: (a *: (b: Any)) => ???  // error