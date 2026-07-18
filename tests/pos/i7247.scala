val x = "foo" match
  case _: (a *: (b: Any)) => ???  // OK since (b: Any) is a named tuple type