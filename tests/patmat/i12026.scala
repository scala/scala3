def test[A, B](a: A|B)(using reflect.TypeTest[Any, A], reflect.TypeTest[Any, B]) =
  a match {
    case a: A =>
    case b: B =>
  }
