def test1 =
  val s: String = ???
  s match
    case _: String =>
    case null => // error: Values of types Null and String cannot be compared

def test2 =
  val s: String | Null = ???
  s match
    case _: String =>
    case null =>
