type X
val x: X = ???
extension (x: X) def unapply(arg: Any): Boolean = true
def test =
  ??? match
    case x() =>
