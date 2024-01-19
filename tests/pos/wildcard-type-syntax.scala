//> using options -Werror

def test =
  Seq() match
    case _: List[_] =>
    case _: Seq[?] =>
