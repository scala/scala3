//> using options -Xfatal-warnings -deprecation -feature

def test(t: Tuple)  =
  t match
    case Tuple() =>
    case head *: tail =>
