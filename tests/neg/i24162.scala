
def test(x: Int) =
  x match
  case `-`42 => true // error `=>` expected but `integer constant` found
  case _ => false
