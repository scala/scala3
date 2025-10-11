
def test(x: Int) =
  x match
  case `-`42 => true // error => expected
  case _ => false // error unindent expected, case found
