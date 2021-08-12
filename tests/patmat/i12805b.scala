def test1(a: 1 | 2) = a match
  case 1 => true
  case 2 => false
  case 4 => ??? // unreachable case, was: no warning

def test2(a: 1 | 2) = a match
  case 1 => true
  case 2 => false
  case _ => ??? // unreachable

def test3(a: 1 | 2) = a match
  case 1 => true
  case 2 => false
  case a if a < 0 => ??? // unreachable
