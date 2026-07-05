class Lst[+T](x: Array[Object]) extends AnyVal
val xs: Lst[Int] = ???

def Test = xs match
  case 2 :: ys => 2  // error
