inline def flatConcat2[A, B](a: A, b: B) =
  b match
    case bs: *:[bh, bt]  => a *: bs

@main def Test =
  val x: (Int, Int, Int) = flatConcat2(1, (2,3))
