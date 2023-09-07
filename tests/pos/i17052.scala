def test[F[_]](fAny: F[Any]) =
  { [X] => (fx: F[X]) => { val fx2: F[X] = fx; () } }.apply[Any](fAny)
