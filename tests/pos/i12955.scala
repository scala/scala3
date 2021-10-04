def test[A, B](using c: A <:< B) =
  val b: B = ??? : A
