def select[A](s: Seq[() => A]) =
  val x = s(0) // error
  val y = s.head // error



