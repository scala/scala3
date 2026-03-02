object ForAll:
  def apply[A1, B](f: A1 => B): Unit = ???
  def apply[A1, A2, B](f: (A1, A2) => B): Unit = ???

@main def Test =
  ForAll: (b1: Boolean, b2: Boolean, b3: Boolean) =>
    ???
