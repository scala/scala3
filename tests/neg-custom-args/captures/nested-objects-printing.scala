object A:
  def f =
    object B:
      val x: Int = B.this // error, was "found: (B: A.B.type)", should be "found: (B: B.type)"
