object Test:
  def notNull[A](x: A | Null): x.type & A =
    assert(x != null)
    x.asInstanceOf  // TODO: drop the .asInstanceOf when explicit nulls are implemented

  locally {
    val x: (Int | Null) = ???
    val y = x; val _: Int | Null = y
  }
  locally {
    val x: Int | Null = ???
    val y = notNull(identity(x)); val yc: Int = y
    val z = notNull(x); val zc: Int = z
  }
  class C { type T }
  locally {
    val x: C { type T = Int } = new C { type T = Int }
    val xnn: x.type & C { type T = Int } = notNull(x)
    val y: xnn.T = 33
    val z = y; val zc: Int = z
  }
