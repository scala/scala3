trait Null extends Any
object Test with
  def notNull(x: Any): x.type & NotNull =
    assert(x != null)
    x.asInstanceOf  // TODO: drop the .asInstanceOf when explicit nulls are implemented

  locally {
    val x: (Int | Null) = ???
    val y = x; val _: Int | Null = y
  }
  locally {
    val x: (Int | Null) & NotNull = ???
    val y = identity(x); val yc: Int = y
    val z = x; val zc: Int = z
  }
  locally {
    val x: Int | Null = ???
    val y = notNull(identity(x)); val yc: Int = y
    val z = notNull(x); val zc: Int = z
  }
  locally {
    val x: Int | Null = ???
    val y = identity(x).$nn; val yc: Int = y
    val z = x.$nn; val zc: Int = z
  }
  class C { type T }
  locally {
    val x: C { type T = Int } = new C { type T = Int }
    val y: x.$nn.T = 33
    val z = y; val zc: Int = z
  }


