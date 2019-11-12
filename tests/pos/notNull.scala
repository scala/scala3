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

